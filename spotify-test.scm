;;; spotify-test.scm --- the player commands and the assistant's tool, offline.
;;;
;;; The player is a Chrome tab. These tests hold the wire, not Chrome: the
;;; browser seams are stubbed, record the ops the commands send, and answer
;;; the way the extension would. These were test/spotify_test.exs.

(require 'spotify)

(domain! 'testing)
(effects! '(write))

(define t--spotify-seams
  '(chrome-call browser-call-sync browser-snapshot chrome-window-resolve! chrome-tab-resolve!
    chrome--tool-allowed?))

;; every op the commands sent, newest first, as (OP ARGS)
(define t--spotify-ops '())
;; the evals' answers, in order; the last one repeats
(define t--spotify-values '())
(define t--spotify-tabs '())

(define t--spotify-player '((id 9 url "https://open.spotify.com/album/x" title "Spotify")))
(define t--spotify-news '((id 3 url "https://news.example.com/" title "News")))

(define (t--spotify-next-value!)
  (let ((v (if (pair? t--spotify-values) (car t--spotify-values) "done")))
    (when (and (pair? t--spotify-values) (pair? (cdr t--spotify-values)))
      (set! t--spotify-values (cdr t--spotify-values)))
    v))

;; the reply the extension would send to OP
(define (t--spotify-reply op args)
  (set! t--spotify-ops (cons (list op args) t--spotify-ops))
  (cond
    ((equal? op "tabs") (list 'ok #t 'tabs t--spotify-tabs))
    ((equal? op "eval") (list 'ok #t 'value (t--spotify-next-value!)))
    ((equal? op "open") (list 'ok #t 'tab 9))
    (else (list 'ok #t))))

(define (t--spotify! tabs values thunk)
  (let ((saved (map (lambda (s) (cons s (symbol-value s))) t--spotify-seams)))
    (unwind-protect
      (lambda ()
        (set! t--spotify-ops '())
        (set! t--spotify-tabs tabs)
        (set! t--spotify-values values)
        (set! chrome-call (lambda (op args k) (k (t--spotify-reply op args))))
        (set! browser-call-sync (lambda (op args . ms) (t--spotify-reply op args)))
        (set! browser-snapshot (lambda (url k) (k "")))
        (set! chrome-window-resolve! (lambda () #f))
        (set! chrome-tab-resolve! (lambda () #f))
        (set! chrome--tool-allowed? (lambda () #t))
        ;; the web player: the desktop app is not this test's to drive
        (with-custom 'spotify-player "web" thunk))
      (lambda ()
        (for-each (lambda (p) (set-symbol-value! (car p) (cdr p))) saved)
        (when (minibuffer-state) (minibuffer-cancel!))))))

;; the ops named OP, oldest first
(define (t--spotify-sent op)
  (reverse (filter (lambda (o) (equal? (car o) op)) t--spotify-ops)))

(define (t--spotify-evals) (map cadr (t--spotify-sent "eval")))
(define (t--spotify-last-eval) (let ((es (t--spotify-evals))) (and (pair? es) (car (reverse es)))))
(define (t--spotify-code args) (or (plist-get args 'code) ""))

(deftest 'spotify-play-pause-presses-the-player-button
  "it finds the player tab and presses play in the page's own world"
  (lambda ()
    (t--spotify! (append t--spotify-news t--spotify-player) '()
      (lambda ()
        (run-command "spotify-play-pause")
        (let ((e (t--spotify-last-eval)))
          (check-true! e "an eval went out")
          (check-equal! (plist-get e 'tab) 9 "to the player tab")
          (check-contains! (t--spotify-code e) "control-button-playpause" "the play button")
          ;; an extension may not evaluate a string in the isolated world
          (check-equal! (plist-get e 'world) "main" "in the main world"))))))

(deftest 'spotify-next-previous-and-now-playing-press-their-controls
  "each command presses its own control"
  (lambda ()
    (for-each
      (lambda (pair)
        (t--spotify! (append t--spotify-news t--spotify-player) '()
          (lambda ()
            (run-command (car pair))
            ;; a control is followed by a read of the now-playing bar
            (let ((hits (filter (lambda (e) (string-contains? (t--spotify-code e) (cadr pair)))
                                (t--spotify-evals))))
              (check-true! (pair? hits) (string-append (car pair) " presses " (cadr pair)))
              (check-equal! (and (pair? hits) (plist-get (car hits) 'tab)) 9
                            (string-append (car pair) " on the player"))))))
      '(("spotify-next" "control-button-skip-forward")
        ("spotify-previous" "control-button-skip-back")
        ("spotify-now-playing" "now-playing-widget")))))

(deftest 'spotify-with-no-player-opens-one
  "with no player tab it opens one rather than doing nothing"
  (lambda ()
    (t--spotify! t--spotify-news '()
      (lambda ()
        (run-command "spotify-play-pause")
        (let ((opens (t--spotify-sent "open")))
          (check-true! (pair? opens) "a tab opens")
          (check-contains! (or (and (pair? opens) (plist-get (cadr (car opens)) 'url)) "")
                           "open.spotify.com" "on the player"))))))

(deftest 'spotify-search-reads-into-the-search-section
  "the query becomes one encoded path segment of the results section"
  (lambda ()
    (t--spotify! t--spotify-player '()
      (lambda ()
        (run-command "spotify-search")
        (minibuffer-input! "miles davis")
        (minibuffer-confirm!)
        (check-true! (string-prefix? "/search/miles%20davis"
                                     (spotify--section-path (site-app-page (site-app-spec 'spotify) 'results)))
                     "the path")
        (check-true! (buffer-exists? "*spotify-results*") "the results buffer")
        (buffer-kill! "*spotify-results*")))))

;;; --- the assistant's tool ---------------------------------------------------
;;; The tool waits for the page, and the answer comes back inside the call,
;;; so the model can say it in the same turn.

(deftest 'spotify-tool-answers-now-playing-in-its-own-turn
  "now-playing reads the track and returns it"
  (lambda ()
    (t--spotify! t--spotify-player '("Windowpane - Opeth")
      (lambda ()
        (let ((answer (llm-tool-call "spotify" (list 'action "now-playing"))))
          (check-contains! (t--spotify-code (t--spotify-last-eval)) "context-item-link" "it reads the track")
          (check-equal! (plist-get (t--spotify-last-eval) 'world) "main" "in the main world")
          (check-contains! (value->string answer) "Windowpane - Opeth" "the answer"))))))

(deftest 'spotify-tool-pause-reads-before-it-presses
  "pause checks the button state in the page"
  (lambda ()
    (t--spotify! t--spotify-player '("paused")
      (lambda ()
        (let ((answer (llm-tool-call "spotify" (list 'action "pause"))))
          (check-contains! (t--spotify-code (t--spotify-last-eval)) "want==='pause'" "it reads first")
          (check-contains! (value->string answer) "paused" "the answer"))))))

(deftest 'spotify-tool-play-searches-then-presses-the-match
  "play with words searches, tries until the results draw, and presses the match"
  (lambda ()
    (t--spotify! t--spotify-player '("searching" "" "Play Blackwater Park")
      (lambda ()
        (let* ((answer (llm-tool-call "spotify" (list 'action "play" 'query "blackwater park opeth")))
               (codes (map t--spotify-code (t--spotify-evals))))
          (check-true! (>= (length codes) 3) "a search and two tries")
          (check-contains! (car codes) "open.spotify.com/search/" "it searches")
          (check-contains! (car codes) "blackwater park opeth" "for the words")
          (check-contains! (cadr codes) "play-button" "then presses play")
          (check-contains! (value->string answer) "playing Play Blackwater Park" "the answer"))))))

(deftest 'spotify-tool-volume-asks-for-a-level
  "volume without a level asks for one rather than guessing"
  (lambda ()
    (t--spotify! t--spotify-player '()
      (lambda ()
        (check-contains! (value->string (llm-tool-call "spotify" (list 'action "volume"))) "0 to 100"
                         "it asks")))))

(deftest 'spotify-volume-commands-move-the-media-element
  "volume up and down step the element both ways"
  (lambda ()
    (t--spotify! (append t--spotify-news t--spotify-player) '()
      (lambda ()
        (run-command "spotify-volume-up")
        (check-contains! (t--spotify-code (t--spotify-last-eval)) "m.volume+(0.1)" "up")
        (run-command "spotify-volume-down")
        (check-contains! (t--spotify-code (t--spotify-last-eval)) "m.volume+(-0.1)" "down")))))
