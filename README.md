# compos-spotify

Spotify as an app in [compos](https://github.com/compos-dev/compos). The web player is the site: the app reads Home, Liked Songs, your playlists and a search in a hidden tab of your own Chrome, and draws them in the editor. Playing a row drives your own player tab. No API keys, no OAuth.

## Install

In `init.scm`:

```scheme
(use-package 'spotify :repo "compos-dev/compos-spotify")
```

Then `M-x spotify`. `M-x package-freeze` pins the commit in `packages.lock`.

## Layout

- `recipe.scm` - the recipe: requires and the lowest `compos-api-version`
- `spotify.scm` - the app
- `spotify/visualisers/` - the ambient page
- `web/parsers/spotify-*.xsl` - the sheets that read the player's pages
- `test/spotify_test.exs` - the ExUnit test from the compos tree; it runs inside a compos checkout

The history before the split is the history of these files in the compos repo.

Changes: send them upstream from compos with `(package-pr 'spotify "what changed")`.
