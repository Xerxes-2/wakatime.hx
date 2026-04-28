# wakatime.hx

`wakatime.hx` is a Helix Steel plugin that sends WakaTime heartbeats for:

- document open
- document save
- document focus loss
- document changes after a short idle delay (true debounce)
- document switches detected via selection changes (as a focus-gained proxy)

## Requirements

- Helix built with the Steel event system. See [`STEEL.md`](https://github.com/mattwparas/helix/blob/steel-event-system/STEEL.md).
- `wakatime-cli` available on `PATH` and configured with your API key

## Installation

Install the package with Forge:

```sh
forge pkg install --git https://github.com/Xerxes-2/wakatime.hx.git
```

Then load it from your Helix `init.scm`:

```scheme
(require "wakatime/wakatime.scm")
```

The plugin installs itself when required.

## Notes

- The plugin sends `helix/<ver> wakatime-hx/0.1.0` as its `--plugin` string, matching the 2-segment convention used by vscode-wakatime, sublime-wakatime, etc.
- Idle heartbeats use a true 2000 ms debounce and fire after the last edit in a burst.
- Activity heartbeats are throttled to at most once per 2 minutes per file; write events always go through.
- Untitled buffers are ignored because WakaTime only receives file-backed entities.
