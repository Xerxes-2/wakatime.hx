# wakatime.hx

`wakatime.hx` is a Helix Steel plugin that sends WakaTime heartbeats for:

- document open
- document save
- document focus loss
- document changes after a short idle delay

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

- The plugin reports itself as `wakatime.hx/0.1.0`.
- Idle heartbeats are debounced by 2000 ms.
- Untitled buffers are ignored because WakaTime only receives file-backed entities.
