# `elsewhere.el`: Open version-controlled code from Emacs in your web browser

Use `elsewhere.el` to open a file (or a marked region of a file) as a
permalinked webpage in your browser.

## Usage

Open a version-controlled file, (optionally) mark a region in the
file, and execute `M-x elsewhere-open`. You should see a webpage open
in your browser. If you want the URL but you don't want to
automatically open it in your browser, then try `M-x
elsewhere-build-url` instead. With that command, you should see the
URL in your echo area.

## Demo

### `elsewhere-open`

![elsewhere-open](https://raw.githubusercontent.com/wesnel/elsewhere/demo/elsewhere-open.gif)

### `elsewhere-build-url`

![elsewhere-build-url](https://raw.githubusercontent.com/wesnel/elsewhere/demo/elsewhere-build-url.gif)

## Requirements

- Emacs version 28.1 or newer

## Currently-Supported VC Backends

### Git

- GitHub
- GitLab

## Gotchas

### Incorrect VC Revision

Since `elsewhere.el` is built in top of `vc`, it is subject to some of
its quirks. Make sure to run `vc-refresh-state` before
`elsewhere-open` if you suspect that anything has modified the state
of the VC behind the scenes.
