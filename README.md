# `elsewhere.el`: Open version-controlled code from Emacs in your web browser

Use `elsewhere.el` to open a file (or a marked region of a file) as a
permalinked webpage in your browser.

## Usage

Open a version-controlled file, (optionally) mark a region in the
file, and call `elsewhere-open` interactively by executing `M-x
elsewhere-open`. You should see a webpage open in your browser.

![demo](https://raw.githubusercontent.com/wesnel/elsewhere/main/demo.gif)

## Currently-Supported VC Backends

### Git

#### GitHub

## Gotchas

### Incorrect VC Revision

Since `elsewhere.el` is built in top of `vc`, it is subject to some of
its quirks. Make sure to run `vc-refresh-state` before
`elsewhere-open` if you suspect that anything has modified the state
of the VC behind the scenes.
