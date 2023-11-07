[![License: GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![CI](https://github.com/wesnel/elsewhere/workflows/CI/badge.svg)](https://github.com/wesnel/elsewhere/actions?query=workflow%3ACI)
[![Coverage Status](https://coveralls.io/repos/github/wesnel/elsewhere/badge.svg?branch=main)](https://coveralls.io/github/wesnel/elsewhere?branch=main)

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

- Emacs version 29.1 or newer

## Currently-Supported VC Backends

### Git

- [GitHub](https://github.com/)
- [GitLab](https://gitlab.com/)
