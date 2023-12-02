[![License: GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![CI](https://github.com/wesnel/elsewhere/workflows/CI/badge.svg)](https://github.com/wesnel/elsewhere/actions?query=workflow%3ACI)
[![Coverage Status](https://coveralls.io/repos/github/wesnel/elsewhere/badge.svg?branch=main)](https://coveralls.io/github/wesnel/elsewhere?branch=main)

**`elsewhere.el`: Open version-controlled code from Emacs in your web browser**

Use `elsewhere.el` to open a file (or a marked region of a file) as a
permalinked webpage in your browser.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Usage](#usage)
- [Demo](#demo)
    - [`elsewhere-open`](#elsewhere-open)
    - [`elsewhere-build-url`](#elsewhere-build-url)
- [Requirements](#requirements)
- [Currently-Supported VC Back Ends](#currently-supported-vc-back-ends)
    - [Git](#git)
- [How is `elsewhere.el` different from `git-link`?](#how-is-elsewhereel-different-from-git-link)

<!-- markdown-toc end -->

#### Usage

Open a version-controlled file, (optionally) mark a region in the
file, and execute `M-x elsewhere-open`. You should see a webpage open
in your browser. If you want the URL but you don't want to
automatically open it in your browser, then try `M-x
elsewhere-build-url` instead. With that command, you should see the
URL in your echo area.

#### Demo

##### `elsewhere-open`

![elsewhere-open](https://raw.githubusercontent.com/wesnel/elsewhere/demo/elsewhere-open.gif)

##### `elsewhere-build-url`

![elsewhere-build-url](https://raw.githubusercontent.com/wesnel/elsewhere/demo/elsewhere-build-url.gif)

#### Requirements

> [!IMPORTANT]
> Currently, `elsewhere.el` requires Emacs version `29.1` or newer.
> You can check your Emacs version by typing `M-x emacs-version`.

#### Currently-Supported VC Back Ends

##### Git

- [GitHub](https://github.com/)
- [GitLab](https://gitlab.com/)
- [Sourcehut](https://git.sr.ht/)

#### How is `elsewhere.el` different from `git-link`?

The [`git-link`](https://github.com/sshaw/git-link) package is similar
to `elsewhere.el`. There are several minor differences between that
package and `elsewhere.el`, but the primary benefit of `elsewhere.el`
is that it supports multiple version control back ends other than
Git. In fact, since `elsewhere.el` is built on top of the built-in
`vc` package, support could theoretically be added for any back end
listed
[here](https://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control-Systems.html).
