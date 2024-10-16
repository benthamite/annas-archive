# annas-archive

## Introduction

`annas-archive` provides rudimentary integration for [Anna’s Archive](https://annas-archive.org/), the largest existing search engine for shadow libraries.

## Installation

Clone this repository to your Emacs load path and add this to your `init.el` file:

```emacs-lisp
(require 'annas-archive)
```

### With `use-pacakge`

If you use the [elpaca](https://github.com/progfolio/elpaca) package manager, add this your `init.el` file:

```emacs-lisp
;; with vc
(use-package annas-archive
  :vc (:url "https://github.com/benthamite/annas-archive"))

;; with elpaca
(use-package annas-archive
  :ensure (:host github :repo "benthamite/annas-archive"))

;; with straight
(use-package annas-archive
  :straight (:host github :repo "benthamite/annas-archive"))

;; with quelpa
(use-package annas-archive
  :quelpa (annas-archive :fetcher github :repo "benthamite/annas-archive"))
```

## Configuration

- `annas-archive-use-fast-download-links`: If non-nil, the package will use the fast download links provided by Anna’s Archive. To use such links, a [membership](https://annas-archive.org/donate) is required.

## Usage

`M-x annas-archive-download` followed by the search string (e.g. book title or ISBN).

## Troubleshooting

The package has not been extensively tested. If you encounter any problems, feel free to open an issue.
