# annas-archive

## Introduction

`annas-archive` provides rudimentary integration for [Anna’s Archive](https://annas-archive.org/), the largest existing search engine for shadow libraries.

## Installation

### Manual installation

Clone this repository and add this to your `init.el` file:

```emacs-lisp
(add-to-list 'load-path "path/to/annas-archive")
```

where `"path/to/annas-archive"` is the path to the local repository you just cloned.

### Elpaca

If you use the [elpaca](https://github.com/progfolio/elpaca) package manager, add this your `init.el` file:

```emacs-lisp
(use-package annas-archive
  :ensure (annas-archive
	   :host github
	   :repo "benthamite/annas-archive")
  :demand t)
```

### Straight

If you use the [straight](https://github.com/radian-software/straight.el) package manager, add this your `init.el` file:

```emacs-lisp
(use-package annas-archive
  :straight (annas-archive
	   :host github
	   :repo "benthamite/annas-archive")
  :demand t)
```

## Configuration

- `annas-archive-downloads-dir`: The directory where the downloaded files will be saved. Default is `~/Downloads/`.

- `annas-archive-use-fast-download-links`: If non-nil, the package will use the fast download links provided by Anna’s Archive. To use such links, a [membership](https://annas-archive.org/donate) is required.

## Usage

`M-x annas-archive-download` followed by the search string (e.g. book title or ISBN).

## Troubleshooting

The package has not been extensively tested. If you encounter any problems, feel free to open an issue.
