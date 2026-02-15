# annas-archive

## Introduction

`annas-archive` provides rudimentary Emacs integration for [Anna's Archive](https://annas-archive.li/), the largest existing search engine for shadow libraries.

![Demo](demo.gif)

## Breaking change

As of February 2026, Anna's Archive changed their download flow so that the "Download" links on item pages are now JavaScript-driven. This broke the previous `eww`-based download mechanism. Programmatic downloads now use the [fast download JSON API](https://annas-archive.li/dyn/api/fast_download.json), which requires a **secret key**.

If you use `annas-archive-use-eww`, you must now also set `annas-archive-secret-key` to your Anna's Archive secret key. To find your key, log into Anna's Archive with your paid membership and visit your [account page](https://annas-archive.li/account/).

## Installation

### Manual

Clone this repository to your Emacs load path and add this to your `init.el` file:

```emacs-lisp
(require 'annas-archive)
```

### With `use-package`

If you use `use-package`, add one of the following snippets to your `init.el` file:

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

## Usage

Run `M-x annas-archive-download` and enter either:

- An ISBN or search string (e.g. title and/or author) (for books)
- A DOI (for papers).

## Configuration

### Download options

- `annas-archive-use-fast-download-links` (default: `nil`): If non-nil, the package will use the fast download links provided by Anna's Archive. To use such links, a [membership](https://annas-archive.li/donate) is required.

- `annas-archive-secret-key` (default: `nil`): Secret key for the Anna's Archive fast download API. Required for programmatic downloads when `annas-archive-use-eww` is non-`nil`. To find your key, log into Anna's Archive with a paid membership and visit the [account page](https://annas-archive.li/account/).

- `annas-archive-use-eww` (default: `nil`): If non-`nil`, the package will download files directly within Emacs using the fast download API. This requires both `annas-archive-use-fast-download-links` and `annas-archive-secret-key` to be set. If `annas-archive-use-fast-download-links` is `nil`, this option will have no effect, since slow download links are protected by a CAPTCHA which `eww` cannot handle.

- `annas-archive-when-eww-download-fails` (default: `external`): What to do in the event of a failure to download the file with `eww` (when `annas-archive-use-eww` is non-`nil`). If `external`, download the file with the default browser. If `error`, signal an error. Otherwise, fail silently.

- `annas-archive-downloads-dir` (default: `~/Downloads/`): The directory where the downloaded files will be saved. This user option is only relevant when `annas-archive-use-eww` is non-`nil`.

- `annas-archive-post-download-hook` (default: `nil`): Hook run after downloading a file from Anna's Archive. The hook is run with the url as its first argument and, when the file was downloaded with `eww`, the destination path of the downloaded file as its second argument.

### Search options

- `annas-archive-included-file-types`: The list of file extensions to include in search results. By default, all supported file extensions are included. (To see the list of supported file types, see the variable `annas-archive-supported-file-types`.)

- `annas-archive-retry-with-all-file-types` (default: `t`): Whether to try the search again with all supported file types when the search restricted to `annas-archive-included-file-types` returns no results.

### Display options

- `annas-archive-title-column-width` (default: `100`): Width of the title column when displaying search results.
- `annas-archive-type-column-width` (default: `5`): Width of the type column when displaying search results.
- `annas-archive-size-column-width` (default: `8`): Width of the size column when displaying search results.
- `annas-archive-year-column-width` (default: `4`): Width of the year column when displaying search results.
- `annas-archive-language-column-width` (default: `20`): Width of the language column when displaying search results.

## Troubleshooting

The package has not been extensively tested. If you encounter any problems, feel free to open an issue.
