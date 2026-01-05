# annas-archive

## Introduction

`annas-archive` provides rudimentary Emacs integration for [Anna’s Archive](https://annas-archive.org/), the largest existing search engine for shadow libraries.

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

## Configuration

- `annas-archive-use-fast-download-links` (default: `nil`): If non-nil, the package will use the fast download links provided by Anna’s Archive. To use such links, a [membership](https://annas-archive.org/donate) is required.

- `annas-archive-use-eww` (default: `nil`): If non-`nil`, the package will use the `eww` browser to download files in the background. Otherwise, the default browser will be used, and the user will need to download the file manually. If `annas-archive-use-fast-download-links` is non-`nil`, to use this option you must first authenticate by running `M-x annas-archive-authenticate`. Note that if `annas-archive-use-fast-download-links` is `nil`, this option will have no effect, since slow download links are protected by a CAPTCHA which `eww` cannot handle.

- `annas-archive-when-eww-download-fails` (default: `external`): What to do in the event of a failure to download the file with `eww` (when `annas-archive-use-eww` is non-`nil`). If `external`, download the file with the default browser. If `error`, signal an error. Otherwise, fail silently.

- `annas-archive-downloads-dir` (default: `~/Downloads/`): The directory where the downloaded files will be saved. This user option is only relevant when `annas-archive-use-eww` is non-`nil`.

- `annas-archive-included-file-types`: The list of file extensions to include in search results. By default, all supported file extensions are included. (To see the list of supported file types, see the variable `annas-archive-supported-file-types`.)

- `annas-archive-retry-with-all-file-types` (default: `t`): Whether to try the search again with all supported file types when the search restricted to `annas-archive-included-file-types` returns no results.

- `annas-archive-post-download-hook` (default: `nil`): Hook run after downloading a file from Anna’s Archive. The hook is run with the url as its first argument and, when the file was downloaded with `eww`, the destination path of the downloaded file as its second argument.

- `annas-archive-title-column-width` (default: `100`): Width of the title column when displaying search results.
- `annas-archive-type-column-width` (default: `5`): Width of the type column when displaying search results.
- `annas-archive-size-column-width` (default: `8`): Width of the size column when displaying search results.
- `annas-archive-year-column-width` (default: `4`): Width of the year column when displaying search results.
- `annas-archive-language-column-width` (default: `20`): Width of the language column when displaying search results.

Here’s an example from my personal configuration, using elpaca:

```emacs-lisp
(use-package annas-archive
  :ensure (:host github
		 :repo "benthamite/annas-archive")
  :custom
  (annas-archive-included-file-types '("pdf"))
  (annas-archive-use-fast-download-links t)
  (annas-archive-use-eww t)

  :config
  (defun annas-archive-process-download (url &optional file)
	"Process downloaded FILE from URL in Anna's Archive."
	(when-let ((key ebib-extras-attach-file-key))
	  (setq ebib-extras-attach-file-key nil)
	  (if annas-archive-use-eww
	  (ebib-extras-attach-file file key)
	(message "Save the file that opens in your browser (%s) and attach it to the relevant Ebib entry (%s)"
		 url key))))

  :hook
  (annas-archive-post-download-hook . annas-archive-process-download))
```

The function `annas-archive-process-download` makes use of my extensions for `Ebib`, a bibliography manager. It allows me to search the Anna’s Archive database with a default string obtained from the Ebib entry at point, and then, if `annas-archive-use-eww` is set to `t`, attach the downloaded file to that entry—all without having to leave Emacs.

## Usage

`M-x annas-archive-download` followed by the search string (e.g. book title or ISBN). The results list shows fixed-pitch, aligned columns: Title, Type, Size, Year and Language (when available). Column widths are configurable via the corresponding user options.

## Troubleshooting

The package has not been extensively tested. If you encounter any problems, feel free to open an issue.
