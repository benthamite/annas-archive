# annas-archive

## Introduction

`annas-archive` provides rudimentary Emacs integration for [Anna’s Archive](https://annas-archive.org/), the largest existing search engine for shadow libraries.

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

- `annas-archive-use-eww`: If non-nil, the package will use the `eww` browser to download files. Otherwise, the default browser will be used. If `annas-archive-use-fast-download-links` is non-nil, to use this option you must first authenticate by running `M-x annas-archive-authenticate`. NB: authentication has been working erratically, so if you are unable to authenticate and want to use fast download links, you may have to set this to nil, unfortunately.

- `annas-archive-downloads-dir`: The directory where the downloaded files will be saved. The default value is `~/Downloads/`.

- `annas-archive-included-file-types`: The list of file extensions to include in search results. By default, all supported file extensions are included.

- `annas-archive-retry-with-all-file-types`: Whether to try the search again with all supported file types when the search restricted to `annas-archive-included-file-types` returns no results.

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

The function `annas-archive-process-download` calls some of my extensions for `Ebib`, a bibliography manager. It allows me to search the Annas Archive database with a default string obtained from the Ebib entry at point, and then, when `annas-archive-use-eww` is set to `t`, attach the downloaded file to that entry.

## Usage

`M-x annas-archive-download` followed by the search string (e.g. book title or ISBN).

## Troubleshooting

The package has not been extensively tested. If you encounter any problems, feel free to open an issue.
