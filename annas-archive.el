;;; annas-archive.el --- Rudimentary integration for Anna’s Archive -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/annas-archive
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Rudimentary integration for Anna’s Archive.

;;; Code:

;;;; Variables

;;;;; Anna’s Archive

(defconst annas-archive-home-url
  "https://annas-archive.org/"
  "URL to Anna’s Archive.
This address changes regularly; to find the most recent URL, go to
<https://en.wikipedia.org/wiki/Anna%27s_Archive> and get the link under
‘External links’.")

(defconst annas-archive-auth-url
  (concat annas-archive-home-url "account/")
  "URL to authenticate with Anna’s Archive.")

(defvar annas-archive-callback nil
  "Callback function to run by `annas-archive-download-file'.")

(defconst annars-archive-supported-file-types
  '("pdf" "epub" "fb2" "mobi" "cbr" "djvu" "cbz" "txt" "azw3")
  "List of supported file extensions.")

;;;; User options

(defgroup annas-archive ()
  "Rudimentary integration for Anna’s Archive."
  :group 'emacs)

(defcustom annas-archive-use-fast-download-links nil
  "Whether to use fast download links from Anna’s Archive.
If non-nil, use fast download links available to paying members."
  :type 'boolean
  :group 'annas-archive)

(defcustom annas-archive-use-eww nil
  "Whether to use `eww' for downloading files.
If non-nil, files will be downloaded directly with `eww'. If
`annas-archive-use-fast-download-links' is non nil, you needto authenticate to
be able to download the files with eww; run `annas-archive-authenticate'.

NB: this option has been working erratically, so if you are unable to
authenticate, you may have to set this to nil, unfortunately."
  :type 'boolean
  :group 'annas-archive)

(defcustom annas-archive-downloads-dir
  (expand-file-name "~/Downloads/")
  "Directory where files downloaded from Anna’s Archive are saved.
This user option is only relevant when `annas-archive-use-eww' is non-nil."
  :type 'directory
  :group 'annas-archive)

(defcustom annas-archive-included-file-types
  annars-archive-supported-file-types
  "List of file extensions to include in search results.
By default, all supported file extensions are included."
  :type '(repeat string)
  :group 'annas-archive)

(defcustom annas-archive-retry-with-all-file-types t
  "Whether to retry to search with all supported file types when no results found."
  :type 'boolean
  :group 'annas-archive)

(defcustom annas-archive-post-download-hook nil
  "Hook run after downloading a file from Anna’s Archive.
The hook is run with the file path as its first argument."
  :type 'hook)

;;;; Functions

;;;###autoload
(defun annas-archive-download (&optional string confirm)
  "Search Anna’s Archive for STRING and download the selected item.
If STRING is nil, prompt for a search string. If both STRING and CONFIRM are
non-nil, prompt the user for confirmation to use STRING as the search string."
  (interactive)
  (save-window-excursion
    (let* ((string (cond ((and string confirm)
			  (read-string "Search string: " string))
			 (string string)
			 (t (read-string "Search string: "))))
	   (url (concat annas-archive-home-url "search?q=" (url-encode-url string))))
      (add-hook 'eww-after-render-hook #'annas-archive-select-and-open-url)
      (eww url))))

;;;;; Selection

(defun annas-archive-select-and-open-url ()
  "Get the download URLs from the Anna’s Archive search results buffer."
  (remove-hook 'eww-after-render-hook #'annas-archive-select-and-open-url)
  (unless (annas-archive-collect-results)
    (when (and annas-archive-retry-with-all-file-types
	       (not (equal (sort (copy-sequence annas-archive-included-file-types) #'string<)
			   (sort (copy-sequence annars-archive-supported-file-types) #'string<)))
	       (y-or-n-p "No results found. Try again with all file types? "))
      (unless (annas-archive-collect-results annars-archive-supported-file-types)
	(message "No results found.")))))

(defun annas-archive-collect-results (&optional types)
  "Collect the download URLs from the Anna’s Archive search results buffer.
Only include links whose file types match TYPES if provided. If
TYPES is nil, use `annas-archive-included-file-types'."
  (save-window-excursion
    (let* ((types (or types annas-archive-included-file-types))
	   (regexp (mapconcat (lambda (extension)
				"Generate a regular expression that matches TYPES."
				(concat "\\." extension))
			      types "\\|"))
	   links)
      (dolist (cons (annas-archive-collect-links-in-buffer))
	(when (string-match-p regexp (car cons))
	  (push cons links)))
      (when links
	(let* ((selection (completing-read "Select a link: " links nil t))
	       (url (alist-get selection links nil nil 'string=)))
	  (add-hook 'eww-after-render-hook #'annas-archive-download-file)
	  (eww url))))))

(defun annas-archive-collect-links-in-buffer ()
  "Get an alist of link titles and URLs for all links in the current `eww' buffer."
  (save-excursion
    (goto-char (point-min))
    (let (beg end candidates)
      (setq end
	    (if (get-text-property (point) 'shr-url)
		(point)
	      (text-property-any (point) (point-max) 'shr-url nil)))
      (while (setq beg (text-property-not-all end (point-max) 'shr-url nil))
	(goto-char beg)
	;; Skip newlines which might precede the link text
	(skip-chars-forward "\n")
	(setq beg (point))
	(if (get-text-property (point) 'shr-url)
	    (progn
	      (setq end (next-single-property-change (point) 'shr-url nil (point-max)))
	      ;; Handle when link is at the end of the buffer
	      (unless end
		(setq end (point-max)))
	      (push (cons (buffer-substring-no-properties beg end) (get-text-property beg 'shr-url))
		    candidates))
	  (setq end (next-single-property-change (point) 'shr-url)))
	(goto-char (max end (1+ (point)))))  ;; ensure progress by moving at least one character forward
      (nreverse candidates))))

;;;;; Downloading

(declare-function browse-url-default-browser "browse-url")
(defun annas-archive-download-file ()
  "Proceed to the Anna’s Archive download page."
  (remove-hook 'eww-after-render-hook #'annas-archive-download-file)
  (save-window-excursion
    (let ((buffer (current-buffer)))
      (goto-char (point-min))
      (if (re-search-forward "Our servers are not responding" nil t)
	  (message "Servers are not responding. Please try again later.")
	(let ((speed (if annas-archive-use-fast-download-links "fast" "slow")))
	  (if-let ((url (annas-archive-get-url-in-link (concat speed " partner server"))))
	      (let ((message (format "Found %s download link. Proceeding to download..." speed)))
		(message message)
		(if annas-archive-use-eww
		    (url-retrieve url (annas-archive-eww-download-file-callback url))
		  (browse-url-default-browser url)
		  (run-hook-with-args 'annas-archive-post-download-hook url)))
	    (user-error "No download link found. If using fast download links, make sure you have run `annas-archive-authenticate'"))))
      (kill-buffer buffer))))

(defun annas-archive-get-url-in-link (title)
  "Return the URL of the link whose title is TITLE."
  (let (found-url)
    (save-excursion
      (goto-char (point-min))
      (while (and (not found-url) (not (eobp)))
	(when-let* ((url (get-text-property (point) 'shr-url))
		    (link-title (buffer-substring-no-properties
				 (point)
				 (or (next-single-property-change (point) 'shr-url)
				     (point-max)))))
	  (when (string-match-p (regexp-quote title) link-title)
	    (setq found-url url)))
	(goto-char (or (next-single-property-change (point) 'shr-url)
		       (point-max)))))
    found-url))

(defun annas-archive-eww-download-file-callback (status)
  "Callback function for run after downloading a file with `eww'.
STATUS is the status of the download process. See the `url-retrieve' docstring
for details."
  (if (plist-get status :error)
      (message "Download failed: %s" (plist-get status :error))
    (let* ((extension (file-name-extension (plist-get status :redirect)))
	   (base (make-temp-name "downloaded-from-annas-archive-"))
	   (filename (if extension
			 (file-name-with-extension base extension)
		       base))
	   (path (file-name-concat annas-archive-downloads-dir filename)))
      (write-file path)
      (message "Downloaded file: `%s'" path)
      (run-hook-with-args 'annas-archive-post-download-hook
			  path annas-archive-post-download-hook-extra-args))))

;;;;; Authentication

;;;###autoload
(defun annas-archive-authenticate ()
  "Authenticate with Anna’s Archive."
  (interactive)
  (save-window-excursion
    (add-hook 'eww-after-render-hook #'annas-archive-get-authentication-details)
    (eww annas-archive-auth-url)))

(defun annas-archive-get-authentication-details ()
  "Return user authentication details from Anna’s Archive."
  (remove-hook 'eww-after-render-hook #'annas-archive-get-authentication-details)
  (let (id key)
    (goto-char (point-min))
    (re-search-forward "Account ID: \\(.*\\)" nil t)
    (setq id (match-string 1))
    (re-search-forward "Secret key (don’t share!): show\\(.*\\)" nil t)
    (setq key (match-string 1))
    (if (and id key)
	(message "You are authenticated.\nAccount ID: %s\nSecret key: %s" id key)
      (eww annas-archive-auth-url)
      (message "You don't seem to be authenticated. Please enter your key in the `eww' buffer."))))

(provide 'annas-archive)
;;; annas-archive.el ends here
