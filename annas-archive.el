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

(require 'cl-lib)

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

(defconst annas-archive-download-url-pattern
  (concat annas-archive-home-url "md5/.*")
  "Regexp pattern for Anna’s Archive download page.")

(defconst annas-archive-supported-file-types
  '("pdf" "epub" "fb2" "mobi" "cbr" "djvu" "cbz" "txt" "azw3")
  "List of supported file extensions.")

(defvar annas-archive-callback nil
  "Callback function to run by `annas-archive-download-file'.")

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
`annas-archive-use-fast-download-links' is non nil, you need to authenticate to
be able to download the files with eww; run `annas-archive-authenticate'. NB:
authentication has been working erratically, so if you are unable to
authenticate and want to use fast download links, you may have to set this to
nil, unfortunately."
  :type 'boolean
  :group 'annas-archive)


(defcustom annas-archive-when-eww-download-fails 'external
  "What to do in the event of a failure to download the file with `eww'.
If `external' (default), download the file with the default browser. If `error',
signal an error. Otherwise, fail silently."
  :type 'boolean
  :group 'annas-archive)

(defcustom annas-archive-downloads-dir
  (expand-file-name "~/Downloads/")
  "Directory where files downloaded from Anna’s Archive are saved.
This user option is only relevant when `annas-archive-use-eww' is non-nil."
  :type 'directory
  :group 'annas-archive)

(defcustom annas-archive-included-file-types
  annas-archive-supported-file-types
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
The hook is run with the url as its first argument and, when the file was
downloaded with `eww', the destination path of the downloaded file as its second
argument."
  :type 'hook)

(defcustom annas-archive-title-column-width 100
  "Width of the title column when displaying search results."
  :type 'integer
  :group 'annas-archive)

;;;; Functions

;;;###autoload
(defun annas-archive-download (&optional string confirm)
  "Search Anna’s Archive for STRING and download the selected item.
If STRING is nil, prompt for a search string. If both STRING and CONFIRM are
non-nil, prompt the user for confirmation to use STRING as the search string."
  (interactive)
  (save-window-excursion
    (let* ((prompt "Search string: ")
	   (string (cond ((and string confirm)
			  (read-string prompt string))
			 (string string)
			 (t (read-string prompt))))
	   (url (concat annas-archive-home-url "search?q=" (url-encode-url string))))
      (add-hook 'eww-after-render-hook #'annas-archive-select-and-open-url)
      (eww url))))

;;;;; Parsing

(defun annas-archive-parse-results ()
  "Parse the current Anna’s Archive results buffer.
Return a list of plists with bibliographic details for each hit.
Each plist has keys :title, :url, :type, :size, :language and :year.
TITLE is taken from the MD5 link whose visible text is not “*”.
TYPE is a lowercase extension like \"pdf\" or \"epub\"."
  (let* ((links (annas-archive-collect-links))  ;; existing helper in your file
	 (url->titles (make-hash-table :test 'equal))
	 (star-urls '()))
    ;; Build (in-order) list of MD5 URLs as they appear via the leading “*” link
    ;; and a map URL -> all visible titles used for that URL.
    (dolist (cons links)
      (let* ((raw-title (car cons))
	     (title (string-trim (if (stringp raw-title) raw-title "")))
	     (url   (cdr cons)))
	(when (annas-archive--md5-url-p url)
	  (puthash url (cons title (gethash url url->titles)) url->titles)
	  (when (and (string= title "*")
		     (not (member url star-urls)))
	    (setq star-urls (append star-urls (list url)))))))
    ;; Extract type and size by scanning the buffer blocks in the same visual order.
    (let ((infos (annas-archive--info-in-order)))
      (cl-mapcar
       (lambda (url info)
	 (let* ((cands (gethash url url->titles))
		(best  (car (sort (cl-remove-if (lambda (s) (string= s "*")) cands)
				  (lambda (a b) (> (length a) (length b))))))
		(type  (plist-get info :type))
		(size  (plist-get info :size))
		(lang  (plist-get info :language))
		(year  (plist-get info :year)))
	   (list :title (or best "*") :url url :type type :size size :language lang :year year)))
       star-urls infos))))

(defun annas-archive-collect-links ()
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

(defun annas-archive--md5-url-p (url)
  "Return non-nil if URL appears to be an Anna’s Archive item (md5) page."
  (and (stringp url)
       (string-match-p "/md5/[0-9a-f]\\{8,\\}" url)))

(defun annas-archive--info-in-order ()
  "Return a list of plists with details in the visual order of the hits.
Each plist has keys :type, :size, :language, :year and :cover."
  (save-excursion
    (goto-char (point-min))
    (let ((regexp "^[ \t]*\\*[ \t]*$")
	  info)
      (while (re-search-forward regexp nil t)
	(let ((block-beg (line-beginning-position))
	      (block-end (save-excursion
			   (if (re-search-forward regexp nil t)
			       (match-beginning 0)
			     (point-max)))))
	  (push (list
		 :type (annas-archive--ext-from-block  block-beg block-end)
		 :size (annas-archive--size-from-block block-beg block-end)
		 :language (annas-archive--language-from-block block-beg block-end)
		 :year (annas-archive--year-from-block block-beg block-end))
		info)))
      (nreverse info))))

(defun annas-archive--ext-from-block (beg end)
  "Return lowercase file extension for the result block between BEG and END.
Tries a filename line ending in .EXT first, then the “· EXT ·” token line."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      ;; 1) Early filename line ending with .EXT (letters only, 2–6)
      (let ((ext nil)
	    (lines-to-check 6))
	(cl-dotimes (_ lines-to-check)
	  ;; Test the current line only.
	  (let ((line (buffer-substring-no-properties
		       (line-beginning-position) (line-end-position))))
	    (when (string-match "\\.\\([[:alpha:]]\\{2,6\\}\\)[ \t]*\\'" line)
	      (setq ext (downcase (match-string 1 line)))
	      (cl-return)))
	  (forward-line 1))
	(unless ext
	  ;; 2) Language/format line: “· EPUB ·”, “· PDF ·”, …
	  (goto-char (point-min))
	  (when (re-search-forward "·[ \t]*\\([A-Z]\\{3,6\\}\\)[ \t]*·" nil t)
	    (setq ext (downcase (match-string 1)))))
	ext))))

(defun annas-archive--size-from-block (beg end)
  "Return human-readable size string found between BEG and END, like “1.2 MB”."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (when (re-search-forward
	     "\\([0-9]+\\(?:\\.[0-9]+\\)?[[:space:]]*[MGK]B\\)" nil t)
	(string-trim (match-string 1))))))

(defun annas-archive--language-from-block (beg end)
  "Return language token(s) for the block between BEG and END.
Examples include “English [en]” or “English [en] · Latin [la]”."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*\\([^·\n]+\\)[ \t]*·[ \t]*[A-Z]\\{3,6\\}[ \t]*·" nil t)
	(string-trim (match-string 1))))))

(defun annas-archive--year-from-block (beg end)
  "Return publication year for the block between BEG and END, as a string."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (when (re-search-forward "·[ \t]*\\([12][0-9]\\{3\\}\\)[ \t]*·" nil t)
	(match-string 1)))))


;;;;; Collection

(defun annas-archive-collect-results (&optional types)
  "Prompt for one result from the current Archive results buffer and visit it.
Only include links whose file types match TYPES (list of lowercase extensions).
If TYPES is nil, use `annas-archive-included-file-types'."
  (interactive)
  (let* ((wanted (mapcar #'downcase (or types annas-archive-included-file-types)))
	 (results (annas-archive-parse-results))
	 (filtered (cl-remove-if-not
		    (lambda (r) (member (plist-get r :type) wanted))
		    results))
	 (title-width annas-archive-title-column-width)
	 (type-width 5)
	 (size-width 8)
	 (year-width 4)
	 (lang-width 20)
	 (cands (mapcar (lambda (r)
			  (let* ((title (annas-archive--truncate
					 (plist-get r :title) title-width))
				 (type  (upcase (or (plist-get r :type) "")))
				 (size  (or (plist-get r :size) ""))
				 (year  (or (plist-get r :year) ""))
				 (lang  (annas-archive--truncate (or (plist-get r :language) "") lang-width)))
			    (let ((disp (format (format "%%s  %%-%ds  %%-%ds  %%-%ds  %%-%ds"
							type-width size-width year-width lang-width)
						(annas-archive--truncate (plist-get r :title) title-width)
						type size year lang)))
			      (cons (propertize disp 'face 'fixed-pitch)
				    (plist-get r :url)))))
			filtered)))
    (if (null cands)
	(user-error "No matching results for types: %s" wanted)
      (let* ((choice (completing-read "Select a link: " (mapcar #'car cands) nil t))
	     (url    (cdr (assoc choice cands))))
	(add-hook 'eww-after-render-hook #'annas-archive-download-file)
	(eww url)))))

(defun annas-archive--truncate (str width)
  "Return STR rendered in exactly WIDTH columns, truncating with \"...\" if needed.
Handles multi-width characters using `truncate-string-to-width' and pads with spaces."
  (let* ((s (truncate-string-to-width (or str "") width nil nil "..."))
	 (w (string-width s)))
    (if (< w width)
	(concat s (make-string (- width w) ?\s))
      s)))

;;;;; Selection

(defun annas-archive-select-and-open-url ()
  "Get the download URLs from the Anna’s Archive search results buffer."
  (remove-hook 'eww-after-render-hook #'annas-archive-select-and-open-url)
  (unless (annas-archive-collect-results)
    (when (and annas-archive-retry-with-all-file-types
	       (not (equal (sort (copy-sequence annas-archive-included-file-types) #'string<)
			   (sort (copy-sequence annas-archive-supported-file-types) #'string<)))
	       (y-or-n-p "No results found. Try again with all file types? "))
      (unless (annas-archive-collect-results annas-archive-supported-file-types)
	(message "No results found.")))))

;;;;; Downloading

(defvar eww-data)
(autoload 'browse-url-default-browser "browse-url")
(defun annas-archive-download-file (&optional interactive)
  "Download the PDF in the current eww buffer page.
If called interactively, or INTERACTIVE is non-nil, display a message indicating
where the file will be downloaded. Otherwise, kill the eww buffer."
  (interactive "p")
  (annas-archive-ensure-download-page)
  (remove-hook 'eww-after-render-hook #'annas-archive-download-file)
  (save-window-excursion
    (let ((buffer (current-buffer)))
      (goto-char (point-min))
      (if (re-search-forward "Our servers are not responding" nil t)
	  (message "Servers are not responding. Please try again later.")
	(let ((speed (if annas-archive-use-fast-download-links "fast" "slow")))
	  (if-let ((url (annas-archive-get-url-in-link (concat speed " partner server"))))
	      (if annas-archive-use-eww
		  (annas-archive-download-file-with-eww url speed)
		(annas-archive-download-file-externally url))
	    (annas-archive-handle-eww-failure (plist-get eww-data :url)))))
      (if interactive
	  (message "File will be downloaded to `%s'" annas-archive-downloads-dir)
	(kill-buffer buffer)))))

;;;###autoload
(defun annas-archive-ensure-download-page ()
  "Ensure that the current `eww' buffer is a download page from Anna’s Archive."
  (if (derived-mode-p 'eww-mode)
      (if-let ((url (plist-get eww-data :url)))
	  (unless (string-match-p annas-archive-download-url-pattern url)
	    (user-error "Not on a download page"))
	(user-error "No URL found"))
    (user-error "Not in an `eww' buffer")))

(defun annas-archive-download-file-with-eww (url speed)
  "Download the file in URL with `eww'.
URL is the URL of the download link, and SPEED is the download speed."
  (url-retrieve url (annas-archive-eww-download-file-callback url))
  (message (format "Found %s download link. Proceeding to download..." speed)))

(defun annas-archive-download-file-externally (url)
  "Download the file in URL with the default browser.
URL is the URL of the download link."
  (browse-url-default-browser url)
  (run-hook-with-args 'annas-archive-post-download-hook url))

(defun annas-archive-get-url-in-link (title)
  "Return the URL of the link whose title is TITLE."
  (let (found-url)
    (save-excursion
      (goto-char (point-min))
      (while (not (or found-url (eobp)))
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

(defun annas-archive-eww-download-file-callback (url)
  "Callback function for run after downloading file in URL with `eww'."
  (lambda (status)
    "STATUS is the status of the download process; see `url-retrieve' for details."
    (if-let ((err (plist-get status :error)))
	(message "Download failed: %s" err)
      (if-let* ((extension (file-name-extension (plist-get status :redirect)))
		(base (make-temp-name "downloaded-from-annas-archive-"))
		(filename (if extension
			      (file-name-with-extension base extension)
			    base))
		(path (file-name-concat annas-archive-downloads-dir filename)))
	  (annas-archive-save-file url path)
	(annas-archive-handle-eww-failure url)))))

(defun annas-archive-save-file (url path)
  "Save the file at URL to PATH."
  (write-file path)
  (message "Downloaded file: `%s'" path)
  (run-hook-with-args 'annas-archive-post-download-hook url path))

(defun annas-archive-handle-eww-failure (url)
  "Take appropriate action when `eww' fails to download file from URL.
Depending on the value of `annas-archive-when-eww-download-fails', download
externally, signal an error, or fail silently."
  (let ((message "Failed to download file with `eww'."))
    (pcase annas-archive-when-eww-download-fails
      ('external
       (annas-archive-download-file-externally url)
       (message (concat message " Downloading with the default browser instead.")))
      ('error (user-error message))
      (_ (message message)))))

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
