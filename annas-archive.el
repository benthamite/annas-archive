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
(require 'json)

;;;; Variables

;;;;; Anna’s Archive

(defconst annas-archive-home-url
  "https://annas-archive.li/"
  "URL to Anna’s Archive.
This address changes regularly; to find the most recent URL, go to
<https://en.wikipedia.org/wiki/Anna%27s_Archive> and get the link under
‘External links’.")

(defconst annas-archive-auth-url
  (concat annas-archive-home-url "account/")
  "URL to authenticate with Anna’s Archive.")

(defconst annas-archive-download-url-pattern
  (concat annas-archive-home-url "\\(?:md5\\|scidb\\)/.*")
  "Regexp pattern for Anna's Archive download page.")

(defconst annas-archive-fast-download-api-path
  "dyn/api/fast_download.json"
  "Path to the fast download JSON API endpoint.")

(defconst annas-archive-supported-file-types
  '("pdf" "epub" "fb2" "mobi" "cbr" "djvu" "cbz" "txt" "azw3")
  "List of supported file extensions.")

;;;;; Regexps

(defconst annas-archive--re-size
  "\\([0-9]+\\(?:\\.[0-9]+\\)?[[:space:]]*[MGK]B\\)"
  "Regexp matching a human-readable size like \"1.2 MB\" in a block.")

(defconst annas-archive--re-language
  "^[ \t]*\\([^·\n]+\\)[ \t]*·[ \t]*[A-Z]\\{3,6\\}[ \t]*·"
  "Regexp matching the language token line in a block.")

(defconst annas-archive--re-year
  "·[ \t]*\\([12][0-9]\\{3\\}\\)[ \t]*·"
  "Regexp matching the publication year token in a block.")

(defconst annas-archive--re-ext-from-filename
  "\\.\\([[:alpha:]]\\{2,6\\}\\)[ \t]*\\'"
  "Regexp matching a filename-ending extension like \".epub\".")

(defconst annas-archive--re-ext-from-token
  "·[ \t]*\\([A-Z]\\{3,6\\}\\)[ \t]*·"
  "Regexp matching an uppercase extension token like \"· EPUB ·\".")

;;;;; DOIs

(defconst annas-archive--doi-regexp
  "\\(10\\.[0-9]\\{4,9\\}\\(/\\)[-._;()/:A-Z0-9]+\\)$"
  "Regular expression that matches a DOI.")

;;;; User options

(defgroup annas-archive ()
  "Rudimentary integration for Anna’s Archive."
  :group 'emacs)

;;;;; Main options

(defcustom annas-archive-use-fast-download-links nil
  "Whether to use fast download links from Anna’s Archive.
If non-nil, use fast download links available to paying members."
  :type 'boolean
  :group 'annas-archive)

(defcustom annas-archive-secret-key nil
  "Secret key for the Anna's Archive fast download API.
Required for programmatic downloads with `annas-archive-use-eww'. To find your
key, log into Anna's Archive with a paid membership and visit the account page,
or run `M-x annas-archive-authenticate'."
  :type '(choice (const :tag "Not set" nil) string)
  :group 'annas-archive)

(defcustom annas-archive-use-eww nil
  "Whether to use `eww' for downloading files.
If non-nil, files will be downloaded directly within Emacs. This requires
`annas-archive-use-fast-download-links' and `annas-archive-secret-key' to be
set. Note that if `annas-archive-use-fast-download-links' is nil, this option
will have no effect, since slow download links are protected by a CAPTCHA which
`eww' cannot handle."
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

;;;;; Column widths

(defcustom annas-archive-title-column-width 100
  "Width of the title column when displaying search results."
  :type 'integer
  :group 'annas-archive)

(defcustom annas-archive-type-column-width 5
  "Width of the type column when displaying search results."
  :type 'integer
  :group 'annas-archive)

(defcustom annas-archive-size-column-width 8
  "Width of the size column when displaying search results."
  :type 'integer
  :group 'annas-archive)

(defcustom annas-archive-year-column-width 4
  "Width of the year column when displaying search results."
  :type 'integer
  :group 'annas-archive)

(defcustom annas-archive-language-column-width 20
  "Width of the language column when displaying search results."
  :type 'integer
  :group 'annas-archive)

;;;; Functions

;;;###autoload
(defun annas-archive-download (&optional string)
  "Search Anna’s Archive for STRING and download the selected item.
STRING can be a descriptive text (such as the book’s title), an ISBN or (for
papers) a DOI.

When called interactively, always prompt for STRING. When called
non-interactively, never prompt; signal an error if STRING is nil or empty."
  (interactive)
  (save-window-excursion
    (let* ((prompt "Search string: ")
	   (string (if (called-interactively-p 'interactive)
		       (read-string prompt)
		     (annas-archive--require-nonempty-string string)))
	   (url (annas-archive--url-for-query string)))
      (add-hook 'eww-after-render-hook
		(if (annas-archive--doi-p string)
		    #'annas-archive-download-file
		  #'annas-archive-select-and-open-url))
      (eww url))))

;;;;; Parsing

(defun annas-archive--doi-p (string)
  "Return non-nil if STRING is a valid DOI.
STRING is the user input, typically a DOI like \"10.1145/1458082.1458150\"."
  (and (stringp string)
       (string-match-p annas-archive--doi-regexp (upcase (string-trim string)))))

(defun annas-archive--require-nonempty-string (string)
  "Return STRING trimmed, or signal an error if it is nil or empty.
STRING is the user input."
  (let ((s (string-trim (or string ""))))
    (if (string-empty-p s)
	(user-error "STRING must be non-empty when called non-interactively")
      s)))

(defun annas-archive--url-for-query (string)
  "Return the Anna’s Archive URL to use for STRING.
If STRING is a DOI, return the SciDB URL. Otherwise, return the search URL."
  (let ((s (string-trim (or string ""))))
    (if (annas-archive--doi-p s)
	(concat annas-archive-home-url "scidb/" s)
      (concat annas-archive-home-url "search?q=" (url-encode-url s)))))

(defun annas-archive-parse-results ()
  "Parse the current Anna’s Archive results buffer.
Return a list of plists with bibliographic details for each hit.
Each plist has keys :title, :url, :type, :size, :language and :year.
TITLE is taken from the MD5 link whose visible text is not \"*\".
TYPE is a lowercase extension like \"pdf\" or \"epub\"."
  (let* ((links (annas-archive-get-links))
         (mappings (annas-archive--build-url-mappings links))
         (url->titles (car mappings))
         (star-urls (cdr mappings)))
    (annas-archive--combine-url-info star-urls url->titles)))

(defun annas-archive-get-links ()
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
	(skip-chars-forward "\n")
	(setq beg (point))
	(if (get-text-property (point) 'shr-url)
	    (progn
	      (setq end (next-single-property-change (point) 'shr-url nil (point-max)))
	      (unless end
		(setq end (point-max)))
	      (push (cons (buffer-substring-no-properties beg end) (get-text-property beg 'shr-url))
		    candidates))
	  (setq end (next-single-property-change (point) 'shr-url)))
	(goto-char (max end (1+ (point)))))
      (nreverse candidates))))

(defun annas-archive--build-url-mappings (links)
  "Build URL mappings from LINKS.
Return a cons cell (URL->TITLES . STAR-URLS) where URL->TITLES is a hash table
mapping URLs to lists of titles, and STAR-URLS is a list of URLs in order."
  (let ((url->titles (make-hash-table :test 'equal))
        (star-urls '()))
    (dolist (cons links)
      (let* ((raw-title (car cons))
             (title (string-trim (if (stringp raw-title) raw-title "")))
             (url   (cdr cons)))
        (when (annas-archive--md5-url-p url)
          (puthash url (cons title (gethash url url->titles)) url->titles)
          (when (and (string= title "*")
                     (not (member url star-urls)))
            (setq star-urls (append star-urls (list url)))))))
    (cons url->titles star-urls)))

(defun annas-archive--combine-url-info (star-urls url->titles)
  "Combine URL information with extracted metadata.
STAR-URLS is a list of URLs and URL->TITLES is a hash table mapping URLs to
titles."
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
     star-urls infos)))

(defun annas-archive--md5-url-p (url)
  "Return non-nil if URL appears to be an Anna’s Archive item (md5) page."
  (and (stringp url)
       (string-match-p "/md5/[0-9a-f]\\{8,\\}" url)))

(defun annas-archive--info-in-order ()
  "Return a list of plists with details in the visual order of the hits.
Each plist has keys :type, :size, :language and :year."
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

;;;;;; Elements

(defun annas-archive--match-in-block (beg end regexp group trim)
  "Return first REGEXP GROUP between BEG and END, optionally trimmed.
BEG and END delimit the search region. REGEXP is the pattern to search.
GROUP is the capturing group number to return. If TRIM is non-nil, trim spaces."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (when (re-search-forward regexp nil t)
	(let ((s (match-string group)))
	  (if trim (string-trim s) s))))))

(defun annas-archive--size-from-block (beg end)
  "Return human-readable size string found between BEG and END, like “1.2 MB”."
  (annas-archive--match-in-block beg end annas-archive--re-size 1 t))

(defun annas-archive--language-from-block (beg end)
  "Return language token(s) for the block between BEG and END.
Examples include “English [en]” or “English [en] · Latin [la]”."
  (annas-archive--match-in-block beg end annas-archive--re-language 1 t))

(defun annas-archive--year-from-block (beg end)
  "Return publication year for the block between BEG and END, as a string."
  (annas-archive--match-in-block beg end annas-archive--re-year 1 nil))

(defun annas-archive--ext-from-block (beg end)
  "Return lowercase file extension for the result block between BEG and END.
Tries a filename line ending in .EXT first, then the “· EXT ·” token line."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((ext nil)
            (lines-to-check 6))
        (cl-dotimes (_ lines-to-check)
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (when (string-match annas-archive--re-ext-from-filename line)
              (setq ext (downcase (match-string 1 line)))
              (cl-return)))
          (forward-line 1))
        (unless ext
          (goto-char (point-min))
          (when (re-search-forward annas-archive--re-ext-from-token nil t)
            (setq ext (downcase (match-string 1)))))
        ext))))

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
	 (cands (annas-archive--format-candidates filtered)))
    (if (null cands)
	(user-error "No matching results for types: %s" wanted)
      (let* ((choice (completing-read "Select a link: " (mapcar #'car cands) nil t))
	     (url    (cdr (assoc choice cands))))
	(add-hook 'eww-after-render-hook #'annas-archive-download-file)
	(eww url)))))

(defun annas-archive--format-candidates (results)
  "Return formatted candidates from RESULTS for completion.
RESULTS is a list of plists with keys `:title', `:url', `:type', `:size',
`:year' and `:language'."
  (mapcar (lambda (r)
	    (let* ((type (upcase (or (plist-get r :type) "")))
		   (size (or (plist-get r :size) ""))
		   (year (or (plist-get r :year) ""))
		   (lang (annas-archive--truncate (or (plist-get r :language) "") annas-archive-language-column-width))
		   (disp (format (format "%%s  %%-%ds  %%%ds  %%-%ds  %%-%ds"
					 annas-archive-type-column-width
					 annas-archive-size-column-width
					 annas-archive-year-column-width
					 annas-archive-language-column-width)
				 (annas-archive--truncate (plist-get r :title) annas-archive-title-column-width)
				 type size year lang)))
	      (cons (propertize disp 'face 'fixed-pitch)
		    (plist-get r :url))))
	  results))

(defun annas-archive--truncate (str width)
  "Return STR rendered in exactly WIDTH columns on a single line.
Collapses internal whitespace, trims ends, and truncates with \"...\" if needed.
Handles multi-width characters using `truncate-string-to-width' and pads with
spaces."
  (let* ((clean (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim (or str ""))))
	 (s (truncate-string-to-width clean width nil nil "..."))
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
(defvar url-request-extra-headers)
(autoload 'browse-url-default-browser "browse-url")
(defun annas-archive-download-file (&optional interactive)
  "Download the file in the current eww buffer page.
If called interactively, or INTERACTIVE is non-nil, display a message indicating
where the file will be downloaded. Otherwise, kill the eww buffer."
  (interactive "p")
  (annas-archive-ensure-download-page)
  (remove-hook 'eww-after-render-hook #'annas-archive-download-file)
  (save-window-excursion
    (let ((buffer (current-buffer))
	  (page-url (plist-get eww-data :url)))
      (goto-char (point-min))
      (if (re-search-forward "Our servers are not responding" nil t)
	  (message "Servers are not responding. Please try again later.")
	(let* ((md5 (annas-archive--md5-from-url page-url))
	       (api-download-url (when (and md5 (annas-archive--use-fast-download-api-p))
				   (annas-archive--fast-download-api md5))))
	  (cond
	   ;; Fast download API returned a direct URL.
	   (api-download-url
	    (annas-archive-download-file-with-eww api-download-url "fast"))
	   ;; Fall back to scraping download links from the page.
	   (t
	    (let ((speed (if annas-archive-use-fast-download-links "fast" "slow")))
	      (if-let ((url (or (annas-archive-get-url-in-link "Download")
				(annas-archive-get-url-in-link (concat speed " partner server")))))
		  (if (and annas-archive-use-eww (string= speed "fast"))
		      (annas-archive-download-file-with-eww url speed)
		    (annas-archive-download-file-externally url)
		    (when (string= speed "slow")
		      (message "Slow download links cannot be opened with `eww'. Downloading with the external browser.")))
		(annas-archive-handle-eww-failure page-url)))))))
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

(defun annas-archive--use-fast-download-api-p ()
  "Return non-nil when the fast download API can be used."
  (and annas-archive-use-fast-download-links
       annas-archive-use-eww
       (stringp annas-archive-secret-key)
       (not (string-empty-p annas-archive-secret-key))))

(defun annas-archive--md5-from-url (url)
  "Extract the MD5 hash from an Anna's Archive URL.
URL is a string like \"https://annas-archive.li/md5/d6e1dc51...\"."
  (when (and (stringp url)
	     (string-match "/md5/\\([0-9a-f]+\\)" url))
    (match-string 1 url)))

(defun annas-archive--fast-download-api (md5)
  "Return a direct download URL for MD5 using the fast download API.
Returns the download URL string, or nil on failure."
  (let* ((api-url (format "%s%s?md5=%s&key=%s&path_index=0&domain_index=0"
			  annas-archive-home-url
			  annas-archive-fast-download-api-path
			  md5 annas-archive-secret-key))
	 (url-request-extra-headers '(("Accept" . "application/json")))
	 (buffer (url-retrieve-synchronously api-url t nil 30)))
    (when buffer
      (unwind-protect
	  (with-current-buffer buffer
	    (goto-char (point-min))
	    (when (re-search-forward "\r?\n\r?\n" nil t)
	      (condition-case nil
		  (let* ((json-data (json-read))
			 (download-url (cdr (assq 'download_url json-data))))
		    (if (and (stringp download-url)
			     (not (string-empty-p download-url)))
			download-url
		      (when-let ((err (cdr (assq 'error json-data))))
			(message "Fast download API error: %s" err))
		      nil))
		(json-error
		 (message "Fast download API returned invalid JSON")
		 nil))))
	(kill-buffer buffer)))))

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
  "Return a callback for saving the file downloaded from URL with `eww'.
URL is the download URL passed to `url-retrieve'."
  (lambda (status)
    "STATUS is the status of the download process; see `url-retrieve' for details."
    (if-let ((err (plist-get status :error)))
	(message "Download failed: %s" err)
      (let* ((redirect (plist-get status :redirect))
	     (extension (or (annas-archive--extension-from-redirect redirect)
			    (annas-archive--extension-from-headers)
			    (annas-archive--extension-from-url url)
			    "pdf")))
	;; Strip HTTP headers from the response buffer.
	(goto-char (point-min))
	(when (re-search-forward "\r?\n\r?\n" nil t)
	  (delete-region (point-min) (point)))
	(if (annas-archive--response-body-html-p)
	    (annas-archive-handle-eww-failure url)
	  (let* ((base (make-temp-name "downloaded-from-annas-archive-"))
		 (filename (file-name-with-extension base extension))
		 (path (file-name-concat annas-archive-downloads-dir filename)))
	    (if (and (stringp path) (not (string-empty-p path)))
		(annas-archive-save-file url path)
	      (annas-archive-handle-eww-failure url))))))))

(defun annas-archive--response-body-html-p ()
  "Return non-nil if the current buffer appears to contain HTML.
This indicates the server returned a challenge page (e.g. DDoS Guard)
rather than the expected file."
  (save-excursion
    (goto-char (point-min))
    (looking-at-p "\\s-*<\\(?:!DOCTYPE\\|[hH][tT][mM][lL]\\)")))

(defun annas-archive--extension-from-redirect (redirect)
  "Return a file extension inferred from REDIRECT.
REDIRECT is the final URL (a string) reported by `url-retrieve'."
  (when (stringp redirect)
    (file-name-extension redirect)))

(defun annas-archive--extension-from-headers ()
  "Return a file extension inferred from the current buffer’s HTTP headers."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^Content-Type:[ \t]*\\([^;\n]+\\)" nil t)
      (pcase (downcase (string-trim (match-string 1)))
	("application/pdf" "pdf")
	("application/epub+zip" "epub")
	("text/plain" "txt")
	(_ nil)))))

(defun annas-archive--extension-from-url (url)
  "Return a file extension inferred from URL.
URL is the original download URL passed to `url-retrieve'."
  (when (stringp url)
    (file-name-extension url)))

(defun annas-archive-save-file (url path)
  "Save the file at URL to PATH."
  (let ((coding-system-for-write 'no-conversion))
    (write-region (point-min) (point-max) path))
  (message "Downloaded file: `%s'" path)
  (run-hook-with-args 'annas-archive-post-download-hook url path))

(defun annas-archive-handle-eww-failure (url)
  "Take appropriate action when `eww' fails to download file from URL.
Depending on the value of `annas-archive-when-eww-download-fails', download
externally, signal an error, or fail silently."
  (let ((message "Failed to download file with `eww'"))
    (pcase annas-archive-when-eww-download-fails
      ('external
       (annas-archive-download-file-externally url)
       (message (concat message " Downloading with the default browser instead")))
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
  (let (id)
    (goto-char (point-min))
    (re-search-forward "Account ID: \\(.*\\)" nil t)
    (setq id (match-string 1))
    (re-search-forward "Secret key (don’t share!): show\\(.*\\)" nil t)
    (if id
	(message "You are authenticated.\nAccount ID: %s\nTo see your secret key, visit %s"
		 id annas-archive-auth-url)
      (eww annas-archive-auth-url)
      (message "You don't seem to be authenticated. Please enter your key in the `eww' buffer."))))

(provide 'annas-archive)
;;; annas-archive.el ends here
