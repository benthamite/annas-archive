;;; annas-archive.el --- Rudimentary integration for Anna’s Archive -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Pablo Stafforini

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
(require 'dom)
(require 'json)
(require 'shr)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)

;;;; Variables

;;;;; Anna’s Archive

(defvar annas-archive-home-url nil
  "Obsolete URL to Anna's Archive.")

(make-obsolete-variable
 'annas-archive-home-url
 "Anna's Archive URLs are resolved dynamically. Set `annas-archive-home-url-override' only for temporary manual overrides."
 "2026-06-07")

(defcustom annas-archive-home-url-override nil
  "Override URL to Anna's Archive.
When nil, resolve the current URL from Wikipedia and cache it for the current
Emacs session."
  :type '(choice (const :tag "Resolve from Wikipedia" nil) string)
  :group 'annas-archive)

(defvar annas-archive--home-url-cache nil
  "Cached Anna's Archive home URL for the current Emacs session.")

(defconst annas-archive-fast-download-api-path
  "dyn/api/fast_download.json"
  "Path to the fast download JSON API endpoint.")

(defconst annas-archive--wikipedia-api-url
  "https://en.wikipedia.org/w/api.php?action=query&prop=revisions&titles=Anna%27s_Archive&rvprop=content&rvslots=main&format=json&formatversion=2"
  "MediaWiki API URL for the Anna's Archive article wikitext.")

(defconst annas-archive-supported-file-types
  '("pdf" "epub" "fb2" "mobi" "cbr" "djvu" "cbz" "txt" "azw3")
  "List of supported file extensions.")

;;;;; Regexps

(defconst annas-archive--re-size
  "\\([0-9]+\\(?:\\.[0-9]+\\)?[[:space:]]*[MGK]B\\)"
  "Regexp matching a human-readable size like \"1.2 MB\" in a block.")

(defconst annas-archive--re-language
  "^[[:space:]]*\\([^·\n]+\\)[[:space:]]*·[[:space:]]*[A-Z]\\{3,6\\}[[:space:]]*·"
  "Regexp matching the language token line in a block.")

(defconst annas-archive--re-year
  "·[[:space:]]*\\([12][0-9]\\{3\\}\\)[[:space:]]*·"
  "Regexp matching the publication year token in a block.")

(defconst annas-archive--re-ext-from-filename
  "\\.\\([[:alpha:]]\\{2,6\\}\\)[ \t]*\\'"
  "Regexp matching a filename-ending extension like \".epub\".")

(defconst annas-archive--re-ext-from-token
  "·[[:space:]]*\\([A-Z]\\{3,6\\}\\)[[:space:]]*·"
  "Regexp matching an uppercase extension token like \"· EPUB ·\".")

;;;;; DOIs

(defconst annas-archive--doi-regexp
  "\\(10\\.[0-9]\\{4,9\\}/[-._;()/:A-Za-z0-9]+\\)$"
  "Regular expression that matches a DOI.
Case-insensitive: matches both uppercase and lowercase DOI suffixes.")

;;;; User options

(defgroup annas-archive ()
  "Rudimentary integration for Anna’s Archive."
  :group 'emacs)

(define-error 'annas-archive-no-matching-results
  "No Anna's Archive results match the configured file types"
  'user-error)

(define-error 'annas-archive-transient-search-error
  "Anna's Archive returned a transient search error")

;;;;; Main options

(defcustom annas-archive-secret-key nil
  "Secret key for the Anna's Archive fast download API.
When set, enables programmatic downloads directly within Emacs via the fast
download API. To find your key, log into Anna's Archive with a paid membership
and visit the account page."
  :type '(choice (const :tag "Not set" nil) string)
  :group 'annas-archive)

(make-obsolete-variable
 'annas-archive-use-eww
 "Set `annas-archive-secret-key' instead."
 "2026-02-15")

(make-obsolete-variable
 'annas-archive-use-fast-download-links
 "Set `annas-archive-secret-key' instead."
 "2026-02-15")

(define-obsolete-variable-alias
  'annas-archive-when-eww-download-fails
  'annas-archive-when-download-fails
  "2026-02-15")

(defcustom annas-archive-when-download-fails 'external
  "What to do when a programmatic download fails.
If `external' (default), download the file with the default browser. If `error',
signal an error. Otherwise, fail silently."
  :type '(choice (const :tag "Download externally" external)
                 (const :tag "Signal error" error)
                 (const :tag "Fail silently" nil))
  :group 'annas-archive)

(defcustom annas-archive-downloads-dir
  (expand-file-name "~/Downloads/")
  "Directory where files downloaded from Anna’s Archive are saved.
This user option is only relevant when `annas-archive-secret-key' is set."
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

(defcustom annas-archive-search-retries 10
  "Maximum retries after a transient Anna's Archive search response."
  :type 'natnum
  :group 'annas-archive)

(defcustom annas-archive-search-retry-delay 1
  "Seconds to wait before retrying a transient search response."
  :type 'natnum
  :group 'annas-archive)

(defcustom annas-archive-search-backend 'auto
  "Backend used to fetch Anna's Archive search pages.
`auto' uses the Chrome bridge when its socket exists and direct HTTP otherwise.
`chrome' uses the Chrome to Emacs extension in an existing browser window.  It
creates an inactive tab, lets Chrome execute JavaScript challenges, reads the
resulting DOM, and closes the tab.  `direct' uses Emacs's URL library and cannot
run JavaScript challenges."
  :type '(choice
	  (const :tag "Chrome bridge when available; direct HTTP otherwise" auto)
	  (const :tag "Chrome to Emacs bridge" chrome)
	  (const :tag "Direct HTTP" direct))
  :group 'annas-archive)

(defcustom annas-archive-search-mirrors
  '("annas-archive.pk" "annas-archive.gd" "annas-archive.gl")
  "Anna's Archive hosts tried after transient search failures.
The order matters.  The host in the original search URL is appended when it is
not already present."
  :type '(repeat string)
  :group 'annas-archive)

(defcustom annas-archive-chrome-bridge-socket nil
  "Unix-domain socket for the Chrome to Emacs bridge.
When nil, use the socket created by the installed Chrome to Emacs native host."
  :type '(choice (const :tag "Use the standard socket" nil) file)
  :group 'annas-archive)

(defcustom annas-archive-chrome-timeout 45
  "Maximum seconds to wait for a Chrome bridge search.
Values below 1 or above 45 are clamped to that range."
  :type 'natnum
  :group 'annas-archive)

(defvar annas-archive--last-chrome-telemetry nil
  "Telemetry from the most recent Chrome bridge search response.")

(defcustom annas-archive-post-download-hook nil
  "Hook run after downloading a file from Anna’s Archive.
Each function is called with the URL as its first argument and, when the file
was downloaded programmatically, the destination path as its second argument."
  :type 'hook
  :group 'annas-archive)

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
  (let* ((prompt "Search string: ")
	 (string (if (called-interactively-p 'interactive)
		     (read-string prompt)
		   (annas-archive--require-nonempty-string string)))
	 (results (annas-archive--search (annas-archive--url-for-query string))))
    (if results
	(annas-archive--select-results results)
      (message "No results found"))))

;;;;; Parsing

(defun annas-archive--doi-p (string)
  "Return non-nil if STRING is a valid DOI.
STRING is the user input, typically a DOI like \"10.1145/1458082.1458150\"."
  (and (stringp string)
       (string-match-p annas-archive--doi-regexp (string-trim string))))

(defun annas-archive--require-nonempty-string (string)
  "Return STRING trimmed, or signal an error if it is nil or empty.
STRING is the user input."
  (let ((s (string-trim (or string ""))))
    (if (string-empty-p s)
	(user-error "Search string must be non-empty when called non-interactively")
      s)))

(defun annas-archive--url-for-query (string)
  "Return the Anna’s Archive URL to use for STRING.
DOIs use the journals index. Other strings use the default search index."
  (let ((s (string-trim (or string "")))
	(home-url (annas-archive--home-url)))
    (if (annas-archive--doi-p s)
	(concat home-url "search?index=journals&q="
		(url-hexify-string (format "\"doi:%s\"" s)))
      (concat home-url "search?q=" (url-hexify-string s)))))

(defun annas-archive--home-url ()
  "Return the Anna's Archive home URL for this session."
  (annas-archive--normalize-home-url
   (or annas-archive-home-url-override
       annas-archive--home-url-cache
       (setq annas-archive--home-url-cache
	     (annas-archive--wikipedia-home-url)))))

(defun annas-archive--wikipedia-home-url ()
  "Return the current Anna's Archive home URL from Wikipedia."
  (annas-archive--wikipedia-home-url-from-wikitext
   (annas-archive--wikipedia-wikitext)))

(defun annas-archive--wikipedia-home-url-from-wikitext (wikitext)
  "Return the first Anna's Archive infobox URL in WIKITEXT."
  (let ((case-fold-search nil))
    (or (and (string-match "|[ \t]*url[ \t]*=" wikitext)
	     (string-match "{{URL[[:space:]\n]*|[[:space:]\n]*\\(https://annas-archive\\.[^][|{}\n[:space:]]+/?\\)"
			   wikitext (match-end 0))
	     (annas-archive--normalize-home-url (match-string 1 wikitext)))
	(user-error "Could not find Anna's Archive URL in Wikipedia article"))))

(defun annas-archive--normalize-home-url (url)
  "Return normalized Anna's Archive home URL from URL."
  (unless (and (stringp url)
	       (string-match-p "\\`https://annas-archive\\.[[:alnum:]-]+/?\\'" url))
    (user-error "Invalid Anna's Archive URL: %S" url))
  (if (string-suffix-p "/" url)
      url
    (concat url "/")))

(defun annas-archive--wikipedia-wikitext-from-json (json)
  "Return Anna's Archive article wikitext from MediaWiki JSON."
  (let* ((query (alist-get 'query json))
	 (pages (alist-get 'pages query))
	 (page (car pages))
	 (revisions (alist-get 'revisions page))
	 (revision (car revisions))
	 (slots (alist-get 'slots revision))
	 (main (alist-get 'main slots))
	 (content (alist-get 'content main)))
    (if (stringp content)
	content
      (user-error "Could not find Wikipedia article wikitext in response"))))

(defun annas-archive--wikipedia-wikitext ()
  "Fetch and return the latest Anna's Archive Wikipedia wikitext."
  (let ((buffer (url-retrieve-synchronously annas-archive--wikipedia-api-url t nil 30)))
    (unless buffer
      (user-error "Could not fetch Anna's Archive Wikipedia article"))
    (unwind-protect
	(with-current-buffer buffer
	  (goto-char (point-min))
	  (unless (re-search-forward "\n\n" nil t)
	    (user-error "Could not parse Wikipedia response headers"))
	  (let ((json-object-type 'alist)
		(json-array-type 'list)
		(json-key-type 'symbol))
	    (annas-archive--wikipedia-wikitext-from-json (json-read))))
      (kill-buffer buffer))))

(defvar url-http-response-status)
(defvar url-request-extra-headers)

(defun annas-archive--search (url)
  "Return validated search results fetched from URL.
Retry transient network, DDoS-Guard, rate-limit, and server responses."
  (let ((attempt 0)
	(urls (annas-archive--search-urls url)))
    (catch 'results
      (while t
	(let ((current-url (nth (mod attempt (length urls)) urls)))
	  (condition-case err
	      (throw 'results
		     (annas-archive--fetch-search-results current-url))
	    (annas-archive-transient-search-error
	     (if (>= attempt annas-archive-search-retries)
		 (user-error
		  "Anna's Archive search failed after %d attempts: %s"
		  (1+ attempt) (error-message-string err))
	       (cl-incf attempt)
	       (message "Anna's Archive search failed on %s; retrying (%d/%d)"
			(url-host (url-generic-parse-url current-url))
			attempt annas-archive-search-retries)
	       (sleep-for annas-archive-search-retry-delay)))))))))

(defun annas-archive--search-urls (url)
  "Return mirror search URLs derived from URL."
  (if (not (string-match
	    "\\`https://\\(annas-archive\\.[^/]+\\)\\(/search\\?.*\\)\\'"
	    url))
      (list url)
    (let ((original-host (match-string 1 url))
	  (suffix (match-string 2 url)))
      (mapcar (lambda (host) (concat "https://" host suffix))
	      (delete-dups
	       (append annas-archive-search-mirrors (list original-host)))))))

(defun annas-archive--fetch-search-results (url)
  "Fetch URL and return its parsed Anna's Archive search results.
Return nil only when the response contains the explicit empty-results marker."
  (pcase (annas-archive--effective-search-backend)
    ('chrome (annas-archive--fetch-search-results-with-chrome url))
    ('direct (annas-archive--fetch-search-results-directly url))))

(defun annas-archive--effective-search-backend ()
  "Return the concrete backend selected by `annas-archive-search-backend'."
  (pcase annas-archive-search-backend
    ('auto (if (file-exists-p (annas-archive--chrome-bridge-socket))
	       'chrome
	     'direct))
    ((or 'chrome 'direct) annas-archive-search-backend)
    (_ (user-error "Invalid Anna's Archive search backend: %S"
		   annas-archive-search-backend))))

(defun annas-archive--fetch-search-results-directly (url)
  "Fetch URL with Emacs's URL library and return parsed search results."
  (let* ((url-request-extra-headers
	  '(("Accept" . "text/html") ("Accept-Language" . "en")))
	 (buffer
	  (condition-case err
	      (url-retrieve-synchronously url t nil 30)
	    (error
	     (signal 'annas-archive-transient-search-error
		     (list (error-message-string err)))))))
    (unless buffer
      (signal 'annas-archive-transient-search-error
	      '("No HTTP response")))
    (unwind-protect
	(with-current-buffer buffer
	  (let ((status url-http-response-status)
		(body (annas-archive--http-response-body)))
	    (annas-archive--parse-search-response status body)))
      (kill-buffer buffer))))

(defun annas-archive--fetch-search-results-with-chrome (url)
  "Fetch URL through the Chrome bridge and return parsed search results."
  (let* ((response (annas-archive--chrome-bridge-request url))
	 (outcome (alist-get 'outcome response))
	 (message-text (or (alist-get 'message response)
			   "Chrome bridge returned an unknown error")))
    (setq annas-archive--last-chrome-telemetry
	  (alist-get 'telemetry response))
    (pcase outcome
      ("success"
	(let ((html (alist-get 'html response)))
	  (unless (stringp html)
	    (signal 'annas-archive-transient-search-error
		    '("Chrome bridge response has no HTML")))
	  (let ((classification (annas-archive--classify-search-html html)))
	    (if (eq (car classification) 'empty)
		(signal 'annas-archive-transient-search-error
			'("Chrome bridge reported results for an empty page"))
	      (annas-archive--parse-search-response 200 html)))))
      ("empty"
	(let ((html (alist-get 'html response)))
	  (unless (stringp html)
	    (signal 'annas-archive-transient-search-error
		    '("Chrome bridge response has no HTML")))
	  (annas-archive--parse-search-response 200 html)))
      ((or "transient" "malformed")
	(signal 'annas-archive-transient-search-error (list message-text)))
      (_ (signal 'annas-archive-transient-search-error
		 (list (format "Anna's Archive Chrome search failed: %s"
			       message-text)))))))

(defun annas-archive--chrome-bridge-request (url)
  "Ask the Chrome bridge to fetch URL and return its response."
  (let* ((socket (annas-archive--chrome-bridge-socket)))
    (unless (file-exists-p socket)
      (user-error "Chrome to Emacs bridge is not running: %s" socket))
    (let* ((buffer (generate-new-buffer " *annas-archive-chrome*"))
	   (request-id (format "%x-%x" (truncate (float-time)) (random)))
	   (timeout (max 1 (min annas-archive-chrome-timeout 45)))
	   (deadline (+ (float-time) timeout 5))
	   process)
      (unwind-protect
	  (progn
	    (condition-case err
		(setq process
		      (make-network-process
		       :name "annas-archive-chrome"
		       :buffer buffer
		       :family 'local
		       :service socket
		       :coding 'utf-8-unix
		       :noquery t))
	      (file-error
	       (signal 'annas-archive-transient-search-error
		       (list (error-message-string err)))))
	    (process-send-string
	     process
	     (concat
	      (json-encode
	       `((action . "fetch")
		 (id . ,request-id)
		 (url . ,url)
		 (timeout . ,(* 1000 timeout))))
	      "\n"))
	    (while (and (process-live-p process)
			(< (float-time) deadline))
	      (accept-process-output process 0.1))
	    (when (process-live-p process)
	      (delete-process process)
	      (signal 'annas-archive-transient-search-error
		      '("Chrome bridge timed out")))
	    (with-current-buffer buffer
	      (goto-char (point-min))
	      (when (= (point-min) (point-max))
		(signal 'annas-archive-transient-search-error
			'("Chrome bridge returned no response")))
	      (let ((json-object-type 'alist)
		    (json-array-type 'list)
		    (json-key-type 'symbol))
		(let ((response (json-read)))
		  (unless (or (null (alist-get 'id response))
			      (equal (alist-get 'id response) request-id))
		    (user-error "Chrome bridge returned a mismatched response"))
		  response))))
	(when (process-live-p process)
	  (delete-process process))
	(kill-buffer buffer)))))

(defun annas-archive--chrome-bridge-socket ()
  "Return the Chrome to Emacs bridge socket path."
  (or annas-archive-chrome-bridge-socket
      (expand-file-name
       (format "chrome-to-eww-%d/bridge.sock" (user-uid))
       temporary-file-directory)))

(defun annas-archive--parse-search-response (status body)
  "Parse a search response with HTTP STATUS and BODY.
Parseable results take precedence over stale challenge markers."
  (let ((classification (annas-archive--classify-search-html body)))
    (cond
     ((eq (car classification) 'results) (cdr classification))
     ((annas-archive--transient-search-response-p status body)
      (signal 'annas-archive-transient-search-error
	      (list (format "HTTP %s or challenge page"
			    (or status "unknown")))))
     ((not (and (integerp status) (<= 200 status) (< status 300)))
      (user-error "Anna's Archive search returned HTTP %s"
		  (or status "unknown")))
     ((eq (car classification) 'empty) nil)
     (t (signal 'annas-archive-transient-search-error
		'("Anna's Archive search response could not be parsed"))))))

(defun annas-archive--http-response-body ()
  "Return the HTTP response body in the current URL buffer."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "\r?\n\r?\n" nil t)
      (signal 'annas-archive-transient-search-error
	      '("Malformed HTTP response")))
    (buffer-substring-no-properties (point) (point-max))))

(defun annas-archive--transient-search-response-p (status body)
  "Return non-nil when STATUS and BODY describe a transient response."
  (or (memq status '(403 408 425 429))
      (and (integerp status) (>= status 500))
      (string-match-p
       (concat "<title>[[:space:]\n]*DDoS-Guard[[:space:]\n]*</title>"
	       "\\|Checking your browser before accessing"
	       "\\|could not verify your browser automatically"
	       "\\|id=[\"']js-challenge[\"']"
	       "\\|Our servers are not responding")
       body)))

(defun annas-archive--parse-search-html (html)
  "Return search results parsed from validated Anna's Archive HTML.
Return nil only if HTML renders with Anna's explicit empty-results marker."
  (pcase (annas-archive--classify-search-html html)
    (`(results . ,results) results)
    (`(empty) nil)
    (_ (user-error "Anna's Archive search response could not be parsed"))))

(defun annas-archive--classify-search-html (html)
  "Classify HTML as results, explicit empty, or malformed."
  (let ((empty-page (annas-archive--valid-empty-search-html-p html)))
    (with-temp-buffer
      (insert html)
      (let ((shr-use-fonts nil)
	    (shr-use-colors nil)
	    (shr-width 200))
	(shr-render-region (point-min) (point-max)))
      (let ((results (annas-archive-parse-results)))
	(cond
	 (results (cons 'results results))
	 (empty-page '(empty))
	 (t '(malformed)))))))

(defun annas-archive--dom-class-p (node class)
  "Return non-nil when NODE has CLASS as a class token."
  (member class (split-string (or (dom-attr node 'class) "") nil t)))

(defun annas-archive--valid-empty-search-html-p (html)
  "Return non-nil when HTML is a structurally valid empty search page."
  (condition-case nil
      (with-temp-buffer
	(insert html)
	(let* ((dom (libxml-parse-html-region (point-min) (point-max)))
	       (forms (dom-by-tag dom 'form))
	       (inputs (dom-by-tag dom 'input))
	       (spans (dom-by-tag dom 'span))
	       (search-form
		(cl-find-if
		 (lambda (node)
		   (and (annas-archive--dom-class-p node "js-search-form")
			(string= (downcase (or (dom-attr node 'method) "get")) "get")
			(string-match-p "\\(?:^\\|/\\)search\\(?:[?#].*\\)?\\'"
					(or (dom-attr node 'action) ""))))
		 forms))
	       (query-input
		(cl-find-if
		 (lambda (node)
		   (and (annas-archive--dom-class-p node "js-search-main-input")
			(string= (or (dom-attr node 'name) "") "q")
			(string= (downcase (or (dom-attr node 'type) "")) "search")))
		 inputs))
	       (empty-marker
		(cl-find-if
		 (lambda (node)
		   (and (annas-archive--dom-class-p node "font-bold")
			(string= (string-trim (dom-texts node)) "No files found.")))
		 spans))
	       (page-text (dom-texts dom " ")))
	  (and search-form query-input empty-marker
	       (string-match-p
		"Try fewer or different search terms and filters\\."
		page-text))))
    (error nil)))

(defun annas-archive-parse-results ()
  "Parse the current Anna’s Archive results buffer.
Return a list of plists with bibliographic details for each hit.
Each plist has keys :title, :url, :type, :size, :language and :year.
TITLE is taken from the MD5 link whose visible text is not \"*\".
TYPE is a lowercase extension like \"pdf\" or \"epub\"."
  (let* ((links (annas-archive-get-links))
         (mappings (annas-archive--build-url-mappings links))
         (url->titles (plist-get mappings :url->titles))
         (star-urls (plist-get mappings :star-urls)))
    (annas-archive--combine-url-info star-urls url->titles)))

(defun annas-archive-get-links ()
  "Return link titles and URLs from the current SHR-rendered buffer."
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
	  (setq end (or (next-single-property-change (point) 'shr-url) (point-max))))
	(goto-char (max end (1+ (point)))))
      (nreverse candidates))))

(defun annas-archive--build-url-mappings (links)
  "Build URL mappings from LINKS.
Return a plist with :url->titles (a hash table mapping URLs to lists of titles)
and :star-urls (a list of URLs in order)."
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
    (list :url->titles url->titles :star-urls star-urls)))

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
         (list :title (if best
			  (replace-regexp-in-string
			   "[[:space:]]+" " " (string-trim best))
			"*")
	       :url url :type type :size size :language lang :year year)))
     star-urls infos)))

(defun annas-archive--md5-url-p (url)
  "Return non-nil if URL appears to be an Anna’s Archive item (md5) page."
  (and (stringp url)
       (let ((case-fold-search nil))
         (string-match-p "/md5/[0-9a-f]\\{8,\\}" url))))

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
	  (if trim
	      (replace-regexp-in-string
	       "[[:space:]]+" " " (string-trim s))
	    s))))))

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
  "Prompt for one result from the current Archive results buffer and download it.
Only include links whose file types match TYPES (list of lowercase extensions).
If TYPES is nil, use `annas-archive-included-file-types'."
  (interactive)
  (annas-archive--select-result (annas-archive-parse-results) types))

(defun annas-archive--select-result (results &optional types)
  "Prompt for one item from RESULTS and download it.
Only include result TYPES, or `annas-archive-included-file-types' if nil."
  (let* ((wanted (mapcar #'downcase
			 (or types annas-archive-included-file-types)))
	 (filtered (cl-remove-if-not
		    (lambda (r) (member (plist-get r :type) wanted))
		    results))
	 (cands (annas-archive--format-candidates filtered)))
    (if (null cands)
	(signal 'annas-archive-no-matching-results (list wanted))
      (let* ((choice (completing-read "Select a link: " (mapcar #'car cands) nil t))
	     (url    (cdr (assoc choice cands))))
	(annas-archive--download-result url)))))

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

(defun annas-archive--select-results (results)
  "Prompt for one result from validated Anna's Archive RESULTS.
Retry with all supported file types when the included types yield no hits and
`annas-archive-retry-with-all-file-types' is non-nil."
  (condition-case nil
      (annas-archive--select-result results)
    (annas-archive-no-matching-results
     (if (and annas-archive-retry-with-all-file-types
	      (not (equal (sort (copy-sequence annas-archive-included-file-types) #'string<)
			  (sort (copy-sequence annas-archive-supported-file-types) #'string<)))
	      (y-or-n-p "No results match configured file types. Try all supported types? "))
	 (condition-case nil
	     (annas-archive--select-result
	      results annas-archive-supported-file-types)
	   (annas-archive-no-matching-results
	    (message "Search results contain no supported file types")))
       (message "No results match configured file types")))))

;;;;; Downloading

(autoload 'browse-url-default-browser "browse-url")

(defun annas-archive--download-result (page-url)
  "Download the item at PAGE-URL without visiting its page in Emacs."
  (let ((page-url (url-expand-file-name page-url
					(annas-archive--home-url))))
    (if (annas-archive--use-fast-download-api-p)
	(let* ((md5 (annas-archive--md5-from-url page-url))
	       (download-url (and md5 (annas-archive--fast-download-api md5))))
	  (if download-url
	      (annas-archive-download-file-internally download-url)
	    (annas-archive-handle-download-failure page-url)))
      (annas-archive-download-file-externally page-url))))

(defun annas-archive--use-fast-download-api-p ()
  "Return non-nil when the fast download API can be used."
  (and (stringp annas-archive-secret-key)
       (not (string-empty-p annas-archive-secret-key))))

(defun annas-archive--md5-from-url (url)
  "Extract the MD5 hash from an Anna's Archive URL.
URL is a string like \"https://annas-archive.gl/md5/d6e1dc51...\"."
  (when (stringp url)
    (let ((case-fold-search nil))
      (when (string-match "/md5/\\([0-9a-f]+\\)" url)
        (match-string 1 url)))))

(defun annas-archive--fast-download-error-message (err)
  "Return a user-friendly message for fast download API error ERR."
  (pcase err
    ("Invalid secret key"
     "Fast download API: invalid secret key. Check `annas-archive-secret-key'.")
    ("Not a member"
     "Fast download API: your account does not have a paid membership.")
    ("No downloads left"
     "Fast download API: daily download quota exhausted. Try again tomorrow.")
    ("Record not found"
     "Fast download API: record not found. The file may not exist in Anna's Archive.")
    ("Invalid domain_index or path_index"
     "Fast download API: file not available for fast download on this server.")
    ("Error during fetching"
     "Fast download API: server error. Try again later.")
    (_
     (format "Fast download API error: %s" err))))

(defun annas-archive--fast-download-api (md5)
  "Return a direct download URL for MD5 using the fast download API.
Returns the download URL string, or nil on failure."
  (let* ((api-url (format "%s%s?md5=%s&key=%s&path_index=0&domain_index=0"
			  (annas-archive--home-url)
			  annas-archive-fast-download-api-path
			  (url-hexify-string md5)
			  (url-hexify-string annas-archive-secret-key)))
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
			(message "%s" (annas-archive--fast-download-error-message err)))
		      nil))
		(json-error
		 (message "Fast download API returned invalid JSON")
		 nil))))
	(kill-buffer buffer)))))

(defun annas-archive-download-file-internally (url)
  "Download the file at URL programmatically within Emacs."
  (url-retrieve url (annas-archive-download-file-callback url))
  (message "Found download link. Proceeding to download..."))

(defun annas-archive-download-file-externally (url)
  "Download the file in URL with the default browser.
URL is the URL of the download link."
  (browse-url-default-browser url)
  (run-hook-with-args 'annas-archive-post-download-hook url))

(defun annas-archive-download-file-callback (url)
  "Return a callback for saving the file downloaded from URL.
URL is the download URL passed to `url-retrieve'."
  (lambda (status)
    "STATUS is the status of the download process; see `url-retrieve' for details."
    (if-let ((err (plist-get status :error)))
	(message "Download failed: %s" err)
      (let* ((redirect (plist-get status :redirect))
	     (extension (or (annas-archive--extension-from-url redirect)
			    (annas-archive--extension-from-headers)
			    (annas-archive--extension-from-url url)
			    "pdf")))
	;; Strip HTTP headers from the response buffer.
	(goto-char (point-min))
	(when (re-search-forward "\r?\n\r?\n" nil t)
	  (delete-region (point-min) (point)))
	(if (annas-archive--response-body-html-p)
	    (annas-archive-handle-download-failure url)
	  (let* ((base (make-temp-name "downloaded-from-annas-archive-"))
		 (filename (file-name-with-extension base extension))
		 (path (file-name-concat annas-archive-downloads-dir filename)))
	    (if (and (stringp path) (not (string-empty-p path)))
		(annas-archive-save-file url path)
	      (annas-archive-handle-download-failure url))))))))

(defun annas-archive--response-body-html-p ()
  "Return non-nil if the current buffer appears to contain HTML.
This indicates the server returned a challenge page (e.g. DDoS Guard)
rather than the expected file."
  (save-excursion
    (goto-char (point-min))
    (looking-at-p "[ \t\n\r]*<\\(?:!DOCTYPE\\|[hH][tT][mM][lL]\\)")))

(defun annas-archive--extension-from-headers ()
  "Return a file extension inferred from the current buffer’s HTTP headers."
  (save-excursion
    (goto-char (point-min))
    ;; Only the most common MIME types are mapped here; uncommon types
    ;; (djvu, mobi, fb2, etc.) fall through to URL-based detection.
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
    (file-name-extension (url-file-nondirectory
			  (car (url-path-and-query (url-generic-parse-url url)))))))

(defun annas-archive-save-file (url path)
  "Save the file at URL to PATH."
  (let ((dir (file-name-directory path)))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (let ((coding-system-for-write 'no-conversion))
    (write-region (point-min) (point-max) path))
  (message "Downloaded file: `%s'" path)
  (run-hook-with-args 'annas-archive-post-download-hook url path))

(defun annas-archive-handle-download-failure (url)
  "Take appropriate action when a programmatic download fails for URL.
Depending on the value of `annas-archive-when-download-fails', download
externally, signal an error, or fail silently."
  (let ((msg "Failed to download file programmatically"))
    (pcase annas-archive-when-download-fails
      ('external
       (annas-archive-download-file-externally url)
       (message (concat msg ". Downloading with the default browser instead")))
      ('error (user-error "%s" msg))
      (_ (message "%s" msg)))))

;;;; Migration warning

(with-eval-after-load 'annas-archive
  (when (and (or (bound-and-true-p annas-archive-use-fast-download-links)
		 (bound-and-true-p annas-archive-use-eww))
	     (not (annas-archive--use-fast-download-api-p)))
    (display-warning
     'annas-archive
     "`annas-archive-use-fast-download-links' and `annas-archive-use-eww' are obsolete.
Set `annas-archive-secret-key' to your account secret key instead.
See https://github.com/benthamite/annas-archive for details."
     :warning)))

(provide 'annas-archive)
;;; annas-archive.el ends here
