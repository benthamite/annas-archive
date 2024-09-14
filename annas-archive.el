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

(defvar annas-archive-callback nil
  "Callback function to run by `annas-archive-download-file'.")

(defvar annas-archive-bibtex-key nil
  "BibTeX key of the book being downloaded.")

(defconst annas-archive-home-url
  "https://annas-archive.org/"
  "URL to Anna’s Archive.")

(defconst annas-archive-auth-url
  (concat annas-archive-home-url "account/")
  "URL to authenticate with Anna’s Archive.")

;;;; User options

(defgroup annas-archive ()
  "Rudimentary integration for Anna’s Archive."
  :group 'eww)

(defcustom annas-archive-downloads-dir
  (expand-file-name "~/Downloads/")
  "Directory where files downloaded from Anna’s Archive are saved."
  :type 'directory
  :group 'annas-archive)

(defcustom annas-archive-use-eww nil
  "Whether to use `eww' for downloading files.
If t, files will be downloaded directly with `eww'.

It used to be the case that one could authenticate a session with `eww', but as
of 2024-07-13 this does not appear to be possible anymore. Non-authenticated
sessions require the user to click on a captcha screen visible only to browsers
that support Javascript. Thus, this user option should be set to nil for the
time being."
  :type 'boolean
  :group 'annas-archive)

(defcustom annas-archive-use-fast-download-links t
  "Whether to use fast download links from Anna’s Archive.
If t, use fast download links available to paying members."
  :type 'boolean
  :group ''annas-archive)

;;;; Functions

(defun annas-archive-download (&optional string confirm callback)
  "Search Anna’s Archive for STRING and download the selected item.
If STRING is nil, prompt for a search string. If both STRING and CONFIRM are
non-nil, prompt the user for confirmation to use STRING as the search string.

CALLBACK is a function called when the process concludes. The function takes two
arguments: the file to attach and the BibTeX key of the entry from which this
function was called, if any."
  (interactive)
  (save-window-excursion
    (let* ((string (cond ((and string confirm)
			  (read-string "Search string: " string))
			 (string string)
			 (t (read-string "Search string: "))))
	   (url (format "%ssearch?index=&page=1&q=%s&ext=pdf&sort="
			annas-archive-home-url (url-encode-url string))))
      (when callback (setq annas-archive-callback callback))
      (add-hook 'eww-after-render-hook #'annas-archive-select-and-open-url)
      (eww url))))

(defun annas-archive-select-and-open-url ()
  "Get the download URLs from the Anna’s Archive search results buffer."
  (remove-hook 'eww-after-render-hook #'annas-archive-select-and-open-url)
  (save-window-excursion
    (let (links)
      (dolist (cons (annas-archive-collect-links-in-buffer))
	(when (string-match-p "\\.pdf" (car cons))
	  (push cons links)))
      (if links
	  (let* ((selection (completing-read "Select a link: " links nil t))
		 (url (alist-get selection links nil nil 'string=)))
	    (add-hook 'eww-after-render-hook #'annas-archive-proceed-to-download-page)
	    (eww url))
	(message "No results found.")))))

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

(defun annas-archive-proceed-to-download-page ()
  "Proceed to the Anna’s Archive download page."
  (remove-hook 'eww-after-render-hook #'annas-archive-proceed-to-download-page)
  (save-window-excursion
    (let* ((speed (if annas-archive-use-fast-download-links "Fast" "Slow"))
	   (url (annas-archive-get-url-in-link (concat speed " Partner Server"))))
      (if annas-archive-use-eww
	  (progn
	    (add-hook 'eww-after-render-hook #'annas-archive-download-file)
	    (eww url))
	(browse-url-default-browser url)))))

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

(defvar ebib-extras-attach-file-key)
(defun annas-archive-download-file ()
  "Download the file from the Anna’s Archive download page."
  (remove-hook 'eww-after-render-hook 'annas-archive-download-file)
  (let* ((bibtex-key ebib-extras-attach-file-key)
	 (url (annas-archive-get-download-url))
	 (raw-file (file-name-nondirectory url))
	 (sans-extension (file-name-sans-extension raw-file))
	 (extension (file-name-extension raw-file))
	 (filename (file-name-with-extension (substring sans-extension 0 100) extension))
	 (final-path (file-name-concat annas-archive-downloads-dir filename))
	 (temp-path (file-name-with-extension final-path ".tmp"))
	 (callback annas-archive-callback))
    (setq annas-archive-callback nil)
    (let ((process (start-process "download-file" "*download-output*" "curl" "-o" temp-path "-L" url)))
      (set-process-sentinel process
			    (lambda (_proc event)
			      (cond ((string= event "finished\n")
				     (rename-file temp-path final-path 'ok-if-already-exists)
				     (message "Downloaded `%s' to `%s`" filename final-path)
				     (annas-archive-run-callback callback final-path bibtex-key))
				    ((string-prefix-p "exited abnormally" event)
				     (string-match "code \\([0-9]+\\)" event)
				     (message "Download failed with error code %s for `%s`"
					      (match-string 1 event) filename))
				    (t
				     (message "Unexpected process event: %s" event))))))
    (message "Downloading `%s'..." filename)))

(defun annas-archive-run-callback (callback file key)
  "When CALLBACK is non-nil, run it with FILE and KEY as arguments.
FILE is the file to attach and KEY is the BibTeX key of the associated entry."
  (when callback
    (funcall callback file key)))

(defun annas-archive-get-download-url ()
  "Get the download URL from the Anna’s Archive download page."
  (or (annas-archive-get-url-in-link "Download now")
      (let ((generic-error "Could not find download link")
	    (quota-exceeded "You’ve run out of fast downloads today"))
	(goto-char (point-min))
	(user-error (cond ((re-search-forward quota-exceeded nil t)
			   quota-exceeded)
			  (t generic-error))))))

;;;;; Authentication

(defun annas-archive-authenticate ()
  "Authenticate with Anna’s Archive.
Note that as of 2024-07-13 this does not appear to be working."
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

