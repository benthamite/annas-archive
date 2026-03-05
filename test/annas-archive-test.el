;;; annas-archive-test.el --- Tests for annas-archive.el -*- lexical-binding: t -*-

;; Tests for the annas-archive Emacs package: DOI validation, URL
;; construction, metadata parsing, download helpers, and formatting.

;;; Code:

(require 'ert)
(require 'annas-archive)

;;;; Helpers

(defmacro annas-archive-test--with-block (text &rest body)
  "Insert TEXT into a temp buffer and evaluate BODY.
Point starts at `point-min'."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-min))
     ,@body))

(defmacro annas-archive-test--with-home-url (url &rest body)
  "Bind `annas-archive-home-url' to URL and evaluate BODY."
  (declare (indent 1))
  `(let ((annas-archive-home-url ,url))
     ,@body))

;;;; DOI validation

(ert-deftest annas-archive-test-doi-p-valid ()
  "A standard DOI should be recognized."
  (should (annas-archive--doi-p "10.1145/1458082.1458150")))

(ert-deftest annas-archive-test-doi-p-valid-with-whitespace ()
  "DOIs with surrounding whitespace should be recognized."
  (should (annas-archive--doi-p "  10.1145/1458082.1458150  ")))

(ert-deftest annas-archive-test-doi-p-valid-mixed-case ()
  "DOI suffixes are case-insensitive."
  (should (annas-archive--doi-p "10.1038/nature12373")))

(ert-deftest annas-archive-test-doi-p-valid-with-parens ()
  "DOIs may contain parentheses."
  (should (annas-archive--doi-p "10.1002/(SICI)1097-0258")))

(ert-deftest annas-archive-test-doi-p-valid-long-prefix ()
  "DOIs can have long registrant codes."
  (should (annas-archive--doi-p "10.123456789/abc")))

(ert-deftest annas-archive-test-doi-p-rejects-plain-text ()
  "Plain text should not be recognized as a DOI."
  (should-not (annas-archive--doi-p "The Great Gatsby")))

(ert-deftest annas-archive-test-doi-p-rejects-isbn ()
  "An ISBN should not be recognized as a DOI."
  (should-not (annas-archive--doi-p "978-0-06-112008-4")))

(ert-deftest annas-archive-test-doi-p-rejects-nil ()
  "Nil input should not crash."
  (should-not (annas-archive--doi-p nil)))

(ert-deftest annas-archive-test-doi-p-rejects-empty ()
  "Empty string should not be recognized as a DOI."
  (should-not (annas-archive--doi-p "")))

(ert-deftest annas-archive-test-doi-p-rejects-short-prefix ()
  "Prefix with fewer than 4 digits after 10. should be rejected."
  (should-not (annas-archive--doi-p "10.12/abc")))

(ert-deftest annas-archive-test-doi-p-rejects-no-suffix ()
  "A DOI prefix without a suffix after / should be rejected."
  (should-not (annas-archive--doi-p "10.1234/")))

;;;; Input validation

(ert-deftest annas-archive-test-require-nonempty-valid ()
  "A normal string should be returned trimmed."
  (should (equal (annas-archive--require-nonempty-string "  hello  ") "hello")))

(ert-deftest annas-archive-test-require-nonempty-signals-on-nil ()
  "Nil should signal a user-error."
  (should-error (annas-archive--require-nonempty-string nil) :type 'user-error))

(ert-deftest annas-archive-test-require-nonempty-signals-on-empty ()
  "An empty string should signal a user-error."
  (should-error (annas-archive--require-nonempty-string "") :type 'user-error))

(ert-deftest annas-archive-test-require-nonempty-signals-on-whitespace ()
  "A whitespace-only string should signal a user-error."
  (should-error (annas-archive--require-nonempty-string "   ") :type 'user-error))

;;;; URL construction

(ert-deftest annas-archive-test-url-for-query-search ()
  "Regular search terms should produce a search URL."
  (annas-archive-test--with-home-url "https://annas-archive.gl/"
    (let ((url (annas-archive--url-for-query "The Great Gatsby")))
      (should (string-prefix-p "https://annas-archive.gl/search?q=" url))
      (should (string-match-p "Great" url)))))

(ert-deftest annas-archive-test-url-for-query-doi ()
  "A DOI should produce a SciDB URL."
  (annas-archive-test--with-home-url "https://annas-archive.gl/"
    (should (equal (annas-archive--url-for-query "10.1145/1458082.1458150")
                   "https://annas-archive.gl/scidb/10.1145/1458082.1458150"))))

(ert-deftest annas-archive-test-url-for-query-trims-input ()
  "Surrounding whitespace should be trimmed before URL construction."
  (annas-archive-test--with-home-url "https://annas-archive.gl/"
    (should (equal (annas-archive--url-for-query "  10.1145/1458082.1458150  ")
                   "https://annas-archive.gl/scidb/10.1145/1458082.1458150"))))

(ert-deftest annas-archive-test-url-for-query-encodes-special-chars ()
  "Special characters in search queries should be URL-encoded."
  (annas-archive-test--with-home-url "https://annas-archive.gl/"
    (let ((url (annas-archive--url-for-query "foo bar&baz")))
      (should-not (string-match-p " " url))
      (should-not (string-match-p "&baz" url)))))

(ert-deftest annas-archive-test-url-for-query-custom-domain ()
  "URL should use the custom domain from `annas-archive-home-url'."
  (annas-archive-test--with-home-url "https://annas-archive.li/"
    (should (string-prefix-p "https://annas-archive.li/search?q="
                             (annas-archive--url-for-query "test")))))

;;;; Download URL pattern

(ert-deftest annas-archive-test-download-url-pattern-matches-md5 ()
  "Pattern should match md5 download page URLs."
  (annas-archive-test--with-home-url "https://annas-archive.gl/"
    (should (string-match-p
             (annas-archive--download-url-pattern)
             "https://annas-archive.gl/md5/d6e1dc51a7b0dcdc3e2ef164a0f1e6b4"))))

(ert-deftest annas-archive-test-download-url-pattern-matches-scidb ()
  "Pattern should match scidb download page URLs."
  (annas-archive-test--with-home-url "https://annas-archive.gl/"
    (should (string-match-p
             (annas-archive--download-url-pattern)
             "https://annas-archive.gl/scidb/10.1145/1458082.1458150"))))

(ert-deftest annas-archive-test-download-url-pattern-rejects-search ()
  "Pattern should NOT match search page URLs."
  (annas-archive-test--with-home-url "https://annas-archive.gl/"
    (should-not (string-match-p
                 (annas-archive--download-url-pattern)
                 "https://annas-archive.gl/search?q=foobar"))))

(ert-deftest annas-archive-test-download-url-pattern-rejects-other-domain ()
  "Pattern should NOT match URLs from different domains."
  (annas-archive-test--with-home-url "https://annas-archive.gl/"
    (should-not (string-match-p
                 (annas-archive--download-url-pattern)
                 "https://example.com/md5/abc123"))))

;;;; MD5 URL validation

(ert-deftest annas-archive-test-md5-url-p-valid ()
  "Valid MD5 URL should be recognized."
  (should (annas-archive--md5-url-p "https://annas-archive.gl/md5/d6e1dc51a7b0dcdc")))

(ert-deftest annas-archive-test-md5-url-p-minimum-length ()
  "MD5 hash must be at least 8 hex characters."
  (should (annas-archive--md5-url-p "/md5/abcdef01"))
  (should-not (annas-archive--md5-url-p "/md5/abcdef0")))

(ert-deftest annas-archive-test-md5-url-p-rejects-uppercase ()
  "MD5 URL pattern should reject uppercase hex (MD5 hashes are lowercase)."
  (should-not (annas-archive--md5-url-p "/md5/ABCDEF01")))

(ert-deftest annas-archive-test-md5-url-p-rejects-nil ()
  "Nil should return nil without crashing."
  (should-not (annas-archive--md5-url-p nil)))

(ert-deftest annas-archive-test-md5-url-p-rejects-non-hex ()
  "Non-hex characters should cause rejection."
  (should-not (annas-archive--md5-url-p "/md5/zzzzzzzz")))

;;;; MD5 extraction from URL

(ert-deftest annas-archive-test-md5-from-url-extracts-hash ()
  "Should extract the MD5 hash from a valid URL."
  (should (equal (annas-archive--md5-from-url
                  "https://annas-archive.gl/md5/d6e1dc51a7b0dcdc3e2ef164a0f1e6b4")
                 "d6e1dc51a7b0dcdc3e2ef164a0f1e6b4")))

(ert-deftest annas-archive-test-md5-from-url-returns-nil-for-non-md5 ()
  "Should return nil when URL has no MD5 path."
  (should-not (annas-archive--md5-from-url "https://annas-archive.gl/search?q=test")))

(ert-deftest annas-archive-test-md5-from-url-returns-nil-for-nil ()
  "Should return nil for nil input."
  (should-not (annas-archive--md5-from-url nil)))

;;;; Fast download API availability

(ert-deftest annas-archive-test-use-fast-download-api-with-key ()
  "Should return non-nil when secret key is set."
  (let ((annas-archive-secret-key "my-secret-key"))
    (should (annas-archive--use-fast-download-api-p))))

(ert-deftest annas-archive-test-use-fast-download-api-without-key ()
  "Should return nil when secret key is nil."
  (let ((annas-archive-secret-key nil))
    (should-not (annas-archive--use-fast-download-api-p))))

(ert-deftest annas-archive-test-use-fast-download-api-empty-key ()
  "Should return nil when secret key is empty string."
  (let ((annas-archive-secret-key ""))
    (should-not (annas-archive--use-fast-download-api-p))))

;;;; Fast download error messages

(ert-deftest annas-archive-test-error-message-invalid-key ()
  "Invalid secret key should produce helpful message."
  (should (string-match-p "secret key"
                           (annas-archive--fast-download-error-message "Invalid secret key"))))

(ert-deftest annas-archive-test-error-message-not-a-member ()
  "Not a member error should mention paid membership."
  (should (string-match-p "paid membership"
                           (annas-archive--fast-download-error-message "Not a member"))))

(ert-deftest annas-archive-test-error-message-no-downloads ()
  "No downloads left should mention quota."
  (should (string-match-p "quota"
                           (annas-archive--fast-download-error-message "No downloads left"))))

(ert-deftest annas-archive-test-error-message-record-not-found ()
  "Record not found should mention the file."
  (should (string-match-p "not found"
                           (annas-archive--fast-download-error-message "Record not found"))))

(ert-deftest annas-archive-test-error-message-unknown ()
  "Unknown errors should include the original error text."
  (should (string-match-p "Something weird"
                           (annas-archive--fast-download-error-message "Something weird"))))

;;;; HTML response detection

(ert-deftest annas-archive-test-response-body-html-doctype ()
  "DOCTYPE declaration should be detected as HTML."
  (annas-archive-test--with-block "<!DOCTYPE html><html><body>Challenge</body></html>"
    (should (annas-archive--response-body-html-p))))

(ert-deftest annas-archive-test-response-body-html-tag ()
  "Bare <html> tag should be detected as HTML."
  (annas-archive-test--with-block "<html><body>Challenge</body></html>"
    (should (annas-archive--response-body-html-p))))

(ert-deftest annas-archive-test-response-body-html-with-leading-whitespace ()
  "HTML with leading whitespace should still be detected."
  (annas-archive-test--with-block "  \n  <!DOCTYPE html>"
    (should (annas-archive--response-body-html-p))))

(ert-deftest annas-archive-test-response-body-html-case-insensitive ()
  "Mixed-case HTML tag should be detected."
  (annas-archive-test--with-block "<HTML><BODY>test</BODY></HTML>"
    (should (annas-archive--response-body-html-p))))

(ert-deftest annas-archive-test-response-body-not-html-pdf ()
  "Binary PDF content should not be detected as HTML."
  (annas-archive-test--with-block "%PDF-1.4 binary content..."
    (should-not (annas-archive--response-body-html-p))))

(ert-deftest annas-archive-test-response-body-not-html-empty ()
  "Empty buffer should not be detected as HTML."
  (with-temp-buffer
    (should-not (annas-archive--response-body-html-p))))

;;;; Extension from headers

(ert-deftest annas-archive-test-extension-from-headers-pdf ()
  "Content-Type application/pdf should return \"pdf\"."
  (annas-archive-test--with-block "HTTP/1.1 200 OK\nContent-Type: application/pdf\n\n"
    (should (equal (annas-archive--extension-from-headers) "pdf"))))

(ert-deftest annas-archive-test-extension-from-headers-epub ()
  "Content-Type application/epub+zip should return \"epub\"."
  (annas-archive-test--with-block "HTTP/1.1 200 OK\nContent-Type: application/epub+zip\n\n"
    (should (equal (annas-archive--extension-from-headers) "epub"))))

(ert-deftest annas-archive-test-extension-from-headers-txt ()
  "Content-Type text/plain should return \"txt\"."
  (annas-archive-test--with-block "HTTP/1.1 200 OK\nContent-Type: text/plain; charset=utf-8\n\n"
    (should (equal (annas-archive--extension-from-headers) "txt"))))

(ert-deftest annas-archive-test-extension-from-headers-unknown ()
  "Unknown Content-Type should return nil."
  (annas-archive-test--with-block "HTTP/1.1 200 OK\nContent-Type: application/octet-stream\n\n"
    (should-not (annas-archive--extension-from-headers))))

(ert-deftest annas-archive-test-extension-from-headers-missing ()
  "Missing Content-Type header should return nil."
  (annas-archive-test--with-block "HTTP/1.1 200 OK\nX-Custom: foo\n\n"
    (should-not (annas-archive--extension-from-headers))))

;;;; Extension from URL

(ert-deftest annas-archive-test-extension-from-url-pdf ()
  "PDF extension should be extracted from download URL."
  (should (equal (annas-archive--extension-from-url
                  "https://download.example.com/file.pdf")
                 "pdf")))

(ert-deftest annas-archive-test-extension-from-url-epub ()
  "EPUB extension should be extracted from URL."
  (should (equal (annas-archive--extension-from-url
                  "https://cdn.example.com/files/book.epub")
                 "epub")))

(ert-deftest annas-archive-test-extension-from-url-nil ()
  "Nil should return nil."
  (should-not (annas-archive--extension-from-url nil)))

(ert-deftest annas-archive-test-extension-from-url-no-extension ()
  "URL without extension should return nil."
  (should-not (annas-archive--extension-from-url
               "https://cdn.example.com/download/abc123")))

;;;; Truncation and padding

(ert-deftest annas-archive-test-truncate-short-string ()
  "Short string should be padded to exact width."
  (let ((result (annas-archive--truncate "hi" 10)))
    (should (= (string-width result) 10))
    (should (string-prefix-p "hi" result))))

(ert-deftest annas-archive-test-truncate-exact-width ()
  "String at exact width should pass through unchanged."
  (should (equal (annas-archive--truncate "hello" 5) "hello")))

(ert-deftest annas-archive-test-truncate-long-string ()
  "Long string should be truncated with ellipsis."
  (let ((result (annas-archive--truncate "A very long title that exceeds the width" 15)))
    (should (= (string-width result) 15))
    (should (string-suffix-p "..." result))))

(ert-deftest annas-archive-test-truncate-collapses-whitespace ()
  "Internal whitespace should be collapsed."
  (let ((result (annas-archive--truncate "hello   world\nfoo" 30)))
    (should (string-prefix-p "hello world foo" result))))

(ert-deftest annas-archive-test-truncate-trims-input ()
  "Leading/trailing whitespace should be trimmed."
  (let ((result (annas-archive--truncate "  hello  " 10)))
    (should (string-prefix-p "hello" result))))

(ert-deftest annas-archive-test-truncate-nil-input ()
  "Nil input should be treated as empty string."
  (let ((result (annas-archive--truncate nil 5)))
    (should (= (string-width result) 5))))

(ert-deftest annas-archive-test-truncate-empty-input ()
  "Empty string should be padded to width."
  (let ((result (annas-archive--truncate "" 5)))
    (should (= (string-width result) 5))))

;;;; Block metadata extraction

(ert-deftest annas-archive-test-size-from-block ()
  "Should extract human-readable size from a result block."
  (annas-archive-test--with-block "English [en] · PDF · 1.2 MB · Some Title"
    (should (equal (annas-archive--size-from-block (point-min) (point-max))
                   "1.2 MB"))))

(ert-deftest annas-archive-test-size-from-block-kb ()
  "Should extract KB-sized values."
  (annas-archive-test--with-block "Some text · 512 KB · rest"
    (should (equal (annas-archive--size-from-block (point-min) (point-max))
                   "512 KB"))))

(ert-deftest annas-archive-test-size-from-block-gb ()
  "Should extract GB-sized values."
  (annas-archive-test--with-block "Some text · 2.5 GB · rest"
    (should (equal (annas-archive--size-from-block (point-min) (point-max))
                   "2.5 GB"))))

(ert-deftest annas-archive-test-size-from-block-missing ()
  "Should return nil when no size found."
  (annas-archive-test--with-block "Just some plain text"
    (should-not (annas-archive--size-from-block (point-min) (point-max)))))

(ert-deftest annas-archive-test-year-from-block ()
  "Should extract publication year from a result block."
  (annas-archive-test--with-block "Something · 2023 · rest"
    (should (equal (annas-archive--year-from-block (point-min) (point-max))
                   "2023"))))

(ert-deftest annas-archive-test-year-from-block-old ()
  "Should extract older publication years."
  (annas-archive-test--with-block "Something · 1984 · rest"
    (should (equal (annas-archive--year-from-block (point-min) (point-max))
                   "1984"))))

(ert-deftest annas-archive-test-year-from-block-missing ()
  "Should return nil when no year found."
  (annas-archive-test--with-block "Just some plain text"
    (should-not (annas-archive--year-from-block (point-min) (point-max)))))

(ert-deftest annas-archive-test-year-from-block-rejects-non-year ()
  "Should not match numbers outside the 1000-2999 range."
  (annas-archive-test--with-block "Something · 999 · rest"
    (should-not (annas-archive--year-from-block (point-min) (point-max)))))

(ert-deftest annas-archive-test-language-from-block ()
  "Should extract language token from a result block."
  (annas-archive-test--with-block "English [en] · PDF · 1.2 MB"
    (should (equal (annas-archive--language-from-block (point-min) (point-max))
                   "English [en]"))))

(ert-deftest annas-archive-test-language-from-block-missing ()
  "Should return nil when no language found."
  (annas-archive-test--with-block "Just some plain text"
    (should-not (annas-archive--language-from-block (point-min) (point-max)))))

(ert-deftest annas-archive-test-ext-from-block-filename ()
  "Should extract extension from a filename line."
  (annas-archive-test--with-block "Some Book Title\nbook-title.epub\nEnglish [en] · EPUB · 1.2 MB"
    (should (equal (annas-archive--ext-from-block (point-min) (point-max))
                   "epub"))))

(ert-deftest annas-archive-test-ext-from-block-token ()
  "Should extract extension from token when no filename found."
  (annas-archive-test--with-block "Some Book Title\nNo filename here\nEnglish [en] · EPUB · 1.2 MB · 2023 ·"
    (should (equal (annas-archive--ext-from-block (point-min) (point-max))
                   "epub"))))

(ert-deftest annas-archive-test-ext-from-block-pdf-filename ()
  "Should extract PDF extension from a filename."
  (annas-archive-test--with-block "Title\nmy-paper.pdf\nRest of metadata"
    (should (equal (annas-archive--ext-from-block (point-min) (point-max))
                   "pdf"))))

(ert-deftest annas-archive-test-ext-from-block-case-insensitive ()
  "Extracted extension should be lowercased."
  (annas-archive-test--with-block "Title\nmy-paper.PDF\nRest"
    (should (equal (annas-archive--ext-from-block (point-min) (point-max))
                   "pdf"))))

(ert-deftest annas-archive-test-ext-from-block-missing ()
  "Should return nil when no extension found."
  (annas-archive-test--with-block "Nothing useful here\nat all"
    (should-not (annas-archive--ext-from-block (point-min) (point-max)))))

;;;; Info in order (full metadata parsing)

(ert-deftest annas-archive-test-info-in-order ()
  "Should parse multiple result blocks in order."
  (annas-archive-test--with-block
      (concat
       "Preamble text\n"
       "   *   \n"
       "Title One\n"
       "book-one.pdf\n"
       "English [en] · PDF · 1.2 MB · 2020 ·\n"
       "   *   \n"
       "Title Two\n"
       "book-two.epub\n"
       "Spanish [es] · EPUB · 3.5 MB · 2019 ·\n")
    (let ((infos (annas-archive--info-in-order)))
      (should (= (length infos) 2))
      ;; First result
      (should (equal (plist-get (nth 0 infos) :type) "pdf"))
      (should (equal (plist-get (nth 0 infos) :size) "1.2 MB"))
      (should (equal (plist-get (nth 0 infos) :year) "2020"))
      (should (string-match-p "English" (plist-get (nth 0 infos) :language)))
      ;; Second result
      (should (equal (plist-get (nth 1 infos) :type) "epub"))
      (should (equal (plist-get (nth 1 infos) :size) "3.5 MB"))
      (should (equal (plist-get (nth 1 infos) :year) "2019"))
      (should (string-match-p "Spanish" (plist-get (nth 1 infos) :language))))))

(ert-deftest annas-archive-test-info-in-order-no-results ()
  "Should return nil when no star markers found."
  (annas-archive-test--with-block "No results here"
    (should-not (annas-archive--info-in-order))))

;;;; match-in-block

(ert-deftest annas-archive-test-match-in-block-basic ()
  "Should find first regex match in region."
  (annas-archive-test--with-block "abc 123 def"
    (should (equal (annas-archive--match-in-block
                    (point-min) (point-max)
                    "\\([0-9]+\\)" 1 nil)
                   "123"))))

(ert-deftest annas-archive-test-match-in-block-trim ()
  "Should trim result when requested."
  (annas-archive-test--with-block "abc  hello  def"
    (should (equal (annas-archive--match-in-block
                    (point-min) (point-max)
                    "\\(  hello  \\)" 1 t)
                   "hello"))))

(ert-deftest annas-archive-test-match-in-block-no-match ()
  "Should return nil when pattern not found."
  (annas-archive-test--with-block "abc def"
    (should-not (annas-archive--match-in-block
                 (point-min) (point-max)
                 "\\([0-9]+\\)" 1 nil))))

(ert-deftest annas-archive-test-match-in-block-respects-region ()
  "Should only search within the specified region."
  (annas-archive-test--with-block "123 abc 456"
    ;; Region from position 5 onwards should find 456, not 123.
    (let ((mid (+ (point-min) 4)))
      (should (equal (annas-archive--match-in-block
                      mid (point-max)
                      "\\([0-9]+\\)" 1 nil)
                     "456")))))

;;;; Link extraction (eww buffer simulation)

(ert-deftest annas-archive-test-get-links-extracts-shr-urls ()
  "Should extract links from text with `shr-url' properties."
  (with-temp-buffer
    ;; Simulate eww-rendered buffer with shr-url properties.
    (insert "Click ")
    (let ((start (point)))
      (insert "here")
      (put-text-property start (point) 'shr-url "https://example.com/link1"))
    (insert " or ")
    (let ((start (point)))
      (insert "there")
      (put-text-property start (point) 'shr-url "https://example.com/link2"))
    (insert " for info.")
    (let ((links (annas-archive-get-links)))
      (should (= (length links) 2))
      (should (equal (car (nth 0 links)) "here"))
      (should (equal (cdr (nth 0 links)) "https://example.com/link1"))
      (should (equal (car (nth 1 links)) "there"))
      (should (equal (cdr (nth 1 links)) "https://example.com/link2")))))

(ert-deftest annas-archive-test-get-links-empty-buffer ()
  "Should return nil for a buffer with no links."
  (with-temp-buffer
    (insert "Plain text without any links.")
    (should-not (annas-archive-get-links))))

;;;; Build URL mappings

(ert-deftest annas-archive-test-build-url-mappings ()
  "Should build correct URL->titles mapping and star-urls list."
  (let* ((links '(("Some Book Title" . "https://annas-archive.gl/md5/abc12345def67890")
                  ("*" . "https://annas-archive.gl/md5/abc12345def67890")
                  ("Another Title" . "https://annas-archive.gl/md5/1234567890abcdef")
                  ("*" . "https://annas-archive.gl/md5/1234567890abcdef")
                  ("External" . "https://example.com/other")))
         (result (annas-archive--build-url-mappings links))
         (url->titles (plist-get result :url->titles))
         (star-urls (plist-get result :star-urls)))
    ;; Two star URLs in order
    (should (= (length star-urls) 2))
    (should (equal (nth 0 star-urls) "https://annas-archive.gl/md5/abc12345def67890"))
    (should (equal (nth 1 star-urls) "https://annas-archive.gl/md5/1234567890abcdef"))
    ;; Hash table has both MD5 URLs but not the external one
    (should (gethash "https://annas-archive.gl/md5/abc12345def67890" url->titles))
    (should (gethash "https://annas-archive.gl/md5/1234567890abcdef" url->titles))
    (should-not (gethash "https://example.com/other" url->titles))))

(ert-deftest annas-archive-test-build-url-mappings-no-duplicates ()
  "Star URLs should not have duplicate entries."
  (let* ((links '(("*" . "https://x.com/md5/abcdef01")
                  ("Title" . "https://x.com/md5/abcdef01")
                  ("*" . "https://x.com/md5/abcdef01")))
         (result (annas-archive--build-url-mappings links))
         (star-urls (plist-get result :star-urls)))
    (should (= (length star-urls) 1))))

;;;; Combine URL info

(ert-deftest annas-archive-test-combine-url-info ()
  "Should combine star URLs, titles, and metadata into plists."
  (let* ((star-urls '("https://x.com/md5/aaa" "https://x.com/md5/bbb"))
         (url->titles (make-hash-table :test 'equal)))
    (puthash "https://x.com/md5/aaa" '("*" "A Great Book") url->titles)
    (puthash "https://x.com/md5/bbb" '("*" "Short" "A Much Longer Title") url->titles)
    (annas-archive-test--with-block
        (concat
         "   *   \n"
         "A Great Book\nbook.pdf\n"
         "English [en] · PDF · 1.2 MB · 2020 ·\n"
         "   *   \n"
         "A Much Longer Title\nother.epub\n"
         "French [fr] · EPUB · 3.5 MB · 2019 ·\n")
      (let ((results (annas-archive--combine-url-info star-urls url->titles)))
        (should (= (length results) 2))
        ;; First result: should pick "A Great Book" (longest non-"*" title)
        (should (equal (plist-get (nth 0 results) :title) "A Great Book"))
        (should (equal (plist-get (nth 0 results) :url) "https://x.com/md5/aaa"))
        ;; Second result: should pick "A Much Longer Title" (longest)
        (should (equal (plist-get (nth 1 results) :title) "A Much Longer Title"))))))

;;;; Format candidates

(ert-deftest annas-archive-test-format-candidates ()
  "Should format results as display-string/url pairs."
  (let ((results (list (list :title "My Book" :url "https://x.com/md5/aaa"
                             :type "pdf" :size "1.2 MB" :year "2020"
                             :language "English [en]"))))
    (let ((cands (annas-archive--format-candidates results)))
      (should (= (length cands) 1))
      (let ((display (car (car cands)))
            (url (cdr (car cands))))
        (should (string-match-p "My Book" display))
        (should (string-match-p "PDF" display))
        (should (string-match-p "1\\.2 MB" display))
        (should (string-match-p "2020" display))
        (should (equal url "https://x.com/md5/aaa"))))))

(ert-deftest annas-archive-test-format-candidates-nil-fields ()
  "Should handle nil metadata fields gracefully."
  (let ((results (list (list :title "Untitled" :url "https://x.com/md5/bbb"
                             :type nil :size nil :year nil :language nil))))
    (let ((cands (annas-archive--format-candidates results)))
      (should (= (length cands) 1))
      ;; Should not error; display string should exist
      (should (stringp (car (car cands)))))))

;;;; Download failure handling

(ert-deftest annas-archive-test-handle-download-failure-error ()
  "With `error' setting, should signal a user-error."
  (let ((annas-archive-when-download-fails 'error))
    (should-error (annas-archive-handle-download-failure "https://example.com")
                  :type 'user-error)))

(ert-deftest annas-archive-test-handle-download-failure-silent ()
  "With nil setting, should fail silently (just message)."
  (let ((annas-archive-when-download-fails nil))
    ;; Should not error
    (annas-archive-handle-download-failure "https://example.com")))

;;;; Ensure download page (guard function)

(ert-deftest annas-archive-test-ensure-download-page-not-eww ()
  "Should signal error when not in eww-mode."
  (with-temp-buffer
    (should-error (annas-archive-ensure-download-page) :type 'user-error)))

;;;; Regexp sanity checks

(ert-deftest annas-archive-test-re-size-matches ()
  "Size regexp should match various size formats."
  (should (string-match-p annas-archive--re-size "1.2 MB"))
  (should (string-match-p annas-archive--re-size "512KB"))
  (should (string-match-p annas-archive--re-size "0.5 GB")))

(ert-deftest annas-archive-test-re-year-matches ()
  "Year regexp should match years surrounded by bullets."
  (should (string-match-p annas-archive--re-year "· 2023 ·"))
  (should (string-match-p annas-archive--re-year "· 1984 ·")))

(ert-deftest annas-archive-test-re-year-rejects-non-year ()
  "Year regexp should reject numbers outside the valid range."
  (should-not (string-match-p annas-archive--re-year "· 0999 ·"))
  (should-not (string-match-p annas-archive--re-year "· 3000 ·")))

(ert-deftest annas-archive-test-re-ext-from-filename-matches ()
  "Extension-from-filename regexp should match file endings."
  (should (string-match-p annas-archive--re-ext-from-filename "book.pdf"))
  (should (string-match-p annas-archive--re-ext-from-filename "paper.epub"))
  (should (string-match-p annas-archive--re-ext-from-filename "archive.djvu")))

(ert-deftest annas-archive-test-re-ext-from-token-matches ()
  "Extension-from-token regexp should match uppercase tokens."
  (should (string-match-p annas-archive--re-ext-from-token "· PDF ·"))
  (should (string-match-p annas-archive--re-ext-from-token "· EPUB ·"))
  (should (string-match-p annas-archive--re-ext-from-token "· DJVU ·")))

(ert-deftest annas-archive-test-doi-regexp-matches ()
  "DOI regexp should match various DOI formats."
  (should (string-match-p annas-archive--doi-regexp "10.1145/1458082.1458150"))
  (should (string-match-p annas-archive--doi-regexp "10.1038/nature12373"))
  (should (string-match-p annas-archive--doi-regexp "10.1002/(SICI)1097-0258")))

(ert-deftest annas-archive-test-doi-regexp-rejects ()
  "DOI regexp should reject non-DOI strings."
  (should-not (string-match-p annas-archive--doi-regexp "978-0-06-112008-4"))
  (should-not (string-match-p annas-archive--doi-regexp "hello world"))
  (should-not (string-match-p annas-archive--doi-regexp "10.12/x")))

;;;; save-file (filesystem)

(ert-deftest annas-archive-test-save-file-writes-content ()
  "Should write buffer contents to the specified path."
  (let* ((dir (make-temp-file "annas-archive-test-" t))
         (path (file-name-concat dir "test-book.pdf"))
         (hook-called nil)
         (annas-archive-post-download-hook
          (list (lambda (url fpath)
                  (setq hook-called (list url fpath))))))
    (unwind-protect
        (with-temp-buffer
          (insert "fake pdf content")
          (annas-archive-save-file "https://example.com/file.pdf" path)
          ;; File should exist with correct content
          (should (file-exists-p path))
          (should (equal (with-temp-buffer
                           (insert-file-contents-literally path)
                           (buffer-string))
                         "fake pdf content"))
          ;; Hook should have been called with url and path
          (should (equal hook-called (list "https://example.com/file.pdf" path))))
      (delete-directory dir t))))

(ert-deftest annas-archive-test-save-file-creates-parent-dirs ()
  "Should create parent directories if they don't exist."
  (let* ((dir (make-temp-file "annas-archive-test-" t))
         (path (file-name-concat dir "sub" "dir" "book.epub")))
    (unwind-protect
        (with-temp-buffer
          (insert "fake epub")
          (let ((annas-archive-post-download-hook nil))
            (annas-archive-save-file "https://example.com/x" path))
          (should (file-exists-p path)))
      (delete-directory dir t))))

(ert-deftest annas-archive-test-save-file-binary-content ()
  "Should preserve binary content without encoding conversion."
  (let* ((dir (make-temp-file "annas-archive-test-" t))
         (path (file-name-concat dir "binary.pdf"))
         (binary-content (unibyte-string ?% ?P ?D ?F 0 1 2 255 254 253)))
    (unwind-protect
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert binary-content)
          (let ((annas-archive-post-download-hook nil))
            (annas-archive-save-file "https://example.com/x" path))
          (should (file-exists-p path))
          (let ((saved (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally path)
                         (buffer-string))))
            (should (equal saved binary-content))))
      (delete-directory dir t))))

;;;; Supported file types constant

(ert-deftest annas-archive-test-supported-file-types ()
  "Supported file types should include common ebook formats."
  (should (member "pdf" annas-archive-supported-file-types))
  (should (member "epub" annas-archive-supported-file-types))
  (should (member "mobi" annas-archive-supported-file-types))
  (should (member "djvu" annas-archive-supported-file-types)))

;;; annas-archive-test.el ends here
