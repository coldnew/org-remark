;;; ox-remark.el --- Export org-mode to remark.js

;; Copyright (c) 2015 - 2016 Yen-Chin, Lee.
;;
;; Author: Yen-Chin, Lee <coldnew.tw@gmail.com>
;; Keywords: html presentation org-mode
;; X-URL: http://github.com/coldnew/org-remark
;; Version: 0.2
;; Package-Requires: ((emacs "24.3") (org "8.3") (ht "2.0") (dash "2.6.0") (mustache "0.22"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; [![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg?dummy)](http://www.gnu.org/licenses/gpl-3.0.txt)
;; [![Build Status](https://travis-ci.org/coldnew/org-remark.svg?branch=master)](https://travis-ci.org/coldnew/org-remark)

;;; Code:

;;; Dependencies

(require 'cl)
(require 'ox-html)
(require 'ox-md)
(require 'mustache)
(require 'ht)                           ; hash table library
(require 'dash)
(require 's)


;;; org-remark info

(defconst org-remark-url
  "http://github.com/coldnew/org-remark"
  "Url for org-remark.")

(defconst org-remark-path
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Get the absolute path of dir contains ox-remark.el.")

(defconst org-remark-template-file
  (concat org-remark-path "/templates/default.html"))


;;; Internal variables



;;; Define Back-End for org-export

(org-export-define-derived-backend 'ox-remark 'md
  :translate-alist
  '(
    (inner-template . org-remark-inner-template)
    (section . org-remark-section)
    (link . org-html-link)
    ;; Use emacs buildin syntax highlight
    (src-block . org-remark-src-block)
    (example-block . org-remark-example-block)
    (headline     . org-remark-headline)
    ))


;;;; headline

(defun org-remark-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Google I/O slides.
CONTENTS holds the contents of the headline. INFO is a plist
holding contextual information."
  ;; First call org-html-headline to get the formatted HTML contents.
  ;; Then add enclosing <article> tags to mark slides.
  (let* ((info (plist-put info :headline-offset
                          (string-to-number (or (org-element-property :OFFSET headline) "0"))))
         (class (org-element-property :CLASS headline)))

    (format "\n%s\n\n%s"
            (if class (concat "class: " class) "")
            (org-md-headline headline contents info))))


;;;; Example Block and Src Block

(defun org-remark-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  ;; following code based on `org-html-example-block'.
  (format "<pre class=\"example\">%s</pre>"
          (org-html-format-code example-block info)))

;;;; Src Block

(defun org-remark-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (org-html-src-block src-block contents info)
  ;; (format "```sh\n%s\n```" (org-remove-indentation
  ;;                           (org-export-format-code-default src-block info)))
  )


;;; Section
(defun org-remark-section (section contents info)
  "Transcode a SECTION element from Org to HTML.
CONTENTS holds the contents of the section. INFO is a plist
holding contextual information."
  ;; Just return the contents. No "<div>" tags.
  ;;  contents
  (let ((parent (org-export-get-parent-headline section)))
    ;; Before first headline: no container, just return CONTENTS.
    (if (not parent) contents
      ;; Get div's class and id references.
      (let* ((class-num (+ (org-export-get-relative-level parent info)
                           (1- org-html-toplevel-hlevel)))
             (section-number
              (mapconcat
               'number-to-string
               (org-export-get-headline-number parent info) "-")))

        ;;  "article"
        ;;  (format "class=\"%s\" id=\"text-%s\""
        ;;          (or (org-element-property :ARTICLE parent) "")
        ;;          (or (org-element-property :CUSTOM_ID parent) section-number))


        (format "%s\n---\n" contents)))))


;;; Template

;; FIXME: remo
(defun ox-remark--parse-option (info key)
  "Read option value of org file opened in current buffer.

This function will first use the standard way to parse org-option.
If parsing failed, use regexp to get the options, else return nil.
"
  (cl-flet ((plist-get-str (info key)
                           (let ((r (plist-get info key)))
                             (if (stringp r) r (or (car r) "")))))
    (let* ((keystr1 (symbol-name key))
           (keystr (upcase (s-right (- (length keystr1) 1) keystr1)))
           (match-regexp (org-make-options-regexp `(,keystr)))
           (option (plist-get-str info key)))

      ;; if we can use plist-get to get org-option, use it
      ;; else use regexp to find options
      (or (if (not (string= "" option)) option)
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward match-regexp nil t)
              (match-string-no-properties 2 nil)))))))

(defun ox-remark--template-to-string (file)
  "Read the content of FILE in template dir and return it as string.
If template contains <lisp> ... </lisp>, evalute this block like o-blog does."
  (with-temp-buffer
    (insert-file-contents file) (buffer-string)))

(defmacro ox-remark--build-context (info contents &rest pairs)
  "Create a hash table with the key-value pairs given.
Keys are compared with `equal'.

\(fn (KEY-1 VALUE-1) (KEY-2 VALUE-2) ...)

This function is used to create context for blogit-render function,
many useful context is predefined here, but you can overwrite it.
"
  `(ht
    ("TITLE"  (or (ox-remark--parse-option ,info :title) "Untitled"))
    ("AUTHOR" (or (ox-remark--parse-option ,info :author) user-full-name "Unknown"))
    ("EMAIL" (or (ox-remark--parse-option ,info :email) user-mail-address ""))

    ("CHARSET" (or (ox-remark--parse-option ,info :charset) "UTF-8"))

    ("CONTENTS" (or ,contents ""))

    ,@pairs))

(defun org-remark-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((file org-remark-template-file))
    (mustache-render
     (ox-remark--template-to-string file)
     (ox-remark--build-context info contents))))


;;; End-user functions

;;;###autoload
(defun org-remark-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "
Export is done in a buffer named \"*Reamrk HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'ox-remark "*Org remark Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-remark-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension)
                                    org-html-extension
                                    "html")))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'ox-remark file
      async subtreep visible-only body-only ext-plist)))


(provide 'ox-remark)
;;; ox-remark.el ends here
