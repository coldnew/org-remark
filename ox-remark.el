;;; org-remark.el ---

;; Copyright (c) 2015 Yen-Chin, Lee.
;;
;; Author: Yen-Chin, Lee <coldnew.tw@gmail.com>
;; Keywords: html presentation org-mode
;; X-URL: http://github.com/coldnew/org-remark
;; Version: 0.1
;; Package-Requires: ((emacs "24.1") (org "8.0") (cl-lib "0.5") (f "0.17.2") (ht "2.0") (dash "2.6.0") (mustache "0.22"))

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

(eval-when-compile (require 'cl-lib))

(require 'ox-html)
(require 'ox-md)
(require 'ox-publish)
(require 'ht)
(require 'mustache)
(require 'dash)
(require 's)


;;; org-remark info

(defconst org-remark-version "0.1"
  "org-remark version string.")

(defconst org-remark-url
  "http://github.com/coldnew/org-remark"
  "Url for org-remark.")

(defconst org-remark-path
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Get the absolute path of dir contains ox-remark.el")


;;; User Configuration Variables

(defvar ox-remark-template-list
  (list
   ;; TODO: replace with slide_header
   :page_header        "page_header.html"
   :page_footer        "page_footer.html"

   :plugin_analytics   "plugin_analytics.html"
   :plugin_lloogg      "plugin_lloogg.html"
   :plugin_fancybox    "plugin_fancybox.html"

   ;; New slide template
   :slide_template     "slide.org")
  "Template filename define for ox-remark to parse.")

;;; Customized Variables


;;; Internal variables

(defvar ox-remark-directory
  (file-name-directory load-file-name))

(defvar ox-remark-template-directory
  (directory-file-name (concat ox-remark-directory "templates")))

;;;; Load all org-remark functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Need to rewrite

(defun ox-remark--string-to-key (string)
  "Conver string to key. eq: \"test\" -> :test"
  (intern (format ":%s" string)))

(defun ox-remark--key-to-string (key)
  "Conver key to string. eq: :test -> \"test\""
  (let ((key-str (symbol-name key)))
    (s-right (- (length key-str) 1) key-str)))

;; FIXME: not elegant
(defun ox-remark--remove-dulpicate-backslash (str)
  "Remove dulpicate backslash for str. If str contains `://', make it not modified."
  (replace-regexp-in-string
   ":/" "://"
   (replace-regexp-in-string "//*" "/"  str)))


(defun ox-remark--template-to-string (file)
  "Read the content of FILE in template dir and return it as string.
If template contains <lisp> ... </lisp>, evalute this block like o-blog does."
  (with-temp-buffer
    (insert-file-contents file) (buffer-string)))

(defun ox-remark--template-fullfile (key)
  "Get match template filename with fullpath according to key."
  (let* ((filename (plist-get ox-remark-template-list key))
         (keystr (ox-remark--key-to-string key))
         (keypost (ox-remark--string-to-key (format "%s_post" keystr)))
         (filename1 (plist-get ox-remark-template-list keypost))
         fullfile)

    ;; Check if key is exist in `ox-remark-template-list', if not
    ;; take it as real file
    (when (not filename)
      ;; If template in key_post format exist
      (if filename1 (setq filename filename1)
        (setq filename keystr)))

    ;; find file, if file is exist, take it as absolute file,
    ;; else think it under ox-remark-template-directory
    (setq fullfile
          (if (file-exists-p filename)
              (expand-file-name filename)
              (concat ox-remark-template-directory "/" filename)))

    ;; Convert file to standart filename
    (convert-standard-filename (ox-remark--remove-dulpicate-backslash fullfile))))


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

(defmacro ox-remark--build-context (info &rest pairs)
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

    ;; Extra blogit plugin
    ("PLUGIN_ANALYTICS" (or (ox-remark--render-analytics-template ,info) ""))
    ("PLUGIN_LLOOGG" (or (ox-remark--render-lloogg-template ,info) ""))

    ,@pairs))

;; TODO: seems like we can reduce some function here.

(defun ox-remark--render-template (type context)
  "Read the file contents, then render it with a hashtable context."
  (let ((file (or (ox-remark--template-fullfile type) type)))
    (mustache-render (ox-remark--template-to-string file) context)))

(defun ox-remark--render-analytics-template (info)
  (let ((analytics (or (ox-remark--parse-option info :analytics) "")))
    (when (and analytics (not (string= "" analytics)))
      (ox-remark--render-template :plugin_analytics (ht ("ANALYTICS" analytics))))))

(defun ox-remark--render-lloogg-template (info)
  (let ((lloogg (or (ox-remark--parse-option info :lloogg) "")))
    (when (and lloogg (not (string= "" lloogg)))
      (ox-remark--render-template :plugin_lloogg (ht ("LLOOGG" lloogg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ox-remark--build-context-html (info &rest pairs)
  "Create a hash table with the key-value pairs given.
Keys are compared with `equal'.

\(fn (KEY-1 VALUE-1) (KEY-2 VALUE-2) ...)

This function is used to create context for blogit-render function,
many useful context is predefined here, but you can overwrite it.
"
  `(ox-remark--build-context
    ,info

    ;; context from blogit template
    ("PAGE_HEADER" (ox-remark--render-header-template ,info))
    ("PAGE_FOOTER" (ox-remark--render-footer-template ,info))

    ,@pairs))


;;; Template render function

(defun ox-remark--render-header-template (info)
  (ox-remark--render-template :page_header (ox-remark--build-context info)))

(defun ox-remark--render-footer-template (info)
  (ox-remark--render-template :page_footer (ox-remark--build-context info)))



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

(defun org-remark--build-head (info)
  "Return information for the <head>..</head> of the HTML output.
INFO is a plist used as a communication channel."
  (org-element-normalize-string
   (concat
    (org-element-normalize-string (plist-get info :html-head))
    (org-element-normalize-string (plist-get info :html-head-extra))
    (when (and (plist-get info :html-htmlized-css-url)
               (eq org-html-htmlize-output-type 'css))
      (org-html-close-tag "link"
                          (format " rel=\"stylesheet\" href=\"%s\" type=\"text/css\""
                                  (plist-get info :html-htmlized-css-url))
                          info)))))

(defun org-remark-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     ;;     (when depth (org-blogit-toc depth info))
     )

   ;; Header
   (ox-remark--render-header-template info)

   ;; Document contents.
   contents

   ;; Footnotes section.
   (org-html-footnote-section info)

   ;; Footer
   (ox-remark--render-footer-template info)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Internal functions

(defun org-remark--save-and-export ()
  (org-remark-export-to-html))


;;; End-user functions

;;;###autoload
(defun org-remark-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer for blogit.

Export is done in a buffer named \"*Blogit HTML Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'ox-remark "*Org remark Export*"
    async subtreep visible-only nil nil (lambda () (html-mode))))

;;;###autoload
(defun org-remark-export-to-html (&optional async subtreep visible-only)
  "Export current buffer to a Markdown file.

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

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".html" subtreep)))
    (org-export-to-file 'ox-remark outfile async subtreep visible-only)))

;;;###autoload
(define-minor-mode org-remark-save-and-export-mode
  "Serves the buffer live over HTTP."
  :group 'org-remark-save-and-export-mode
  :lighter " remark"
  (save-restriction
    (if org-remark-save-and-export-mode
        (add-hook 'after-save-hook nil 'append 'make-it-local)
      (remove-hook 'after-save-hook 'org-remark--save-and-export 'make-it-local))
    org-remark-save-and-export-mode))

(provide 'org-remark)
;;; org-remark.el ends here.
