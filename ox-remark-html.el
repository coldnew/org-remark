;;; ox-remark-html.el ---

;; Copyright (c) 2015 Yen-Chin, Lee.
;;
;; Author: Yen-Chin, Lee <coldnew.tw@gmail.com>
;; Keywords: html presentation org-mode
;; X-URL: http://github.com/coldnew/org-remark
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
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

;;; Code:

;;; Dependencies

(require 'ht)
(require 'dash)
(require 's)
(require 'ox-md)
(require 'impatient-mode)

(eval-when-compile (require 'cl))

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

;;;###autoload
(defun org-remark-open-browser()
  (interactive)
  ;;  (add-hook 'after-change-functions 'org-remark--change nil t)
  (browse-url (format "http://%s:%d/imp/live/%s"
                      "localhost" httpd-port (buffer-name (current-buffer)))))


(defun org-remark--htmlize-filter (buffer)
  (let ((org-export-show-temporary-export-buffer nil))

    (with-current-buffer buffer (org-remark-export-as-html))
    (insert-buffer-substring "*Org remark Export*")
    (kill-buffer "*Org remark Export*")
    ))


(provide 'ox-remark-html)
;;; ox-remark-html.el ends here.
