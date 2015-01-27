;;; org-remark.el ---

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

(require 'ox-html)
(require 'ox-md)
(require 'ox-publish)
(require 'ht)
(require 'mustache)
(require 'dash)
(require 's)

(eval-when-compile (require 'cl))


;;; org-remark info

(defconst org-remark-version "0.1"
  "Blogit version string.")

(defconst org-remark-url
  "http://github.com/coldnew/org-remark"
  "Url for org-remakr.")


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
(mapcar (lambda (x) (require (intern (format "ox-remark-%s" x)) nil t))
        '("core" "html"))


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

(define-minor-mode org-remark-save-and-export-mode
  "Serves the buffer live over HTTP."
  :group 'org-remark-save-and-export-mode
  :lighter " remark-se"
  ;;  (impatient-mode)
  ;;  (imp-set-user-filter #'org-remark--htmlize-filter)
  (save-restriction
    (if org-remark-save-and-export-mode
        (add-hook 'after-save-hook nil 'append 'make-it-local)
      (remove-hook 'after-save-hook 'org-remark--save-and-export 'make-it-local))
    org-remark-save-and-export-mode))


(provide 'org-remark)
;;; org-remark.el ends here.
