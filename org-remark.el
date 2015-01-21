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

(eval-when-compile (require 'cl))


;;; org-remark info

(defconst org-remark-version "0.1"
  "Blogit version string.")

(defconst org-remark-url
  "http://github.com/coldnew/org-remark"
  "Url for org-remakr.")

;;;; Load all org-remark functions
(mapcar (lambda (x) (require (intern (format "ox-remark-%s" x)) nil t))
        '("vars" "core" "html"))



;;; End-user functions



(provide 'org-remark)
;;; org-remark.el ends here.
