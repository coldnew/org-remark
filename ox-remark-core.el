;;; ox-remark-core.el ---

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

(eval-when-compile (require 'cl))


;;; Internal general purpose functions

(defun ox-remark--string-to-key (string)
  "Conver string to key. eq: \"test\" -> :test"
  (intern (format ":%s" string)))

(defun ox-remark--symbol-to-key (symbol)
  "Conver symbol to key. eq: test -> :test"
  (ox-remark--string-to-key (symbol-name symbol)))

(defun ox-remark--key-to-string (key)
  "Conver key to string. eq: :test -> \"test\""
  (let ((key-str (symbol-name key)))
    (s-right (- (length key-str) 1) key-str)))

(defun ox-remark--key-to-symbol (key)
  "Conver key to symbol. eq: test -> :test"
  (intern (ox-remark--key-to-string key)))

;; FIXME: not elegant
(defun ox-remark--remove-dulpicate-backslash (str)
  "Remove dulpicate backslash for str. If str contains `://', make it not modified."
  (replace-regexp-in-string
   ":/" "://"
   (replace-regexp-in-string "//*" "/"  str)))

(defun ox-remark--eval-lisp()
  "Eval embeded lisp code defined by <lisp> tags in html fragment
when parsing a template."
  (save-excursion
    (save-restriction
      (save-match-data
        ;; needed for thing-at-point
        (html-mode)
        (beginning-of-buffer)
        (let ((open-tag "<lisp>\\|{lisp}\\|\\[lisp\\]")
              (close-tag "</lisp>\\|{/lisp}\\|\\[/lisp\\]")
              beg end sexp)
          (while (search-forward-regexp open-tag nil t)
            (setq beg (- (point) (length  (match-string 0))))
            (when (search-forward-regexp close-tag nil t)
              (setq end (point))
              (backward-char (length (match-string 0)))
              (backward-sexp)
              (setq sexp (substring-no-properties (thing-at-point 'sexp)))
              (narrow-to-region beg end)
              (delete-region (point-min) (point-max))
              (insert
               (save-match-data
                 (condition-case err
                     (let ((object (eval (read sexp))))
                       (cond
                        ;; result is a string
                        ((stringp object) object)
                        ;; a list
                        ((and (listp object)
                              (not (eq object nil)))
                         (let ((string (pp-to-string object)))
                           (substring string 0 (1- (length string)))))
                        ;; a number
                        ((numberp object)
                         (number-to-string object))
                        ;; nil
                        ((eq object nil) "")
                        ;; otherwise
                        (t (pp-to-string object))))
                   ;; error handler
                   (error
                    (format "Lisp error in %s: %s" (buffer-file-name) err)))))
              (goto-char (point-min))
              (widen))))))))


(defun ox-remark--template-to-string (file)
  "Read the content of FILE in template dir and return it as string.
If template contains <lisp> ... </lisp>, evalute this block like o-blog does."
  (with-temp-buffer
    (insert-file-contents file) (ox-remark--eval-lisp) (buffer-string)))

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
  (flet ((plist-get-str (info key)
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


(provide 'ox-remark-core)
;;; ox-remark-core.el ends here.
