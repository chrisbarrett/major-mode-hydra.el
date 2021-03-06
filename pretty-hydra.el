;;; pretty-hydra.el --- A macro for creating nice-looking hydras -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>
;; URL: https://github.com/jerrypnz/major-mode-hydra.el
;; Version: 0.1.0
;; Package-Requires: ((hydra "0.13.4") (s "1.10.0") (dash "2.12.1") (emacs "24"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides a macro, `pretty-hydra-define', which defines a hydra with column for each group
;; of heads.

;;; Code:

(require 'dash)
(require 's)
(require 'hydra)

(defgroup pretty-hydra nil
  "Generate preformatted hydras easily."
  :group 'bindings
  :prefix "pretty-hydra-")

(defcustom pretty-hydra-head-formatting-function #'pretty-hydra-default-head-formatting-function
  "The function to use to convert a hydra head into a string.

It is called with two arguments: the key and optional string hint."
  :group 'pretty-hydra
  :type 'function)

(defun pretty-hydra--calc-column-width (column-name heads)
  (->> heads
       (-map (-lambda ((key _ hint))
               (cond
                ((char-or-string-p hint) (+ 7 (length key) (length hint))) ;; string hint
                ((or (null hint) (symbolp hint)) 0) ;; no hint
                (t 17))))   ;; dynamic hint (TODO trim to 10 chars long)
       (cons (+ 2 (length column-name)))
       -max))

(defun pretty-hydra-default-head-formatting-function (key &optional hint)
  (cond
   ((char-or-string-p hint) ;; string hint
    (list (format " [_%s_] %s" key hint)))
   ((or (null hint) (symbolp hint)) ;; no hint, doesn't show it in docstring at all
    nil)
   (t  ;; dynamic hint (TODO trim to 10 chars long)
    (list (format " [_%s_] ?%s?" key key)))))


(defun pretty-hydra--gen-heads-docstring (column-name heads max-heads)
  (-let ((column-len (pretty-hydra--calc-column-width column-name heads)))
    (-as-> heads docs
           (-mapcat (-lambda ((key _ hint))
                      (funcall pretty-hydra-head-formatting-function key hint))
                    docs)
           (-concat (list (format " %s^^" column-name)
                          (format "%s" (s-pad-right column-len "─" "")))
                    docs
                    ;; Add empty rows if it doesn't have as many heads in this column
                    (-repeat (- max-heads (length docs)) (s-pad-left column-len " " "^^")))
           (-map (lambda (doc) (s-pad-right column-len " " doc)) docs))))

(defun pretty-hydra--gen-body-docstring (hydra-plist)
  (-let* ((head-columns (-partition 2 hydra-plist))
          (max-heads (->> head-columns
                          (-map (-lambda ((_ heads)) (length heads)))
                          -max))
          (head-docstrings (-map (-lambda ((column-name heads))
                                   (pretty-hydra--gen-heads-docstring column-name heads max-heads))
                                 head-columns)))
    (->> head-docstrings
         (apply #'-zip)
         (-map-indexed (lambda (i ss)
                         (s-join (if (= i 1) "" " ")
                                 (if (listp (cdr ss))
                                     ss
                                   (cons (car ss) (cons (cdr ss) nil)))))) ;; cons-pair to list because of -zip
         (s-join "\n")
         (format "\n%s\n"))))

(defun pretty-hydra--get-heads (hydra-plist)
  (->> hydra-plist
       (-partition 2)
       (-mapcat #'cadr)
       (-map (-lambda ((head &as key cmd hint . opts))
               (if (char-or-string-p hint)
                   (-concat (list key cmd) opts)
                 head)))))

;;;###autoload
(cl-defmacro pretty-hydra-define (name body heads-plist
                           &key
                           (docstring-formatter #'identity)
                           (docstring-prefix ""))
  (declare (indent defun) (doc-string 3))
  (let* ((docstring (pretty-hydra--gen-body-docstring heads-plist))
         (updated-docstring (-as-> docstring it
                                   (concat (eval docstring-prefix) it)
                                   (funcall docstring-formatter it)
                                   ;; Docstring must start with a newline.
                                   (if (s-starts-with? "\n" it)
                                       it
                                     (concat "\n" it))))
         (heads (append '(("q" nil :exit t)
                          ("<escape>" nil :exit t))
                        (pretty-hydra--get-heads heads-plist))))
    `(defhydra ,name ,body
       ,updated-docstring
       ,@heads)))

(provide 'pretty-hydra)

;;; pretty-hydra.el ends here
