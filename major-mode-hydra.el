;;; major-mode-hydra.el --- Major mode keybindings managed by Hydra -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jerry Peng

;; Author: Jerry Peng <pr2jerry@gmail.com>
;; URL: https://github.com/jerrypnz/major-mode-hydra.el
;; Keywords: hydra
;; Version: 0.1.0
;; Package-Requires: ((dash "2.12.1"))

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

;;; Code:

(require 'dash)
(require 'pretty-hydra)

(defvar major-mode-hydra--heads-alist nil
  "An alist holding hydra heads for each major mode, keyed by the mode name.")

(defvar major-mode-hydra--body-cache nil
  "An alist holding compiled hydras for each major mode.
Whenever `major-mode-hydra--heads-alist' is changed, the hydra
for the mode gets recompiled.")

(defun major-mode-hydra--recompile (mode heads)
  "Recompile a hydra for a major MODE from a list of hydra HEADS.
The created hydra has :exit set to t, which means after a
head command gets executed, it exits the hydra."
  (let ((hydra-name (make-symbol (format "major-mode-hydras/%s" mode)))
        ;; By default, exit hydra after invoking a head and warn if a foreign key is pressed.
        (hydra-body '(:exit t :hint nil :foreign-keys warn))
        ;; Convert heads to a plist that `pretty-hydra-define' expects.
        (hydra-heads-plist (->> heads
                                reverse
                                (-group-by #'car)
                                (-mapcat (-lambda ((column . heads)) (list column (-map #'cdr heads)))))))
    (eval `(pretty-hydra-define ,hydra-name ,hydra-body ,hydra-heads-plist))))

(defun major-mode-hydra--get-or-recompile (mode)
  "Get the existing hydra from the cache, or create a new one for the given major MODE."
  (-if-let (hydra (alist-get mode major-mode-hydra--body-cache))
      hydra
    (-when-let (heads (alist-get mode major-mode-hydra--heads-alist))
      (let ((hydra (major-mode-hydra--recompile mode heads)))
        (setf (alist-get mode major-mode-hydra--body-cache) hydra)
        hydra))))

(defun major-mode-hydra--add-heads (heads column bindings)
  "Update the HEADS list and add to the given COLUMN a list of BINDINGS.
If the key is already bound to another head in the HEADS list, it
logs an error without adding that binding."
  (-reduce-from (-lambda (heads (key command . hint-and-plist))
                  (-let [(hint . plist) (if (and (car hint-and-plist)
                                                 (not (keywordp (car hint-and-plist))))
                                            hint-and-plist
                                          (cons (symbol-name command) hint-and-plist))]
                    (-if-let ((_ _ cmd) (-first (lambda (h) (equal (cadr h) key)) heads))
                        (progn
                          (message "\"%s\" has already been bound to %s" key cmd)
                          heads)
                      (cons `(,column ,key ,command ,hint ,@plist) heads))))
                heads
                bindings))

(defun major-mode-hydra--bind-key (mode column bindings)
  "Bind commands to hydra heads for a major MODE under the given COLUMN.
Each binding in BINDINGS should follow the same format as the
head-list in `defhydra'.

Existing hydra for the given MODE also gets removed from the
cache so that next time its gets recompiled with the updated
heads."
  (-as-> (alist-get mode major-mode-hydra--heads-alist) heads
         (major-mode-hydra--add-heads heads column bindings)
         (setf (alist-get mode major-mode-hydra--heads-alist)
               heads))
  ;; Invalidate cached hydra for the mode
  (setq major-mode-hydra--body-cache
        (assq-delete-all mode major-mode-hydra--body-cache)))

(defun major-mode-hydra--unbind-all (mode)
  "Remove all the hydra heads for the given MODE.
Intended for testing."
  (setq major-mode-hydra--body-cache
        (assq-delete-all mode major-mode-hydra--body-cache))
  (setq major-mode-hydra--heads-alist
        (assq-delete-all mode major-mode-hydra--heads-alist)))

(defmacro major-mode-hydra-bind (mode column &rest body)
  "Bind commands to hydra heads for a major MODE under the given COLUMN.
Each binding in BODY should follow the same format as the
head-list in `defhydra'.

This is a macro which calls the function `major-mode-hydra--bind-key'
under the hood.  It's introduced so that it's unecessary to quote
 the mode name and the bindings."
  (declare (indent defun))
  `(major-mode-hydra--bind-key ',mode ,column ',body))

(defun major-mode-hydra ()
  "Summon the major mode hydra for the major mode of current buffer, if there is one."
  (interactive)
  (let* ((mode major-mode)
         (hydra (major-mode-hydra--get-or-recompile mode)))
    (if hydra
        (call-interactively hydra)
      (message "Major mode hydra not found for %s" mode))))

(provide 'major-mode-hydra)
;;; major-mode-hydra.el ends here
