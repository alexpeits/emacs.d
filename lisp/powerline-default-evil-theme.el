;;; powerline-default-evil-theme.el --- Powerline theme for evil mode
;;;
;;; Author: Alex Peitsinis <alexpeitsinis@gmail.com>
;;; Url: https://github.com/alexpeits/powerline-default-evil-theme.el
;;; Version: 20200416.0
;;;
;;; Changelog :
;;;
;;; 20200416.0: Initial version
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, version 3 of the License.
;;;
;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs.
;;;
;;; This file is not a part of Emacs
;;;
;;; Commentary:
;;;
;;; Code:

(require 'powerline)

(defface powerline-active0-edited
  '((t (:inherit powerline-active0)))
  "Powerline active face 0 edited."
  :group 'powerline)

(defface powerline-active1-edited
  '((t (:inherit powerline-active1)))
  "Powerline active face 1 edited."
  :group 'powerline)

(defface powerline-active2-edited
  '((t (:inherit powerline-active2)))
  "Powerline active face 2 edited."
  :group 'powerline)

(defface powerline-inactive0-edited
  '((t (:inherit powerline-inactive0)))
  "Powerline inactive face 0 edited."
  :group 'powerline)

(defface powerline-inactive1-edited
  '((t (:inherit powerline-inactive1)))
  "Powerline inactive face 1 edited."
  :group 'powerline)

(defface powerline-inactive2-edited
  '((t (:inherit powerline-inactive2)))
  "Powerline inactive face 2 edited."
  :group 'powerline)

(defface powerline-active0-evil-insert
  '((t (:inherit powerline-active0)))
  "Powerline active face 0 evil insert."
  :group 'powerline)

(defface powerline-active0-evil-visual
  '((t (:inherit powerline-active0)))
  "Powerline active face 0 evil visual."
  :group 'powerline)

(defface powerline-active0-evil-replace
  '((t (:inherit powerline-active0)))
  "Powerline active face 0 evil replace."
  :group 'powerline)

(defface powerline-active0-evil-emacs
  '((t (:inherit powerline-active0)))
  "Powerline active face 0 evil emacs."
  :group 'powerline)

(defface powerline-active0-evil-operator
  '((t (:inherit powerline-active0)))
  "Powerline active face 0 evil operator."
  :group 'powerline)

(defvar powerline-default-evil-theme-state-faces
  '((insert . powerline-active0-evil-insert)
    (visual . powerline-active0-evil-visual)
    (replace . powerline-active0-evil-replace)
    (emacs . powerline-active0-evil-emacs)
    (motion . powerline-active0-evil-visual)
    (operator . powerline-active0-evil-operator)
    ))

(defun powerline-default-evil-theme-buffer-modified-p ()
  (and (not (string-prefix-p "*" (buffer-name)))
       (buffer-modified-p)))

(defun powerline-default-evil-theme ()
  "Powerline default evil theme."
  (interactive)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
             (mode-line-buffer-id
              (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
             (mode-line
              (if active 'mode-line 'mode-line-inactive))

             (face0
              (if active
                  (or (cdr (assoc evil-state powerline-default-evil-theme-state-faces))
                      (if (powerline-default-evil-theme-buffer-modified-p)
                          'powerline-active0-edited
                        'powerline-active0))
                (if (powerline-default-evil-theme-buffer-modified-p)
                    'powerline-inactive0-edited
                  'powerline-inactive0)))
             (face1
              (if (powerline-default-evil-theme-buffer-modified-p)
                  (if active 'powerline-active1-edited 'powerline-inactive1-edited)
                (if active 'powerline-active1 'powerline-inactive1)))
             (face2
              (if (powerline-default-evil-theme-buffer-modified-p)
                  (if active 'powerline-active2-edited 'powerline-inactive2-edited)
                (if active 'powerline-active2 'powerline-inactive2)))
             (face2-hud (if active 'powerline-active2 'powerline-inactive2))

             (separator-left (intern (format "powerline-%s-%s"
                                             (powerline-current-separator)
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              (powerline-current-separator)
                                              (cdr powerline-default-separator-dir))))
             (lhs (list (powerline-raw "%*" face0 'l)
                        (when powerline-display-buffer-size
                          (powerline-buffer-size face0 'l))
                        (when powerline-display-mule-info
                          (powerline-raw mode-line-mule-info face0 'l))
                        (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                        (when (and (boundp 'which-func-mode) which-func-mode)
                          (powerline-raw which-func-format face0 'l))
                        (powerline-raw " " face0)
                        (funcall separator-left face0 face1)
                        (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                          (powerline-raw erc-modified-channels-object face1 'l))
                        (powerline-major-mode face1 'l)
                        (powerline-process face1)
                        (powerline-minor-modes face1 'l)
                        (powerline-narrow face1 'l)
                        (powerline-raw " " face1)
                        (funcall separator-left face1 face2)
                        (powerline-vc face2 'r)
                        (when (bound-and-true-p nyan-mode)
                          (powerline-raw (list (nyan-create)) face2 'l))))
             (rhs (list (powerline-raw global-mode-string face2 'r)
                        (funcall separator-right face2 face1)
                        (unless window-system
                          (powerline-raw (char-to-string #xe0a1) face1 'l))
                        (powerline-raw "%4l" face1 'l)
                        (powerline-raw ":" face1 'l)
                        (powerline-raw "%3c" face1 'r)
                        (funcall separator-right face1 face0)
                        (powerline-raw " " face0)
                        (powerline-raw "%p" face0 'r)
                        (when powerline-display-hud
                          (powerline-hud face0 face2-hud))
                        (powerline-fill face0 0)
                        )))
        (concat (powerline-render lhs)
                (powerline-fill face2 (powerline-width rhs))
                (powerline-render rhs)))))))

(provide 'powerline-default-evil-theme)

;;; powerline-default-evil-theme.el ends here
