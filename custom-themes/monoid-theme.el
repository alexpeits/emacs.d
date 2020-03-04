;;; monoid-theme --- A light theme inspired by vim-lucius

;; Copyright (C) 2018 Alex Peitsinis

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; TODO
;;
;;; Code:

(deftheme monoid
  "A light theme inspired by vim-lucius")

(defvar monoid-theme-hues
;;            foreground   org-block   hl-line     org-block   fci
  '((white . ("white"      "grey97"    "grey96"    "grey90"    "grey85"))
    (tan .   ("#fbf4e8"    "#f3ede1"   "#f2eadc"   "#efe6d5"   "#e7dbc2"))
    (cyan .  ("honeydew"   "#e9f7e9"   "#e7f6e7"   "#d6eed6"   "#d0dfd0"))))

(defvar monoid-theme-variant 'white)

(let* ((hues (cdr (assoc (or monoid-theme-variant 'white) monoid-theme-hues)))
       (h1 (elt hues 0))
       (h2 (elt hues 1))
       (h3 (elt hues 2))
       (h4 (elt hues 3))
       (h5 (elt hues 4)))
  (custom-theme-set-faces
   'monoid
   `(default ((t (:background ,h1 :foreground "Black"))))
   `(fringe ((t (:background ,h3))))
   `(hl-line ((t (:background ,h3))))
   '(whitespace-trailing ((t (:background "#602020"))))
   '(region ((t (:background "#b1d8e6"))))
   '(show-paren-match ((t (:background "#A3CFD5"))))
   '(fixed-pitch ((t nil)))
   '(minibuffer-prompt ((t (:inherit font-lock-function-name-face :weight bold))))

   `(fill-column-indicator ((t (:foreground ,h5))))
   '(line-number ((t (:inherit default :inherit shadow))))
   `(line-number-current-line ((t (:inherit line-number :background ,h4))))

   '(font-lock-preprocessor-face      ((t (:foreground "#870087"))))
   `(font-lock-comment-face           ((t (:foreground "#757a7a" :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-function-name-face     ((t (:foreground "#008787"))))
   `(font-lock-builtin-face           ((t (:foreground "#00875f"))))
   `(font-lock-constant-face          ((t (:inherit font-lock-builtin-face))))
   `(font-lock-keyword-face           ((t (:foreground "#008c00"))))
   `(font-lock-string-face            ((t (:foreground "#af5f00"))))
   `(font-lock-doc-face               ((t (:inherit font-lock-string-face))))
   `(font-lock-type-face              ((t (:foreground "#005f90"))))
   `(font-lock-variable-name-face     ((t (:inherit font-lock-builtin-face))))

   '(tuareg-font-lock-governing-face ((t (:foreground "#005f90" :weight bold))))

   `(org-block ((t (:background ,h2))))
   `(org-block-begin-line ((t (:background ,h4))))
   '(org-block-end-line ((t (:inherit org-block-begin-line))))

   `(diff-hl-insert ((t (:background "#c0e7bb" :foreground "#235323"))))
   `(diff-hl-change ((t (:background "#bedef9" :foreground "#324f80"))))
   `(diff-hl-delete ((t (:background "#ffcdcd" :foreground "#632121"))))

   '(flycheck-warning ((t (:underline (:color "orange1" :style wave)))))
   '(flycheck-fringe-warning ((t (:foreground "orange1"))))
   '(flycheck-error ((t (:underline (:color "red1" :style wave)))))
   '(flycheck-fringe-error ((t (:foreground "red1"))))
   '(flycheck-info ((t (:underline (:color "DeepSkyBlue2" :style wave)))))
   '(flycheck-fringe-info ((t (:foreground "DeepSkyBlue2"))))

   '(persp-selected-face ((t :inherit font-lock-string-face :weight bold)))

   '(powerline-active0   ((t (:background "grey62" :foreground "black"))))
   '(powerline-active1   ((t (:background "grey72" :foreground "black"))))
   '(powerline-active2   ((t (:background "grey85" :foreground "black"))))
   '(powerline-inactive0 ((t (:background "grey90" :foreground "grey50" :weight normal))))
   '(powerline-inactive1 ((t (:background "grey90" :foreground "grey50" :weight normal))))
   '(powerline-inactive2 ((t (:background "grey90" :foreground "grey50" :weight normal))))

   ))

(provide-theme 'monoid)
;;; monoid-theme ends here
