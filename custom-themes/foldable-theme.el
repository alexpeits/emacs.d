;;; foldable-theme --- Default light emacs theme with a twist

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
;;;
;;; Code:

(deftheme foldable
  "Default light emacs theme with a twist")

(defvar foldable-theme-hues
  ;;         foreground    org-block   hl-line     org-block   fci
  '((white . ("white"      "grey99"    "grey96"    "grey92"    "grey85"))
    (tan .   ("#fbf4e8"    "#f5efe3"   "#f2eadc"   "#efe6d5"   "#e7dbc2"))
    (cyan .  ("honeydew"   "#ecfaec"   "#e7f6e7"   "#d9f1d9"   "#d0dfd0"))))

(defvar foldable-theme-variant 'white)

(let* ((hues (cdr (assoc (or foldable-theme-variant 'white) foldable-theme-hues)))
       (h1 (elt hues 0))
       (h2 (elt hues 1))
       (h3 (elt hues 2))
       (h4 (elt hues 3))
       (h5 (elt hues 4)))
  (custom-theme-set-faces
   'foldable
   `(default             ((t (:background ,h1 :foreground "Black"))))
   `(fringe              ((t (:background ,h3))))
   `(hl-line             ((t (:background ,h3))))
   '(whitespace-trailing ((t (:background "#ffcdcd"))))
   '(region              ((t (:background "#b1d8e6"))))
   '(fixed-pitch         ((t nil)))
   '(fixed-pitch-serif   ((t nil)))
   '(secondary-selection ((t (:background "lemon chiffon"))))

   `(fill-column-indicator ((t (:foreground ,h5))))

   '(line-number              ((t (:inherit default :inherit shadow))))
   `(line-number-current-line ((t (:inherit line-number :background ,h4))))

   `(org-block            ((t (:background ,h2))))
   `(org-block-begin-line ((t (:background ,h4))))
   '(org-block-end-line   ((t (:inherit org-block-begin-line))))
   '(org-hide             ((t (:inherit fill-column-indicator))))

   '(hi-green-b ((t (:background "#a0f0cf" :foreground "#005040" :weight bold))))
   '(hi-blue-b  ((t (:background "#bedef9" :foreground "#324f80" :weight bold))))
   '(hi-red-b   ((t (:background "#ffccbb" :foreground "#840040" :weight bold))))
   '(hi-black-b ((t (:background "#f5d88f" :foreground "#782900" :weight bold))))

   '(dired-marked  ((t (:inherit hi-black-b))))
   '(dired-flagged ((t (:inherit hi-red-b))))

   `(diff-hl-insert ((t (:background "#c0e7bb" :foreground "#235323"))))
   `(diff-hl-change ((t (:background "#bedef9" :foreground "#324f80"))))
   `(diff-hl-delete ((t (:background "#ffcdcd" :foreground "#632121"))))

   '(coq-cheat-face  ((t (:inherit hi-red-b))))

   '(flycheck-warning        ((t (:underline (:color "orange1" :style wave)))))
   '(flycheck-fringe-warning ((t (:foreground "orange1"))))
   '(flycheck-error          ((t (:underline (:color "red1" :style wave)))))
   '(flycheck-fringe-error   ((t (:foreground "red1"))))
   '(flycheck-info           ((t (:underline (:color "DeepSkyBlue2" :style wave)))))
   '(flycheck-fringe-info    ((t (:foreground "DeepSkyBlue2"))))

   '(persp-selected-face ((t :inherit font-lock-keyword-face :weight bold)))

   '(powerline-active0   ((t (:background "grey64" :foreground "black"))))
   '(powerline-active1   ((t (:background "grey72" :foreground "black"))))
   '(powerline-active2   ((t (:background "grey85" :foreground "black"))))
   '(powerline-inactive0 ((t (:background "grey90" :foreground "grey50" :weight normal))))
   '(powerline-inactive1 ((t (:background "grey90" :foreground "grey50" :weight normal))))
   '(powerline-inactive2 ((t (:background "grey90" :foreground "grey50" :weight normal))))

   '(sh-heredoc ((t (:inherit font-lock-type-face))))

   `(Info-quoted ((t (:inherit font-lock-type-face))))

   `(rst-block     ((t (:inherit font-lock-doc-face))))
   `(rst-literal   ((t (:inherit font-lock-constant-face))))
   `(rst-adornment ((t (:inherit font-lock-keyword-face :weight bold))))
   `(rst-level-1   ((t (:inherit font-lock-keyword-face :weight bold))))
   `(rst-level-2   ((t (:inherit rst-level-1))))
   `(rst-level-3   ((t (:inherit rst-level-1))))
   `(rst-level-4   ((t (:inherit rst-level-1))))
   `(rst-level-5   ((t (:inherit rst-level-1))))
   `(rst-level-6   ((t (:inherit rst-level-1))))

   `(markup-gen-face        ((t (:inherit font-lock-string-face))))
   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(markup-verbatim-face   ((t (:inherit font-lock-constant-face))))
   `(markup-meta-face       ((t (:inherit markup-gen-face))))
   `(markup-list-face       ((t (:inherit font-lock-function-name-face))))
   `(markup-meta-hide-face  ((t (:inherit markup-meta-face))))
   `(markup-title-0-face    ((t (:inherit font-lock-preprocessor-face :weight bold))))
   `(markup-title-1-face    ((t (:inherit markup-title-0-face))))
   `(markup-title-2-face    ((t (:inherit markup-title-0-face))))
   `(markup-title-3-face    ((t (:inherit markup-title-0-face))))
   `(markup-title-4-face    ((t (:inherit markup-title-0-face))))
   `(markup-title-5-face    ((t (:inherit markup-title-0-face))))

   ))

(custom-theme-set-variables
 'foldable
 '(ibuffer-deletion-face 'dired-flagged)
 '(ibuffer-filter-group-name-face 'dired-mark)
 '(ibuffer-marked-face 'dired-marked)
 '(ibuffer-title-face 'dired-header)
 '(coq-highlighted-hyps-bg "grey")
 )

(provide-theme 'foldable)
;;; foldable-theme ends here
