;;; lucius-theme.el --- a vim-lucius port for emacs
;;;
;;; Author: Alex Peitsinis <alexpeitsinis@gmail.com>
;;; Url: https://github.com/alexpeits/emacs-lucius-theme
;;; Version: 20190222.0
;;;
;;; Changelog :
;;;
;;; 20190222.0: Initial version
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

(deftheme lucius
  "lucius theme - a vim-lucius port for emacs")

(custom-theme-set-faces
 'lucius

 '(cursor              ((t (:background "#99d1ce"))))
 '(default             ((t (:foreground "#CFE0DA" :background "#1b1b1c"))))
 '(linum               ((t (:foreground "#53676b" :background "#1e2222"))))
 '(minibuffer-prompt   ((t (:foreground "#12A89F" :weight bold))))
 '(escape-glyph        ((t (:foreground "orange"))))
 '(highlight           ((t (:background "#283438"))))
 '(hl-line             ((t (:background "#252a2a"))))
 '(region              ((t (:background "#004870"))))
 '(fringe              ((t (:background "#232828"))))
 '(shadow              ((t (:foreground "#808080"))))
 '(secondary-selection ((t (:background "#132125"))))
 '(trailing-whitespace ((t (:background "#C74000"))))
 '(show-paren-match    ((t (:background "#456665" :weight bold))))

 '(mode-line-highlight ((t (:box nil))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line           ((t (:foreground "#C1EAE2" :background "#005555" :box (:line-width 1 :color "#005555")))))
 '(mode-line-inactive  ((t (:foreground "#387A71" :background "#1e2827" :box (:line-width 1 :color "#004040")))))

 `(font-lock-comment-face              ((t (:foreground "#808080" :background nil :slant italic))))
 `(font-lock-comment-delimiter-face    ((t (:inherit font-lock-comment-face))))
 `(font-lock-constant-face             ((t (:foreground "#87D7FF" :background nil))))
 `(font-lock-builtin-face              ((t (:foreground "#87D7FF" :background nil))))
 `(font-lock-function-name-face        ((t (:foreground "#a7d37b" :background nil))))
 `(font-lock-variable-name-face        ((t (:foreground "#87D7FF" :background nil))))
 `(font-lock-keyword-face              ((t (:foreground "#2aa198" :background nil))))
 `(font-lock-string-face               ((t (:foreground "#d7d7af" :background nil))))
 `(font-lock-doc-string-face           ((t (:inherit font-lock-string-face))))
 `(font-lock-doc-face                  ((t (:inherit font-lock-string-face))))
 `(font-lock-type-face                 ((t (:foreground "#87D7D7" :background nil))))
 '(font-lock-negation-char-face        ((t (:foreground "#CB5A37" :background nil))))
 '(font-lock-preprocessor-face         ((t (:foreground "#CB5A37" :background nil))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#f66500" :background nil))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "red" :background nil))))
 '(font-lock-warning-face              ((t (:foreground "salmon" :background nil))))

 '(vertical-border     ((t (:foreground "#758A86"))))
 '(persp-selected-face ((t (:inherit font-lock-function-name-face :weight bold))))
 '(isearch-fail        ((t (:foreground "#3f4758" :background "salmon"))))
 '(isearch             ((t (:inverse-video nil :foreground "black" :background "#db7e4c"))))
 '(lazy-highlight      ((t (:foreground "#a0a8b0" :background "#3d464f"))))

 `(haskell-pragma-face    ((t (:foreground "#859900" :slant italic))))
 `(purescript-pragma-face ((t (:foreground "#859900" :slant italic))))

 '(markdown-code-face ((t (:inherit font-lock-keyword-face))))
 '(markdown-pre-face  ((t (:inherit font-lock-keyword-face))))

 `(diff-hl-insert ((t (:background "#183918" :foreground "#4c934c"))))
 `(diff-hl-change ((t (:background "#1a2d4c" :foreground "#298ed5"))))
 `(diff-hl-delete ((t (:background "#4c1515" :foreground "#bc4d4d"))))

 `(diff-added   ((t (:foreground "#859900"))))
 `(diff-changed ((t (:foreground "#298ed5"))))
 `(diff-removed ((t (:foreground "#dc322f"))))

 `(ivy-grep-info ((t (:foreground "#afd787" :weight bold))))

 `(header-line ((t (:foreground "#a0b5b5" :background "#304545"))))

 `(powerline-active0   ((t (:background "#005252" :foreground "#C1EAE2"))))
 `(powerline-active1   ((t (:background "#003F3F" :foreground "#C1EAE2"))))
 `(powerline-active2   ((t (:background "#002B2B" :foreground "#C1EAE2"))))
 `(powerline-inactive0 ((t (:background "#0e2626" :foreground "#387A71"))))
 `(powerline-inactive1 ((t (:background "#0e2626" :foreground "#387A71"))))
 `(powerline-inactive2 ((t (:background "#0e2626" :foreground "#387A71"))))

 '(org-block            ((t (:foreground "#bbccc5" :background "#191e1d"))))
 '(org-block-begin-line ((t (:foreground "#768e84" :background "#1e2623" :slant italic))))
 '(org-block-end-line   ((t (:inherit org-block-begin-line))))

 '(outline-1 ((t (:foreground "#AFD787"))))
 '(outline-2 ((t (:foreground "#2aa198"))))
 '(outline-3 ((t (:foreground "#62B49F"))))
 '(outline-4 ((t (:foreground "#87D7FF"))))
 '(outline-5 ((t (:foreground "#AFD787"))))
 '(outline-6 ((t (:foreground "#2aa198"))))

 '(company-tooltip                      ((t (:foreground "#cfe0da" :background "#252525"))))
 '(company-tooltip-annotation           ((t (:foreground "#47bcb3"))))
 '(company-tooltip-annotation-selection ((t (:foreground "#47bcb3"))))
 '(company-tooltip-common               ((t (:foreground "#cfe0da"))))
 '(company-tooltip-common-selection     ((t (:weight bold))))
 '(company-tooltip-mouse                ((t (:background "#47bcb3" :foreground "#74e0d8"))))
 '(company-tooltip-search               ((t (:foreground "#d33682"))))
 '(company-tooltip-search-selection     ((t (:foreground "#d33682" :weight bold))))
 '(company-tooltip-selection            ((t (:background "#353535" :weight bold))))
 '(company-scrollbar-bg                 ((t (:background "#252525" :foreground "#47bcb3"))))
 '(company-scrollbar-fg                 ((t (:foreground "#1b1b1b" :background "#47bcb3"))))

 '(term-color-black   ((t (:foreground "#1b1b1c"))))
 '(term-color-red     ((t (:foreground "#CB5A37"))))
 '(term-color-green   ((t (:foreground "#859900"))))
 '(term-color-yellow  ((t (:foreground "#D09F07"))))
 '(term-color-blue    ((t (:foreground "#268bd2"))))
 '(term-color-magenta ((t (:foreground "#d33682"))))
 '(term-color-cyan    ((t (:foreground "#47bcb3"))))
 '(term-color-white   ((t (:foreground "#CFE0DA"))))

 )

(custom-theme-set-variables
 'lucius
 '(ansi-color-names-vector
   ["#1b1b1c" "#CB5A37" "#859900" "#D09F07" "#268bd2" "#d33682" "#47bcb3" "#CFE0DA"]))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'lucius)

;;; lucius-theme.el ends here
