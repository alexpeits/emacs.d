;;; wombat-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2011-2017 Free Software Foundation, Inc.

;; Author: Kristoffer Grönlund <krig@koru.se>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme vscode
  "")

(let ((class '((class color) (min-colors 89)))
      (black   "#1e1e1e")
      (grey01  "#252526")
      (grey02  "#292929")
      (grey03  "#383b3d")
      (grey04  "#3a3d41")
      (grey05  "#404040")
      (grey06  "#707070")
      (grey07  "#808080")
      (grey08  "#a6a6a6")
      (grey09  "#bbbbbb")
      (grey10  "#cccccc")
      (white   "#d4d4d4")
      (lyellow "#dcdcaa")
      (yellow  "#d7ba7d")
      (orange  "#ce9178")
      (lred    "#d16969")
      (red     "#f44747")
      (pink    "#c586c0")
      (purple  "#646695")
      (dblue   "#000080")
      (blue01  "#007acc")
      (blue02  "#6796e6")
      (blue03  "#569cd6")
      (lblue   "#9ccfed")
      (cyan    "#4ec9b0")
      (lgreen  "#b5cea8")
      (green   "#6a9955")
      (bgreen  "#77c653")
      )
  (custom-theme-set-faces
   'vscode
   `(default ((,class (:background ,black :foreground ,white))))
   `(cursor ((,class (:background ,grey10))))
   ;; Highlighting faces
   `(fringe ((,class (:background ,grey02))))
   `(vertical-border ((t (:foreground "#b0b0b0"))))
   `(highlight ((,class (:background ,grey05))))
   `(region ((t (:background "#253b76"))))
   `(hl-line ((t (:background ,grey02))))
   ;; `(secondary-selection ((,class (:background "#333366" :foreground "#f6f3e8"))))
   `(isearch ((t (:foreground "#242424" :background "#e5786d"))))
   `(lazy-highlight ((,class (:background "#384048" :foreground "#a0a8b0"))))
   ;; Mode line faces
   `(mode-line
     ((t (:background "#454545" :foreground "#d1d1d1" :box (:line-width 1 :color "#4a4a4a")))))
   `(mode-line-inactive
     ((t (:inherit mode-line :background "#282828" :foreground "#858585"))))
   `(persp-selected-face ((,class (:foreground ,lblue :weight bold))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,orange))))
   `(escape-glyph ((,class (:foreground ,yellow :weight bold))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,cyan))))
   `(font-lock-comment-face ((,class (:foreground ,green))))
   `(font-lock-constant-face ((,class (:foreground ,lblue))))
   `(font-lock-function-name-face ((,class (:foreground ,lyellow))))
   `(font-lock-keyword-face ((,class (:foreground ,pink))))
   `(font-lock-string-face ((,class (:foreground ,orange))))
   `(font-lock-type-face ((,class (:foreground ,blue03))))
   `(font-lock-variable-name-face ((,class (:foreground ,lgreen))))
   `(font-lock-warning-face ((,class (:foreground ,yellow))))
   `(haskell-constructor-face ((,class (:foreground ,lblue))))
   ;; `(haskell-constructor-face ((,class (:inherit default))))
   `(haskell-operator-face ((,class (:foreground ,bgreen))))
   `(purescript-constructor-face ((,class (:foreground ,lblue))))
   ;; `(purescript-constructor-face ((,class (:inherit default))))
   `(purescript-operator-face ((,class (:foreground ,bgreen))))
   ;; Button and link faces
   ;; `(link ((,class (:foreground "#8ac6f2" :underline t))))
   ;; `(link-visited ((,class (:foreground "#e5786d" :underline t))))
   ;; `(button ((,class (:background "#333333" :foreground "#f6f3e8"))))
   ;; `(header-line ((,class (:background "#303030" :foreground "#e7f6da"))))
   ;; Gnus faces
   ;; Message faces
   ;; `(message-header-name ((,class (:foreground "#8ac6f2" :weight bold))))
   ;; `(message-header-cc ((,class (:foreground "#95e454"))))
   ;; `(message-header-other ((,class (:foreground "#95e454"))))
   ;; `(message-header-subject ((,class (:foreground "#cae682"))))
   ;; `(message-header-to ((,class (:foreground "#cae682"))))
   ;; `(message-cited-text ((,class (:foreground "#99968b"))))
   ;; `(message-separator ((,class (:foreground "#e5786d" :weight bold))))
   ))

(provide-theme 'vscode)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; wombat-theme.el ends here
