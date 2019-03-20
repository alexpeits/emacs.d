;;; vscode-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2011-2017 Free Software Foundation, Inc.

;; Author: Alex Peitsinis <alexpeitsinis@gmail.com>

;;; Commentary:

;;; Code:

(deftheme vscode
  "Dark+ theme from Visual Studio Code")

(let ((class '((class color) (min-colors 89)))
      (black   "#1c1c1c")
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
      (lcyan   "#87d7d7")
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
   `(vertical-border ((t (:foreground "#a0a0a0"))))
   `(highlight ((,class (:background ,grey05))))
   `(region ((t (:background "#253b76"))))
   `(hl-line ((t (:background ,grey02))))
   `(isearch ((t (:foreground "#242424" :background "#e5786d"))))
   `(lazy-highlight ((,class (:background "#384048" :foreground "#a0a8b0"))))
   ;; Mode line faces
   `(mode-line
     ((t (:background "#454545" :foreground "#d1d1d1" :box (:line-width 1 :color "#454545")))))
   `(mode-line-inactive
     ((t (:inherit mode-line :background "#282828" :foreground "#858585" :box (:line-width 1 :color "#353535")))))
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
   `(haskell-operator-face ((,class (:foreground ,lcyan))))
   `(purescript-constructor-face ((,class (:foreground ,lblue))))
   `(purescript-operator-face ((,class (:foreground ,lcyan))))
   ))

(provide-theme 'vscode)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; vscode-theme.el ends here
