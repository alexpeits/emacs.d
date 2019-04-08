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
   `(persp-selected-face ((,class (:foreground ,cyan :weight bold))))
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

   '(show-paren-match    ((t (:background "#2f5068" :weight bold))))

   `(ivy-grep-info ((t (:foreground ,bgreen :weight bold))))
   `(ag-hit-face   ((t (:foreground ,bgreen :weight bold))))

   `(diff-hl-insert ((t (:background "#183918" :foreground "#4c934c"))))
   `(diff-hl-change ((t (:background "#1a2d4c" :foreground "#298ed5"))))
   `(diff-hl-delete ((t (:background "#4c1515" :foreground "#bc4d4d"))))

   `(diff-added   ((t (:foreground "#859900"))))
   `(diff-changed ((t (:foreground "#298ed5"))))
   `(diff-removed ((t (:foreground "#dc322f"))))

   `(header-line ((t (:foreground "#a0b5b5" :background "#304545"))))

   `(markdown-code-face     ((t (:inherit font-lock-string-face))))
   `(markdown-pre-face      ((t (:inherit font-lock-string-face))))

   ;; TODO: all of the below

   `(company-preview ((t (:inherit default :background "#304540"))))
   `(company-preview-common ((t (:inherit company-preview :slant italic))))
   `(company-tooltip                      ((t (:foreground "#cfe0da" :background "#252525"))))
   `(company-tooltip-annotation           ((t (:foreground ,cyan))))
   `(company-tooltip-annotation-selection ((t (:foreground ,cyan))))
   `(company-tooltip-common               ((t (:foreground "#cfe0da"))))
   `(company-tooltip-common-selection     ((t (:weight bold))))
   `(company-tooltip-mouse                ((t (:background ,cyan :foreground ,green))))
   `(company-tooltip-search               ((t (:foreground "#d33682"))))
   `(company-tooltip-search-selection     ((t (:foreground "#d33682" :weight bold))))
   `(company-tooltip-selection            ((t (:background "#353535" :weight bold))))
   `(company-scrollbar-bg                 ((t (:background "#252525" :foreground ,cyan))))
   `(company-scrollbar-fg                 ((t (:foreground "#1b1b1b" :background ,cyan))))

   `(term-color-black   ((t (:foreground "#1b1b1c"))))
   `(term-color-red     ((t (:foreground "#cb5a37"))))
   `(term-color-green   ((t (:foreground "#859900"))))
   `(term-color-yellow  ((t (:foreground "#d09f07"))))
   `(term-color-blue    ((t (:foreground "#268bd2"))))
   `(term-color-magenta ((t (:foreground "#d33682"))))
   `(term-color-cyan    ((t (:foreground "#47bcb3"))))
   `(term-color-white   ((t (:foreground "#cfe0da"))))
   ))

(custom-theme-set-variables
 'vscode
 '(ansi-color-names-vector
   ["#1b1b1c" "#cb5a37" "#859900" "#d09f07" "#268bd2" "#d33682" "#47bcb3" "#cfe0da"]))

(provide-theme 'vscode)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; vscode-theme.el ends here
