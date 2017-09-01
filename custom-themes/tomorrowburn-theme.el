;;; tomorrowburn-theme.el --- A low contrast color theme for Emacs.

;; Copyright (C) 2011-2017 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://github.com/bbatsov/tomorrowburn-emacs
;; Package-Version: 20170511.1337
;; Version: 2.5

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

;; A port of the popular Vim theme Tomorrowburn for Emacs 24+, built on top
;; of the new built-in theme support in Emacs 24.

;;; Credits:

;; Jani Nurminen created the original theme for vim on which this port
;; is based.

;;; Code:

(deftheme tomorrowburn "The Tomorrowburn color theme")

;;; Color Palette

(defvar tomorrowburn-default-colors-alist
  '(("tomorrowburn-fg+1"     . "#FFFFEF")
    ("tomorrowburn-fg"       . "#DCDCCC")
    ("tomorrowburn-fg-1"     . "#656555")
    ("tomorrowburn-bg-2"     . "#000000")
    ("tomorrowburn-bg-1"     . "#2B2B2B")
    ("tomorrowburn-bg-05"    . "#383838")
    ("tomorrowburn-bg"       . "#1D1F21")
    ("tomorrowburn-bg+05"    . "#494949")
    ("tomorrowburn-bg+1"     . "#4F4F4F")
    ("tomorrowburn-bg+2"     . "#5F5F5F")
    ("tomorrowburn-bg+3"     . "#6F6F6F")
    ("tomorrowburn-red+1"    . "#B5BD68")
    ("tomorrowburn-red"      . "#B5BD68")
    ("tomorrowburn-red-1"    . "#B5BD68")
    ("tomorrowburn-red-2"    . "#B5BD68")
    ("tomorrowburn-red-3"    . "#B5BD68")
    ("tomorrowburn-red-4"    . "#B5BD68")
    ("tomorrowburn-orange"   . "#DFAF8F")
    ("tomorrowburn-yellow"   . "#B294BB")
    ("tomorrowburn-yellow-1" . "#B294BB")
    ("tomorrowburn-yellow-2" . "#B294BB")
    ("tomorrowburn-green-1"  . "#999998")
    ("tomorrowburn-green"    . "#999998")
    ("tomorrowburn-green+1"  . "#999998")
    ("tomorrowburn-green+2"  . "#999998")
    ("tomorrowburn-green+3"  . "#999998")
    ("tomorrowburn-green+4"  . "#999998")
    ("tomorrowburn-cyan"     . "#81A2BE")
    ("tomorrowburn-blue+1"   . "#F0C674")
    ("tomorrowburn-blue"     . "#F0C674")
    ("tomorrowburn-blue-1"   . "#F0C674")
    ("tomorrowburn-blue-2"   . "#F0C674")
    ("tomorrowburn-blue-3"   . "#F0C674")
    ("tomorrowburn-blue-4"   . "#F0C674")
    ("tomorrowburn-blue-5"   . "#F0C674")
    ("tomorrowburn-magenta"  . "#DC8CC3"))
  "List of Tomorrowburn colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defvar tomorrowburn-override-colors-alist
  '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist before loading the theme.")

(defvar tomorrowburn-colors-alist
  (append tomorrowburn-default-colors-alist tomorrowburn-override-colors-alist))

(defmacro tomorrowburn-with-color-variables (&rest body)
  "`let' bind all colors defined in `tomorrowburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   tomorrowburn-colors-alist))
     ,@body))

;;; Theme Faces
(tomorrowburn-with-color-variables
  (custom-theme-set-faces
   'tomorrowburn
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,tomorrowburn-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,tomorrowburn-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-bg))))
   `(cursor ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-fg+1))))
   `(escape-glyph ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(fringe ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-bg+1))))
   `(header-line ((t (:foreground ,tomorrowburn-yellow
                                  :background ,tomorrowburn-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,tomorrowburn-bg-05))))
   `(success ((t (:foreground ,tomorrowburn-green :weight bold))))
   `(warning ((t (:foreground ,tomorrowburn-orange :weight bold))))
   `(tooltip ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,tomorrowburn-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,tomorrowburn-green))))
   `(compilation-error-face ((t (:foreground ,tomorrowburn-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,tomorrowburn-fg))))
   `(compilation-info-face ((t (:foreground ,tomorrowburn-blue))))
   `(compilation-info ((t (:foreground ,tomorrowburn-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,tomorrowburn-green))))
   `(compilation-line-face ((t (:foreground ,tomorrowburn-yellow))))
   `(compilation-line-number ((t (:foreground ,tomorrowburn-yellow))))
   `(compilation-message-face ((t (:foreground ,tomorrowburn-blue))))
   `(compilation-warning-face ((t (:foreground ,tomorrowburn-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,tomorrowburn-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,tomorrowburn-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,tomorrowburn-yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,tomorrowburn-fg-1))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,tomorrowburn-fg))))
   `(grep-error-face ((t (:foreground ,tomorrowburn-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,tomorrowburn-blue))))
   `(grep-match-face ((t (:foreground ,tomorrowburn-orange :weight bold))))
   `(match ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-orange :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,tomorrowburn-cyan    :foreground ,tomorrowburn-bg-1))))
   `(hi-green   ((t (:background ,tomorrowburn-green+4 :foreground ,tomorrowburn-bg-1))))
   `(hi-pink    ((t (:background ,tomorrowburn-magenta :foreground ,tomorrowburn-bg-1))))
   `(hi-yellow  ((t (:background ,tomorrowburn-yellow  :foreground ,tomorrowburn-bg-1))))
   `(hi-blue-b  ((t (:foreground ,tomorrowburn-blue    :weight     bold))))
   `(hi-green-b ((t (:foreground ,tomorrowburn-green+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,tomorrowburn-red     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,tomorrowburn-yellow-2 :weight bold :background ,tomorrowburn-bg+2))))
   `(isearch-fail ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-red-4))))
   `(lazy-highlight ((t (:foreground ,tomorrowburn-yellow-2 :weight bold :background ,tomorrowburn-bg-05))))

   `(menu ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-bg))))
   `(minibuffer-prompt ((t (:foreground ,tomorrowburn-yellow))))
   `(mode-line
     ((,class (:foreground ,tomorrowburn-green+1
                           :background ,tomorrowburn-bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,tomorrowburn-green-1
                      :background ,tomorrowburn-bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,tomorrowburn-bg-1))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,tomorrowburn-bg+2))))
   `(trailing-whitespace ((t (:background ,tomorrowburn-red))))
   `(vertical-border ((t (:foreground ,tomorrowburn-fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,tomorrowburn-fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,tomorrowburn-green))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,tomorrowburn-green-1))))
   `(font-lock-constant-face ((t (:foreground ,tomorrowburn-green+4))))
   `(font-lock-doc-face ((t (:foreground ,tomorrowburn-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,tomorrowburn-cyan))))
   `(font-lock-keyword-face ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,tomorrowburn-blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,tomorrowburn-green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,tomorrowburn-red))))
   `(font-lock-type-face ((t (:foreground ,tomorrowburn-blue-1))))
   `(font-lock-variable-name-face ((t (:foreground ,tomorrowburn-orange))))
   `(font-lock-warning-face ((t (:foreground ,tomorrowburn-yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,tomorrowburn-fg))))
   `(newsticker-default-face ((t (:foreground ,tomorrowburn-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,tomorrowburn-green+3))))
   `(newsticker-extra-face ((t (:foreground ,tomorrowburn-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,tomorrowburn-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,tomorrowburn-green))))
   `(newsticker-new-item-face ((t (:foreground ,tomorrowburn-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,tomorrowburn-red))))
   `(newsticker-old-item-face ((t (:foreground ,tomorrowburn-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,tomorrowburn-fg))))
   `(newsticker-treeview-face ((t (:foreground ,tomorrowburn-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,tomorrowburn-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,tomorrowburn-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,tomorrowburn-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,tomorrowburn-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,tomorrowburn-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-yellow))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,tomorrowburn-fg-1 :background ,tomorrowburn-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,tomorrowburn-green+2 :background ,tomorrowburn-bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,tomorrowburn-fg-1 :background ,tomorrowburn-bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,tomorrowburn-green+1))))
   `(android-mode-error-face ((t (:foreground ,tomorrowburn-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,tomorrowburn-fg))))
   `(android-mode-verbose-face ((t (:foreground ,tomorrowburn-green))))
   `(android-mode-warning-face ((t (:foreground ,tomorrowburn-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,tomorrowburn-cyan :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,tomorrowburn-red :weight bold))))
   `(anzu-match-1 ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-green))))
   `(anzu-match-2 ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-orange))))
   `(anzu-match-3 ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-blue))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,tomorrowburn-yellow))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,tomorrowburn-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,tomorrowburn-yellow))))
   `(font-latex-italic-face ((t (:foreground ,tomorrowburn-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,tomorrowburn-orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,tomorrowburn-red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,tomorrowburn-orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,tomorrowburn-blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,tomorrowburn-fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,tomorrowburn-fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,tomorrowburn-blue))))
   `(agda2-highlight-function-face ((t (:foreground ,tomorrowburn-blue))))
   `(agda2-highlight-module-face ((t (:foreground ,tomorrowburn-blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,tomorrowburn-red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,tomorrowburn-bg+3 :foreground ,tomorrowburn-bg-2))))
   `(ac-selection-face ((t (:background ,tomorrowburn-blue-4 :foreground ,tomorrowburn-fg))))
   `(popup-tip-face ((t (:background ,tomorrowburn-yellow-2 :foreground ,tomorrowburn-bg-2))))
   `(popup-menu-mouse-face ((t (:background ,tomorrowburn-yellow-2 :foreground ,tomorrowburn-bg-2))))
   `(popup-summary-face ((t (:background ,tomorrowburn-bg+3 :foreground ,tomorrowburn-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,tomorrowburn-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,tomorrowburn-bg-1))))
   `(popup-isearch-match ((t (:background ,tomorrowburn-bg :foreground ,tomorrowburn-fg))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,tomorrowburn-fg-1 :background ,tomorrowburn-bg :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,tomorrowburn-green+3 :background ,tomorrowburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,tomorrowburn-yellow :background ,tomorrowburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,tomorrowburn-red+1 :background ,tomorrowburn-bg :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,tomorrowburn-cyan :background ,tomorrowburn-bg :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,tomorrowburn-orange :background ,tomorrowburn-bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,tomorrowburn-orange :background ,tomorrowburn-bg-1))))
   `(company-tooltip-selection ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-bg-1))))
   `(company-tooltip-mouse ((t (:background ,tomorrowburn-bg-1))))
   `(company-tooltip-common ((t (:foreground ,tomorrowburn-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,tomorrowburn-green+2))))
   `(company-scrollbar-fg ((t (:background ,tomorrowburn-bg-1))))
   `(company-scrollbar-bg ((t (:background ,tomorrowburn-bg+2))))
   `(company-preview ((t (:background ,tomorrowburn-green+2))))
   `(company-preview-common ((t (:foreground ,tomorrowburn-green+2 :background ,tomorrowburn-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,tomorrowburn-yellow-1 :foreground ,tomorrowburn-bg))))
   `(bm-fringe-face ((t (:background ,tomorrowburn-yellow-1 :foreground ,tomorrowburn-bg))))
   `(bm-fringe-persistent-face ((t (:background ,tomorrowburn-green-1 :foreground ,tomorrowburn-bg))))
   `(bm-persistent-face ((t (:background ,tomorrowburn-green-1 :foreground ,tomorrowburn-bg))))
;;;;; calfw
   `(cfw:face-annotation ((t (:foreground ,tomorrowburn-red :inherit cfw:face-day-title))))
   `(cfw:face-day-title ((t nil)))
   `(cfw:face-default-content ((t (:foreground ,tomorrowburn-green))))
   `(cfw:face-default-day ((t (:weight bold))))
   `(cfw:face-disable ((t (:foreground ,tomorrowburn-fg-1))))
   `(cfw:face-grid ((t (:inherit shadow))))
   `(cfw:face-header ((t (:inherit font-lock-keyword-face))))
   `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))
   `(cfw:face-periods ((t (:foreground ,tomorrowburn-cyan))))
   `(cfw:face-saturday ((t (:foreground ,tomorrowburn-blue :weight bold))))
   `(cfw:face-select ((t (:background ,tomorrowburn-blue-5))))
   `(cfw:face-sunday ((t (:foreground ,tomorrowburn-red :weight bold))))
   `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
   `(cfw:face-today ((t (:foreground ,tomorrowburn-cyan :weight bold))))
   `(cfw:face-today-title ((t (:inherit highlight bold))))
   `(cfw:face-toolbar ((t (:background ,tomorrowburn-blue-5))))
   `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))
   `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,tomorrowburn-orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,tomorrowburn-green+1))))
   `(cider-deprecated-face ((t (:background ,tomorrowburn-yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,tomorrowburn-red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,tomorrowburn-cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,tomorrowburn-red-4))))
   `(cider-test-error-face ((t (:background ,tomorrowburn-magenta))))
   `(cider-test-success-face ((t (:background ,tomorrowburn-green-1))))
   `(cider-fringe-good-face ((t (:foreground ,tomorrowburn-green+4))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,tomorrowburn-cyan))))
   `(circe-my-message-face ((t (:foreground ,tomorrowburn-fg))))
   `(circe-fool-face ((t (:foreground ,tomorrowburn-red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,tomorrowburn-red :weight bold))))
   `(circe-originator-face ((t (:foreground ,tomorrowburn-fg))))
   `(circe-server-face ((t (:foreground ,tomorrowburn-green))))
   `(circe-topic-diff-new-face ((t (:foreground ,tomorrowburn-orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,tomorrowburn-orange :background ,tomorrowburn-bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,tomorrowburn-fg)))
   `(context-coloring-level-1-face ((t :foreground ,tomorrowburn-cyan)))
   `(context-coloring-level-2-face ((t :foreground ,tomorrowburn-green+4)))
   `(context-coloring-level-3-face ((t :foreground ,tomorrowburn-yellow)))
   `(context-coloring-level-4-face ((t :foreground ,tomorrowburn-orange)))
   `(context-coloring-level-5-face ((t :foreground ,tomorrowburn-magenta)))
   `(context-coloring-level-6-face ((t :foreground ,tomorrowburn-blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,tomorrowburn-green+2)))
   `(context-coloring-level-8-face ((t :foreground ,tomorrowburn-yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,tomorrowburn-red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,tomorrowburn-blue :foreground ,tomorrowburn-bg))))
   `(ctbl:face-continue-bar ((t (:background ,tomorrowburn-bg-05 :foreground ,tomorrowburn-bg))))
   `(ctbl:face-row-select ((t (:background ,tomorrowburn-cyan :foreground ,tomorrowburn-bg))))
;;;;; debbugs
   `(debbugs-gnu-done ((t (:foreground ,tomorrowburn-fg-1))))
   `(debbugs-gnu-handled ((t (:foreground ,tomorrowburn-green))))
   `(debbugs-gnu-new ((t (:foreground ,tomorrowburn-red))))
   `(debbugs-gnu-pending ((t (:foreground ,tomorrowburn-blue))))
   `(debbugs-gnu-stale ((t (:foreground ,tomorrowburn-orange))))
   `(debbugs-gnu-tagged ((t (:foreground ,tomorrowburn-red))))
;;;;; diff
   `(diff-added          ((t (:background "#335533" :foreground ,tomorrowburn-green))))
   `(diff-changed        ((t (:background "#555511" :foreground ,tomorrowburn-yellow-1))))
   `(diff-removed        ((t (:background "#553333" :foreground ,tomorrowburn-red-2))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,tomorrowburn-green+4))))
   `(diff-refine-change  ((t (:background "#888811" :foreground ,tomorrowburn-yellow))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,tomorrowburn-red))))
   `(diff-header ((,class (:background ,tomorrowburn-bg+2))
                  (t (:background ,tomorrowburn-fg :foreground ,tomorrowburn-bg))))
   `(diff-file-header
     ((,class (:background ,tomorrowburn-bg+2 :foreground ,tomorrowburn-fg :weight bold))
      (t (:background ,tomorrowburn-fg :foreground ,tomorrowburn-bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,tomorrowburn-blue :background ,tomorrowburn-blue-2))))
   `(diff-hl-delete ((,class (:foreground ,tomorrowburn-red+1 :background ,tomorrowburn-red-1))))
   `(diff-hl-insert ((,class (:foreground ,tomorrowburn-green+1 :background ,tomorrowburn-green-1))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,tomorrowburn-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,tomorrowburn-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,tomorrowburn-orange))))
   `(diredp-date-time ((t (:foreground ,tomorrowburn-magenta))))
   `(diredp-deletion ((t (:foreground ,tomorrowburn-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,tomorrowburn-red))))
   `(diredp-dir-heading ((t (:foreground ,tomorrowburn-blue :background ,tomorrowburn-bg-1))))
   `(diredp-dir-priv ((t (:foreground ,tomorrowburn-cyan))))
   `(diredp-exec-priv ((t (:foreground ,tomorrowburn-red))))
   `(diredp-executable-tag ((t (:foreground ,tomorrowburn-green+1))))
   `(diredp-file-name ((t (:foreground ,tomorrowburn-blue))))
   `(diredp-file-suffix ((t (:foreground ,tomorrowburn-green))))
   `(diredp-flag-mark ((t (:foreground ,tomorrowburn-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,tomorrowburn-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,tomorrowburn-red))))
   `(diredp-link-priv ((t (:foreground ,tomorrowburn-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,tomorrowburn-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,tomorrowburn-orange))))
   `(diredp-no-priv ((t (:foreground ,tomorrowburn-fg))))
   `(diredp-number ((t (:foreground ,tomorrowburn-green+1))))
   `(diredp-other-priv ((t (:foreground ,tomorrowburn-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,tomorrowburn-red-1))))
   `(diredp-read-priv ((t (:foreground ,tomorrowburn-green-1))))
   `(diredp-symlink ((t (:foreground ,tomorrowburn-yellow))))
   `(diredp-write-priv ((t (:foreground ,tomorrowburn-magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,tomorrowburn-red :weight bold))))
   `(dired-async-message ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,tomorrowburn-yellow))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-blue-5))))
   `(ediff-even-diff-A ((t (:background ,tomorrowburn-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,tomorrowburn-bg+1))))
   `(ediff-even-diff-B ((t (:background ,tomorrowburn-bg+1))))
   `(ediff-even-diff-C ((t (:background ,tomorrowburn-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,tomorrowburn-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,tomorrowburn-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,tomorrowburn-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,tomorrowburn-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,tomorrowburn-fg))))
   `(egg-help-header-1 ((t (:foreground ,tomorrowburn-yellow))))
   `(egg-help-header-2 ((t (:foreground ,tomorrowburn-green+3))))
   `(egg-branch ((t (:foreground ,tomorrowburn-yellow))))
   `(egg-branch-mono ((t (:foreground ,tomorrowburn-yellow))))
   `(egg-term ((t (:foreground ,tomorrowburn-yellow))))
   `(egg-diff-add ((t (:foreground ,tomorrowburn-green+4))))
   `(egg-diff-del ((t (:foreground ,tomorrowburn-red+1))))
   `(egg-diff-file-header ((t (:foreground ,tomorrowburn-yellow-2))))
   `(egg-section-title ((t (:foreground ,tomorrowburn-yellow))))
   `(egg-stash-mono ((t (:foreground ,tomorrowburn-green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,tomorrowburn-red))))
   `(elfeed-log-info-level-face ((t (:foreground ,tomorrowburn-blue))))
   `(elfeed-log-warn-level-face ((t (:foreground ,tomorrowburn-yellow))))
   `(elfeed-search-date-face ((t (:foreground ,tomorrowburn-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,tomorrowburn-green))))
   `(elfeed-search-feed-face ((t (:foreground ,tomorrowburn-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,tomorrowburn-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,tomorrowburn-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,tomorrowburn-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,tomorrowburn-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,tomorrowburn-green+2 :background ,tomorrowburn-bg))))
   `(w3m-lnum-match ((t (:background ,tomorrowburn-bg-1
                                     :foreground ,tomorrowburn-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,tomorrowburn-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,tomorrowburn-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,tomorrowburn-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,tomorrowburn-yellow))))
   `(erc-keyword-face ((t (:foreground ,tomorrowburn-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,tomorrowburn-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,tomorrowburn-green))))
   `(erc-pal-face ((t (:foreground ,tomorrowburn-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,tomorrowburn-orange :background ,tomorrowburn-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,tomorrowburn-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; eros
   `(eros-result-overlay-face ((t (:background unspecified))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,tomorrowburn-green+4 :background ,tomorrowburn-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,tomorrowburn-red :background ,tomorrowburn-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,tomorrowburn-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,tomorrowburn-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,tomorrowburn-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,tomorrowburn-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,tomorrowburn-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,tomorrowburn-green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,tomorrowburn-red-1) :inherit unspecified))
      (t (:foreground ,tomorrowburn-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,tomorrowburn-yellow) :inherit unspecified))
      (t (:foreground ,tomorrowburn-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,tomorrowburn-cyan) :inherit unspecified))
      (t (:foreground ,tomorrowburn-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,tomorrowburn-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,tomorrowburn-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,tomorrowburn-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,tomorrowburn-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,tomorrowburn-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,tomorrowburn-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,tomorrowburn-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,tomorrowburn-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,tomorrowburn-orange) :inherit unspecified))
      (t (:foreground ,tomorrowburn-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,tomorrowburn-red) :inherit unspecified))
      (t (:foreground ,tomorrowburn-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,tomorrowburn-fg))))
   `(ack-file ((t (:foreground ,tomorrowburn-blue))))
   `(ack-line ((t (:foreground ,tomorrowburn-yellow))))
   `(ack-match ((t (:foreground ,tomorrowburn-orange :background ,tomorrowburn-bg-1 :weight bold))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,tomorrowburn-green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,tomorrowburn-blue+1  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,tomorrowburn-yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,tomorrowburn-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,tomorrowburn-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,tomorrowburn-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,tomorrowburn-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,tomorrowburn-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,tomorrowburn-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,tomorrowburn-magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, tomorrowburn-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,tomorrowburn-green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,tomorrowburn-red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,tomorrowburn-blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,tomorrowburn-blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,tomorrowburn-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,tomorrowburn-blue))))
   `(gnus-summary-high-read ((t (:foreground ,tomorrowburn-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,tomorrowburn-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,tomorrowburn-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,tomorrowburn-blue))))
   `(gnus-summary-low-read ((t (:foreground ,tomorrowburn-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,tomorrowburn-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,tomorrowburn-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,tomorrowburn-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,tomorrowburn-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,tomorrowburn-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,tomorrowburn-fg))))
   `(gnus-summary-selected ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,tomorrowburn-blue))))
   `(gnus-cite-10 ((t (:foreground ,tomorrowburn-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,tomorrowburn-yellow))))
   `(gnus-cite-2 ((t (:foreground ,tomorrowburn-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,tomorrowburn-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,tomorrowburn-green+2))))
   `(gnus-cite-5 ((t (:foreground ,tomorrowburn-green+1))))
   `(gnus-cite-6 ((t (:foreground ,tomorrowburn-green))))
   `(gnus-cite-7 ((t (:foreground ,tomorrowburn-red))))
   `(gnus-cite-8 ((t (:foreground ,tomorrowburn-red-1))))
   `(gnus-cite-9 ((t (:foreground ,tomorrowburn-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,tomorrowburn-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,tomorrowburn-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,tomorrowburn-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,tomorrowburn-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,tomorrowburn-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,tomorrowburn-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,tomorrowburn-bg+2))))
   `(gnus-signature ((t (:foreground ,tomorrowburn-yellow))))
   `(gnus-x ((t (:background ,tomorrowburn-fg :foreground ,tomorrowburn-bg))))
   `(mm-uu-extract ((t (:background ,tomorrowburn-bg-05 :foreground ,tomorrowburn-green+1))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,tomorrowburn-blue))))
   `(guide-key/key-face ((t (:foreground ,tomorrowburn-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,tomorrowburn-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,tomorrowburn-green
                      :background ,tomorrowburn-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,tomorrowburn-yellow
                      :background ,tomorrowburn-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,tomorrowburn-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,tomorrowburn-bg+1))))
   `(helm-visible-mark ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,tomorrowburn-green+4 :background ,tomorrowburn-bg-1))))
   `(helm-separator ((t (:foreground ,tomorrowburn-red :background ,tomorrowburn-bg))))
   `(helm-time-zone-current ((t (:foreground ,tomorrowburn-green+2 :background ,tomorrowburn-bg))))
   `(helm-time-zone-home ((t (:foreground ,tomorrowburn-red :background ,tomorrowburn-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,tomorrowburn-orange :background ,tomorrowburn-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,tomorrowburn-magenta :background ,tomorrowburn-bg))))
   `(helm-bookmark-info ((t (:foreground ,tomorrowburn-green+2 :background ,tomorrowburn-bg))))
   `(helm-bookmark-man ((t (:foreground ,tomorrowburn-yellow :background ,tomorrowburn-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,tomorrowburn-magenta :background ,tomorrowburn-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,tomorrowburn-red :background ,tomorrowburn-bg))))
   `(helm-buffer-process ((t (:foreground ,tomorrowburn-cyan :background ,tomorrowburn-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-bg))))
   `(helm-buffer-size ((t (:foreground ,tomorrowburn-fg-1 :background ,tomorrowburn-bg))))
   `(helm-ff-directory ((t (:foreground ,tomorrowburn-cyan :background ,tomorrowburn-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,tomorrowburn-green+2 :background ,tomorrowburn-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,tomorrowburn-red :background ,tomorrowburn-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,tomorrowburn-yellow :background ,tomorrowburn-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,tomorrowburn-cyan :background ,tomorrowburn-bg))))
   `(helm-grep-file ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-bg))))
   `(helm-grep-finish ((t (:foreground ,tomorrowburn-green+2 :background ,tomorrowburn-bg))))
   `(helm-grep-lineno ((t (:foreground ,tomorrowburn-fg-1 :background ,tomorrowburn-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,tomorrowburn-red :background ,tomorrowburn-bg))))
   `(helm-match ((t (:foreground ,tomorrowburn-orange :background ,tomorrowburn-bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,tomorrowburn-cyan :background ,tomorrowburn-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,tomorrowburn-fg-1 :background ,tomorrowburn-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,tomorrowburn-yellow :background ,tomorrowburn-bg+2 :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,tomorrowburn-bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,tomorrowburn-bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,tomorrowburn-bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,tomorrowburn-red-1 :background ,tomorrowburn-bg))))
   `(hydra-face-amaranth ((t (:foreground ,tomorrowburn-red-3 :background ,tomorrowburn-bg))))
   `(hydra-face-blue ((t (:foreground ,tomorrowburn-blue :background ,tomorrowburn-bg))))
   `(hydra-face-pink ((t (:foreground ,tomorrowburn-magenta :background ,tomorrowburn-bg))))
   `(hydra-face-teal ((t (:foreground ,tomorrowburn-cyan :background ,tomorrowburn-bg))))
;;;;; info+
   `(info-command-ref-item ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-orange))))
   `(info-constant-ref-item ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-magenta))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-yellow))))
   `(info-function-ref-item ((t (:background ,tomorrowburn-bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-yellow))))
   `(info-menu ((t (:foreground ,tomorrowburn-yellow))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,tomorrowburn-bg-1))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-yellow))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-blue+1))))
   `(info-user-option-ref-item ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-red))))
   `(info-variable-ref-item ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-orange))))
;;;;; irfc
   `(irfc-head-name-face ((t (:foreground ,tomorrowburn-red :weight bold))))
   `(irfc-head-number-face ((t (:foreground ,tomorrowburn-red :weight bold))))
   `(irfc-reference-face ((t (:foreground ,tomorrowburn-blue-1 :weight bold))))
   `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(irfc-rfc-link-face ((t (:inherit link))))
   `(irfc-rfc-number-face ((t (:foreground ,tomorrowburn-cyan :weight bold))))
   `(irfc-std-number-face ((t (:foreground ,tomorrowburn-green+4 :weight bold))))
   `(irfc-table-item-face ((t (:foreground ,tomorrowburn-green+3))))
   `(irfc-title-face ((t (:foreground ,tomorrowburn-yellow
                                      :underline t :weight bold))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,tomorrowburn-green :background ,tomorrowburn-bg))))
   `(ivy-current-match ((t (:foreground ,tomorrowburn-yellow :weight bold :underline t))))
   `(ivy-cursor ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-fg))))
   `(ivy-match-required-face ((t (:foreground ,tomorrowburn-red :background ,tomorrowburn-bg))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,tomorrowburn-bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,tomorrowburn-green-1))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,tomorrowburn-green))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,tomorrowburn-green+1))))
   `(ivy-remote ((t (:foreground ,tomorrowburn-blue :background ,tomorrowburn-bg))))
   `(ivy-subdir ((t (:foreground ,tomorrowburn-yellow :background ,tomorrowburn-bg))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,tomorrowburn-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,tomorrowburn-yellow))))
   `(ido-indicator ((t (:foreground ,tomorrowburn-yellow :background ,tomorrowburn-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,tomorrowburn-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,tomorrowburn-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,tomorrowburn-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,tomorrowburn-red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,tomorrowburn-magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,tomorrowburn-orange))))
   `(jabber-roster-user-error ((t (:foreground ,tomorrowburn-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,tomorrowburn-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,tomorrowburn-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,tomorrowburn-red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,tomorrowburn-green+3))))
   `(jabber-activity-face((t (:foreground ,tomorrowburn-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,tomorrowburn-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,tomorrowburn-orange))))
   `(js2-error ((t (:foreground ,tomorrowburn-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,tomorrowburn-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,tomorrowburn-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,tomorrowburn-green+3))))
   `(js2-function-param ((t (:foreground, tomorrowburn-orange))))
   `(js2-external-variable ((t (:foreground ,tomorrowburn-orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,tomorrowburn-green-1))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,tomorrowburn-orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,tomorrowburn-red-1))))
   `(js2-object-property ((t (:foreground ,tomorrowburn-blue+1))))
   `(js2-magic-paren ((t (:foreground ,tomorrowburn-blue-5))))
   `(js2-private-function-call ((t (:foreground ,tomorrowburn-cyan))))
   `(js2-function-call ((t (:foreground ,tomorrowburn-cyan))))
   `(js2-private-member ((t (:foreground ,tomorrowburn-blue-1))))
   `(js2-keywords ((t (:foreground ,tomorrowburn-magenta))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,tomorrowburn-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,tomorrowburn-fg :weight normal))))
   `(ledger-font-payee-pending-face ((t (:foreground ,tomorrowburn-red :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,tomorrowburn-bg+1))))
   `(ledger-font-auto-xact-face ((t (:foreground ,tomorrowburn-yellow-1 :weight normal))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,tomorrowburn-green :weight normal))))
   `(ledger-font-pending-face ((t (:foreground ,tomorrowburn-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,tomorrowburn-fg))))
   `(ledger-font-posting-date-face ((t (:foreground ,tomorrowburn-orange :weight normal))))
   `(ledger-font-posting-account-face ((t (:foreground ,tomorrowburn-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,tomorrowburn-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,tomorrowburn-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,tomorrowburn-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,tomorrowburn-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,tomorrowburn-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,tomorrowburn-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,tomorrowburn-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,tomorrowburn-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,tomorrowburn-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,tomorrowburn-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,tomorrowburn-green+2 :background ,tomorrowburn-bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,tomorrowburn-bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-fg))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,tomorrowburn-yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,tomorrowburn-fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,tomorrowburn-yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,tomorrowburn-yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,tomorrowburn-green+2 :background ,tomorrowburn-bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,tomorrowburn-blue-1))))
   `(lui-hilight-face ((t (:foreground ,tomorrowburn-green+2 :background ,tomorrowburn-bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,tomorrowburn-green+2 :background ,tomorrowburn-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,tomorrowburn-red+1 :background ,tomorrowburn-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,tomorrowburn-blue+1 :background ,tomorrowburn-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,tomorrowburn-magenta :background ,tomorrowburn-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,tomorrowburn-yellow :background ,tomorrowburn-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,tomorrowburn-bg+05))))
   `(magit-section-heading             ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,tomorrowburn-orange :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,tomorrowburn-bg+05  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,tomorrowburn-bg+05
                                                        :foreground ,tomorrowburn-orange :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,tomorrowburn-bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,tomorrowburn-bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,tomorrowburn-bg+2
                                                        :foreground ,tomorrowburn-orange))))
   `(magit-diff-lines-heading          ((t (:background ,tomorrowburn-orange
                                                        :foreground ,tomorrowburn-bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,tomorrowburn-bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,tomorrowburn-green+4))))
   `(magit-diffstat-removed ((t (:foreground ,tomorrowburn-red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,tomorrowburn-yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,tomorrowburn-green-1 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,tomorrowburn-green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,tomorrowburn-fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,tomorrowburn-blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,tomorrowburn-green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,tomorrowburn-red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,tomorrowburn-orange))))
   `(magit-log-date      ((t (:foreground ,tomorrowburn-fg-1))))
   `(magit-log-graph     ((t (:foreground ,tomorrowburn-fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,tomorrowburn-yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,tomorrowburn-green))))
   `(magit-sequence-part ((t (:foreground ,tomorrowburn-yellow))))
   `(magit-sequence-head ((t (:foreground ,tomorrowburn-blue))))
   `(magit-sequence-drop ((t (:foreground ,tomorrowburn-red))))
   `(magit-sequence-done ((t (:foreground ,tomorrowburn-fg-1))))
   `(magit-sequence-onto ((t (:foreground ,tomorrowburn-fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,tomorrowburn-green))))
   `(magit-bisect-skip ((t (:foreground ,tomorrowburn-yellow))))
   `(magit-bisect-bad  ((t (:foreground ,tomorrowburn-red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-blue-2))))
   `(magit-blame-hash    ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-blue-2))))
   `(magit-blame-name    ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-orange))))
   `(magit-blame-date    ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-orange))))
   `(magit-blame-summary ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,tomorrowburn-bg+3))))
   `(magit-hash           ((t (:foreground ,tomorrowburn-bg+3))))
   `(magit-tag            ((t (:foreground ,tomorrowburn-orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,tomorrowburn-green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,tomorrowburn-blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,tomorrowburn-blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,tomorrowburn-blue   :weight bold))))
   `(magit-refname        ((t (:background ,tomorrowburn-bg+2 :foreground ,tomorrowburn-fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,tomorrowburn-bg+2 :foreground ,tomorrowburn-fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,tomorrowburn-bg+2 :foreground ,tomorrowburn-fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,tomorrowburn-green))))
   `(magit-signature-bad       ((t (:foreground ,tomorrowburn-red))))
   `(magit-signature-untrusted ((t (:foreground ,tomorrowburn-yellow))))
   `(magit-cherry-unmatched    ((t (:foreground ,tomorrowburn-cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,tomorrowburn-magenta))))
   `(magit-reflog-commit       ((t (:foreground ,tomorrowburn-green))))
   `(magit-reflog-amend        ((t (:foreground ,tomorrowburn-magenta))))
   `(magit-reflog-merge        ((t (:foreground ,tomorrowburn-green))))
   `(magit-reflog-checkout     ((t (:foreground ,tomorrowburn-blue))))
   `(magit-reflog-reset        ((t (:foreground ,tomorrowburn-red))))
   `(magit-reflog-rebase       ((t (:foreground ,tomorrowburn-magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,tomorrowburn-green))))
   `(magit-reflog-remote       ((t (:foreground ,tomorrowburn-cyan))))
   `(magit-reflog-other        ((t (:foreground ,tomorrowburn-cyan))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,tomorrowburn-green+1))))
   `(message-header-other ((t (:foreground ,tomorrowburn-green))))
   `(message-header-to ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,tomorrowburn-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,tomorrowburn-green))))
   `(message-mml ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,tomorrowburn-orange))))
   `(mew-face-header-from ((t (:foreground ,tomorrowburn-yellow))))
   `(mew-face-header-date ((t (:foreground ,tomorrowburn-green))))
   `(mew-face-header-to ((t (:foreground ,tomorrowburn-red))))
   `(mew-face-header-key ((t (:foreground ,tomorrowburn-green))))
   `(mew-face-header-private ((t (:foreground ,tomorrowburn-green))))
   `(mew-face-header-important ((t (:foreground ,tomorrowburn-blue))))
   `(mew-face-header-marginal ((t (:foreground ,tomorrowburn-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,tomorrowburn-red))))
   `(mew-face-header-xmew ((t (:foreground ,tomorrowburn-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,tomorrowburn-red))))
   `(mew-face-body-url ((t (:foreground ,tomorrowburn-orange))))
   `(mew-face-body-comment ((t (:foreground ,tomorrowburn-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,tomorrowburn-green))))
   `(mew-face-body-cite2 ((t (:foreground ,tomorrowburn-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,tomorrowburn-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,tomorrowburn-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,tomorrowburn-red))))
   `(mew-face-mark-review ((t (:foreground ,tomorrowburn-blue))))
   `(mew-face-mark-escape ((t (:foreground ,tomorrowburn-green))))
   `(mew-face-mark-delete ((t (:foreground ,tomorrowburn-red))))
   `(mew-face-mark-unlink ((t (:foreground ,tomorrowburn-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,tomorrowburn-green))))
   `(mew-face-mark-unread ((t (:foreground ,tomorrowburn-red-2))))
   `(mew-face-eof-message ((t (:foreground ,tomorrowburn-green))))
   `(mew-face-eof-part ((t (:foreground ,tomorrowburn-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,tomorrowburn-cyan :background ,tomorrowburn-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,tomorrowburn-blue))))
   `(mingus-pausing-face ((t (:foreground ,tomorrowburn-magenta))))
   `(mingus-playing-face ((t (:foreground ,tomorrowburn-cyan))))
   `(mingus-playlist-face ((t (:foreground ,tomorrowburn-cyan ))))
   `(mingus-mark-face ((t (:bold t :foreground ,tomorrowburn-magenta))))
   `(mingus-song-file-face ((t (:foreground ,tomorrowburn-yellow))))
   `(mingus-artist-face ((t (:foreground ,tomorrowburn-cyan))))
   `(mingus-album-face ((t (:underline t :foreground ,tomorrowburn-red+1))))
   `(mingus-album-stale-face ((t (:foreground ,tomorrowburn-red+1))))
   `(mingus-stopped-face ((t (:foreground ,tomorrowburn-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,tomorrowburn-yellow))))
   `(nav-face-button-num ((t (:foreground ,tomorrowburn-cyan))))
   `(nav-face-dir ((t (:foreground ,tomorrowburn-green))))
   `(nav-face-hdir ((t (:foreground ,tomorrowburn-red))))
   `(nav-face-file ((t (:foreground ,tomorrowburn-fg))))
   `(nav-face-hfile ((t (:foreground ,tomorrowburn-red-4))))
;;;;; merlin
   `(merlin-type-face ((t (:inherit highlight))))
   `(merlin-compilation-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,tomorrowburn-orange)))
      (t
       (:underline ,tomorrowburn-orange))))
   `(merlin-compilation-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,tomorrowburn-red)))
      (t
       (:underline ,tomorrowburn-red))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,tomorrowburn-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,tomorrowburn-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,tomorrowburn-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,tomorrowburn-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,tomorrowburn-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,tomorrowburn-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,tomorrowburn-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,tomorrowburn-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,tomorrowburn-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,tomorrowburn-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,tomorrowburn-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,tomorrowburn-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,tomorrowburn-bg+1))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,tomorrowburn-blue+1 :weight bold))))
   `(neo-header-face ((t (:foreground ,tomorrowburn-fg))))
   `(neo-root-dir-face ((t (:foreground ,tomorrowburn-blue+1 :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,tomorrowburn-blue))))
   `(neo-file-link-face ((t (:foreground ,tomorrowburn-fg))))
   `(neo-expand-btn-face ((t (:foreground ,tomorrowburn-blue))))
   `(neo-vc-default-face ((t (:foreground ,tomorrowburn-fg+1))))
   `(neo-vc-user-face ((t (:foreground ,tomorrowburn-red :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,tomorrowburn-fg))))
   `(neo-vc-edited-face ((t (:foreground ,tomorrowburn-magenta))))
   `(neo-vc-needs-merge-face ((t (:foreground ,tomorrowburn-red+1))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,tomorrowburn-red :background ,tomorrowburn-blue-5))))
   `(neo-vc-added-face ((t (:foreground ,tomorrowburn-green+1))))
   `(neo-vc-conflict-face ((t (:foreground ,tomorrowburn-red+1))))
   `(neo-vc-missing-face ((t (:foreground ,tomorrowburn-red+1))))
   `(neo-vc-ignored-face ((t (:foreground ,tomorrowburn-fg-1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,tomorrowburn-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,tomorrowburn-fg :weight bold))))
   `(org-checkbox ((t (:background ,tomorrowburn-bg+2 :foreground ,tomorrowburn-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,tomorrowburn-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,tomorrowburn-red-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,tomorrowburn-green+3))))
   `(org-formula ((t (:foreground ,tomorrowburn-yellow-2))))
   `(org-headline-done ((t (:foreground ,tomorrowburn-green+3))))
   `(org-hide ((t (:foreground ,tomorrowburn-bg-1))))
   `(org-level-1 ((t (:foreground ,tomorrowburn-orange))))
   `(org-level-2 ((t (:foreground ,tomorrowburn-green+4))))
   `(org-level-3 ((t (:foreground ,tomorrowburn-blue-1))))
   `(org-level-4 ((t (:foreground ,tomorrowburn-yellow-2))))
   `(org-level-5 ((t (:foreground ,tomorrowburn-cyan))))
   `(org-level-6 ((t (:foreground ,tomorrowburn-green+2))))
   `(org-level-7 ((t (:foreground ,tomorrowburn-red-4))))
   `(org-level-8 ((t (:foreground ,tomorrowburn-blue-4))))
   `(org-link ((t (:foreground ,tomorrowburn-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,tomorrowburn-green+4))))
   `(org-scheduled-previously ((t (:foreground ,tomorrowburn-red))))
   `(org-scheduled-today ((t (:foreground ,tomorrowburn-blue+1))))
   `(org-sexp-date ((t (:foreground ,tomorrowburn-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,tomorrowburn-green+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,tomorrowburn-orange))))
   `(org-todo ((t (:weight bold :foreground ,tomorrowburn-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,tomorrowburn-red :weight bold :underline nil))))
   `(org-column ((t (:background ,tomorrowburn-bg-1))))
   `(org-column-title ((t (:background ,tomorrowburn-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-red-1))))
   `(org-ellipsis ((t (:foreground ,tomorrowburn-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,tomorrowburn-cyan :underline t))))
   `(org-document-title ((t (:foreground ,tomorrowburn-blue))))
   `(org-document-info ((t (:foreground ,tomorrowburn-blue))))
   `(org-habit-ready-face ((t :background ,tomorrowburn-green)))
   `(org-habit-alert-face ((t :background ,tomorrowburn-yellow-1 :foreground ,tomorrowburn-bg)))
   `(org-habit-clear-face ((t :background ,tomorrowburn-blue-3)))
   `(org-habit-overdue-face ((t :background ,tomorrowburn-red-3)))
   `(org-habit-clear-future-face ((t :background ,tomorrowburn-blue-4)))
   `(org-habit-ready-future-face ((t :background ,tomorrowburn-green-1)))
   `(org-habit-alert-future-face ((t :background ,tomorrowburn-yellow-2 :foreground ,tomorrowburn-bg)))
   `(org-habit-overdue-future-face ((t :background ,tomorrowburn-red-4)))
;;;;; outline
   `(outline-1 ((t (:foreground ,tomorrowburn-orange))))
   `(outline-2 ((t (:foreground ,tomorrowburn-green+4))))
   `(outline-3 ((t (:foreground ,tomorrowburn-blue-1))))
   `(outline-4 ((t (:foreground ,tomorrowburn-yellow-2))))
   `(outline-5 ((t (:foreground ,tomorrowburn-cyan))))
   `(outline-6 ((t (:foreground ,tomorrowburn-green+2))))
   `(outline-7 ((t (:foreground ,tomorrowburn-red-4))))
   `(outline-8 ((t (:foreground ,tomorrowburn-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,tomorrowburn-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,tomorrowburn-bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,tomorrowburn-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,tomorrowburn-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,tomorrowburn-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-orange))))
   `(proof-error-face ((t (:foreground ,tomorrowburn-fg :background ,tomorrowburn-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-orange))))
   `(proof-locked-face ((t (:background ,tomorrowburn-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-orange))))
   `(proof-queue-face ((t (:background ,tomorrowburn-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,tomorrowburn-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,tomorrowburn-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,tomorrowburn-bg))))
   `(proof-warning-face ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,tomorrowburn-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,tomorrowburn-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,tomorrowburn-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,tomorrowburn-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,tomorrowburn-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,tomorrowburn-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,tomorrowburn-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,tomorrowburn-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,tomorrowburn-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,tomorrowburn-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,tomorrowburn-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,tomorrowburn-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,tomorrowburn-blue))))
   `(rcirc-other-nick ((t (:foreground ,tomorrowburn-orange))))
   `(rcirc-bright-nick ((t (:foreground ,tomorrowburn-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,tomorrowburn-blue-2))))
   `(rcirc-server ((t (:foreground ,tomorrowburn-green))))
   `(rcirc-server-prefix ((t (:foreground ,tomorrowburn-green+1))))
   `(rcirc-timestamp ((t (:foreground ,tomorrowburn-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,tomorrowburn-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:weight bold))))
   `(rcirc-prompt ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:weight bold))))
   `(rcirc-url ((t (:weight bold))))
   `(rcirc-keyword ((t (:foreground ,tomorrowburn-yellow :weight bold))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-magenta))))
   `(reb-match-1 ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-blue))))
   `(reb-match-2 ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-orange))))
   `(reb-match-3 ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-red))))
;;;;; regex-tool
   `(regex-tool-matched-face ((t (:background ,tomorrowburn-blue-4 :weight bold))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,tomorrowburn-green))))
   `(rpm-spec-doc-face ((t (:foreground ,tomorrowburn-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,tomorrowburn-red))))
   `(rpm-spec-macro-face ((t (:foreground ,tomorrowburn-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,tomorrowburn-red))))
   `(rpm-spec-package-face ((t (:foreground ,tomorrowburn-red))))
   `(rpm-spec-section-face ((t (:foreground ,tomorrowburn-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,tomorrowburn-blue))))
   `(rpm-spec-var-face ((t (:foreground ,tomorrowburn-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,tomorrowburn-orange))))
   `(rst-level-2-face ((t (:foreground ,tomorrowburn-green+1))))
   `(rst-level-3-face ((t (:foreground ,tomorrowburn-blue-1))))
   `(rst-level-4-face ((t (:foreground ,tomorrowburn-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,tomorrowburn-cyan))))
   `(rst-level-6-face ((t (:foreground ,tomorrowburn-green-1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,tomorrowburn-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,tomorrowburn-red+1 :background ,tomorrowburn-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,tomorrowburn-bg+3 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Tomorrowburn for sml
   `(sml/global ((,class (:foreground ,tomorrowburn-fg :weight bold))))
   `(sml/modes ((,class (:foreground ,tomorrowburn-yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,tomorrowburn-fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,tomorrowburn-yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,tomorrowburn-blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,tomorrowburn-blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,tomorrowburn-blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,tomorrowburn-orange))))
   `(sml/git ((,class (:foreground ,tomorrowburn-green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,tomorrowburn-orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,tomorrowburn-red-2))))
   `(sml/outside-modified ((,class (:foreground ,tomorrowburn-orange))))
   `(sml/modified ((,class (:foreground ,tomorrowburn-red))))
   `(sml/vc-edited ((,class (:foreground ,tomorrowburn-green+2))))
   `(sml/charging ((,class (:foreground ,tomorrowburn-green+4))))
   `(sml/discharging ((,class (:foreground ,tomorrowburn-red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,tomorrowburn-red+1 :background ,tomorrowburn-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,tomorrowburn-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,tomorrowburn-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,tomorrowburn-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,tomorrowburn-red)))
      (t
       (:underline ,tomorrowburn-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,tomorrowburn-orange)))
      (t
       (:underline ,tomorrowburn-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,tomorrowburn-yellow)))
      (t
       (:underline ,tomorrowburn-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,tomorrowburn-green)))
      (t
       (:underline ,tomorrowburn-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,tomorrowburn-green+2))))
   `(speedbar-directory-face ((t (:foreground ,tomorrowburn-cyan))))
   `(speedbar-file-face ((t (:foreground ,tomorrowburn-fg))))
   `(speedbar-highlight-face ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-green+2))))
   `(speedbar-selected-face ((t (:foreground ,tomorrowburn-red))))
   `(speedbar-separator-face ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,tomorrowburn-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,tomorrowburn-fg
                                    :background ,tomorrowburn-bg))))
   `(tabbar-selected ((t (:foreground ,tomorrowburn-fg
                                      :background ,tomorrowburn-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,tomorrowburn-fg
                                        :background ,tomorrowburn-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,tomorrowburn-bg
                                       :background ,tomorrowburn-bg-1))))
   `(term-color-red ((t (:foreground ,tomorrowburn-red-2
                                     :background ,tomorrowburn-red-4))))
   `(term-color-green ((t (:foreground ,tomorrowburn-green
                                       :background ,tomorrowburn-green+2))))
   `(term-color-yellow ((t (:foreground ,tomorrowburn-orange
                                        :background ,tomorrowburn-yellow))))
   `(term-color-blue ((t (:foreground ,tomorrowburn-blue-1
                                      :background ,tomorrowburn-blue-4))))
   `(term-color-magenta ((t (:foreground ,tomorrowburn-magenta
                                         :background ,tomorrowburn-red))))
   `(term-color-cyan ((t (:foreground ,tomorrowburn-cyan
                                      :background ,tomorrowburn-blue))))
   `(term-color-white ((t (:foreground ,tomorrowburn-fg
                                       :background ,tomorrowburn-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,tomorrowburn-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,tomorrowburn-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,tomorrowburn-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,tomorrowburn-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,tomorrowburn-cyan))))
;;;;; visual-regexp
   `(vr/group-0 ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-green :weight bold))))
   `(vr/group-1 ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-orange :weight bold))))
   `(vr/group-2 ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-blue :weight bold))))
   `(vr/match-0 ((t (:inherit isearch))))
   `(vr/match-1 ((t (:foreground ,tomorrowburn-yellow-2 :background ,tomorrowburn-bg-1 :weight bold))))
   `(vr/match-separator-face ((t (:foreground ,tomorrowburn-red :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,tomorrowburn-bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,tomorrowburn-orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,tomorrowburn-orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,tomorrowburn-green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,tomorrowburn-blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,tomorrowburn-blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,tomorrowburn-orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,tomorrowburn-cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,tomorrowburn-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,tomorrowburn-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,tomorrowburn-bg+1 :foreground ,tomorrowburn-bg+1))))
   `(whitespace-hspace ((t (:background ,tomorrowburn-bg+1 :foreground ,tomorrowburn-bg+1))))
   `(whitespace-tab ((t (:background ,tomorrowburn-red-1))))
   `(whitespace-newline ((t (:foreground ,tomorrowburn-bg+1))))
   `(whitespace-trailing ((t (:background ,tomorrowburn-red))))
   `(whitespace-line ((t (:background ,tomorrowburn-bg :foreground ,tomorrowburn-magenta))))
   `(whitespace-space-before-tab ((t (:background ,tomorrowburn-orange :foreground ,tomorrowburn-orange))))
   `(whitespace-indentation ((t (:background ,tomorrowburn-yellow :foreground ,tomorrowburn-red))))
   `(whitespace-empty ((t (:background ,tomorrowburn-yellow))))
   `(whitespace-space-after-tab ((t (:background ,tomorrowburn-yellow :foreground ,tomorrowburn-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,tomorrowburn-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,tomorrowburn-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,tomorrowburn-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,tomorrowburn-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,tomorrowburn-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,tomorrowburn-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,tomorrowburn-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,tomorrowburn-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,tomorrowburn-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,tomorrowburn-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,tomorrowburn-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,tomorrowburn-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,tomorrowburn-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,tomorrowburn-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,tomorrowburn-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,tomorrowburn-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,tomorrowburn-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,tomorrowburn-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,tomorrowburn-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,tomorrowburn-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,tomorrowburn-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,tomorrowburn-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,tomorrowburn-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,tomorrowburn-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,tomorrowburn-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,tomorrowburn-green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,tomorrowburn-yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,tomorrowburn-cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,tomorrowburn-red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,tomorrowburn-bg :background ,tomorrowburn-blue+1))))
   `(cscope-separator-face ((t (:foreground ,tomorrowburn-red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,tomorrowburn-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,tomorrowburn-bg-1 :foreground ,tomorrowburn-bg-1))))
   ))

;;; Theme Variables
(tomorrowburn-with-color-variables
  (custom-theme-set-variables
   'tomorrowburn
;;;;; ansi-color
   `(ansi-color-names-vector [,tomorrowburn-bg ,tomorrowburn-red ,tomorrowburn-green ,tomorrowburn-yellow
                                          ,tomorrowburn-blue ,tomorrowburn-magenta ,tomorrowburn-cyan ,tomorrowburn-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,tomorrowburn-bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,tomorrowburn-red ,tomorrowburn-orange ,tomorrowburn-yellow ,tomorrowburn-green ,tomorrowburn-green+4
                    ,tomorrowburn-cyan ,tomorrowburn-blue+1 ,tomorrowburn-magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,tomorrowburn-fg . ,tomorrowburn-bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,tomorrowburn-red-1)
       ( 40. . ,tomorrowburn-red)
       ( 60. . ,tomorrowburn-orange)
       ( 80. . ,tomorrowburn-yellow-2)
       (100. . ,tomorrowburn-yellow-1)
       (120. . ,tomorrowburn-yellow)
       (140. . ,tomorrowburn-green-1)
       (160. . ,tomorrowburn-green)
       (180. . ,tomorrowburn-green+1)
       (200. . ,tomorrowburn-green+2)
       (220. . ,tomorrowburn-green+3)
       (240. . ,tomorrowburn-green+4)
       (260. . ,tomorrowburn-cyan)
       (280. . ,tomorrowburn-blue-2)
       (300. . ,tomorrowburn-blue-1)
       (320. . ,tomorrowburn-blue)
       (340. . ,tomorrowburn-blue+1)
       (360. . ,tomorrowburn-magenta)))
   `(vc-annotate-very-old-color ,tomorrowburn-magenta)
   `(vc-annotate-background ,tomorrowburn-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar tomorrowburn-add-font-lock-keywords nil
  "Whether to add font-lock keywords for tomorrowburn color names.
In buffers visiting library `tomorrowburn-theme.el' the tomorrowburn
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar tomorrowburn-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after tomorrowburn activate)
;;   "Maybe also add font-lock keywords for tomorrowburn colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or tomorrowburn-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "tomorrowburn-theme.el")))
;;     (unless tomorrowburn-colors-font-lock-keywords
;;       (setq tomorrowburn-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car tomorrowburn-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc tomorrowburn-colors-alist))))))
;;     (font-lock-add-keywords nil tomorrowburn-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after tomorrowburn activate)
;;   "Also remove font-lock keywords for tomorrowburn colors."
;;   (font-lock-remove-keywords nil tomorrowburn-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'tomorrowburn)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; tomorrowburn-theme.el ends here
