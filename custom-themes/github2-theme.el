;;; github2-theme.el --- The GitHub color theme.

;; Copyright (C) 2016-2017 Philip Arvidsson

;; Author: Philip Arvidsson <philip@philiparvidsson.com>
;; URL: https://github.com/philiparvidsson/emacs-github-theme
;; Package-Version: 20170221.804
;; Version: 1.0

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

;; This file is based on the Zenburn theme file by Bozhidar Batsov.

;;; Credits:

;; Bozhidar Batsov created the Zenburn theme file which this file is based on.

;;; Code:

(deftheme github2 "The Github2 color theme")

;;; Color Palette

(defvar github2-default-colors-alist
  '(("github2-black"                  . "#000000")
    ("github2-border"                 . "#d0d0d0")
    ("github2-comment"                . "#969896")
    ("github2-constant"               . "#0086b3")
    ("github2-diff-added"             . "#eaffea")
    ("github2-diff-added-highlight"   . "#a6f3a6")
    ("github2-diff-removed"           . "#ffecec")
    ("github2-diff-removed-highlight" . "#f8cbcb")
    ("github2-function"               . "#654C8C")
    ("github2-highlight"              . "#f8eec7")
    ("github2-html-tag"               . "#63a35c")
    ("github2-keyword"                . "#a71d5d")
    ("github2-selection"              . "#b0cde7")
    ("github2-string"                 . "#183691")
    ("github2-text"                   . "#333333")
    ("github2-white"                  . "#ffffff"))
  "List of Github2 colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defvar github2-override-colors-alist
  '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist before loading the theme.")

(defvar github2-colors-alist
  (append github2-default-colors-alist github2-override-colors-alist))

(defmacro github2-with-color-variables (&rest body)
  "`let' bind all colors defined in `github2-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   github2-colors-alist))
     ,@body))

;;; Theme Faces
(github2-with-color-variables
  (custom-theme-set-faces
   'github2
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,github2-keyword :underline t :weight bold))))
   `(link-visited ((t (:foreground ,github2-text :underline t :weight normal))))
   `(default ((t (:foreground "#080808" :background ,github2-white))))
   `(cursor ((t (:foreground ,github2-text :background ,github2-text))))
   `(escape-glyph ((t (:foreground ,github2-keyword :bold t))))
   `(fringe ((t (:foreground ,github2-text :background ,github2-white))))
   `(header-line ((t (:foreground ,github2-keyword
                                  :background ,github2-selection
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,github2-highlight))))
   `(success ((t (:foreground ,github2-comment :weight bold))))
   `(warning ((t (:foreground ,github2-text :weight bold))))
   `(tooltip ((t (:foreground ,github2-text :background ,github2-white))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,github2-keyword))))
   `(compilation-enter-directory-face ((t (:foreground ,github2-comment))))
   `(compilation-error-face ((t (:foreground ,github2-text :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,github2-text))))
   `(compilation-info-face ((t (:foreground ,github2-text))))
   `(compilation-info ((t (:foreground ,github2-constant :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,github2-comment))))
   `(compilation-line-face ((t (:foreground ,github2-keyword))))
   `(compilation-line-number ((t (:foreground ,github2-keyword))))
   `(compilation-message-face ((t (:foreground ,github2-text))))
   `(compilation-warning-face ((t (:foreground ,github2-text :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,github2-comment :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,github2-string :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,github2-keyword :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,github2-text))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,github2-text))))
   `(grep-error-face ((t (:foreground ,github2-text :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,github2-text))))
   `(grep-match-face ((t (:foreground ,github2-text :weight bold))))
   `(match ((t (:background ,github2-selection :foreground ,github2-text :weight bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,github2-white :weight bold :background ,github2-selection))))
   `(isearch-fail ((t (:foreground ,github2-border :background ,github2-white))))
   `(lazy-highlight ((t (:foreground ,github2-text :weight bold :background ,github2-highlight))))

   `(menu ((t (:foreground ,github2-text :background ,github2-white))))
   `(minibuffer-prompt ((t (:foreground ,github2-keyword))))
   `(mode-line
     ((,class (:foreground "black" :background "grey75" :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,github2-black :weight bold))))
   `(mode-line-inactive
     ((t (:foreground "grey20"
                      :background "grey90"
                      :box (:line-width -1 :color ,github2-border)))))
   `(region ((,class (:background ,github2-selection))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,github2-white))))
   `(trailing-whitespace ((t (:background ,github2-string))))
   `(vertical-border ((t (:foreground ,github2-border))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,github2-keyword))))
   `(font-lock-comment-face ((t (:foreground ,github2-comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,github2-comment))))
   `(font-lock-constant-face ((t (:foreground ,github2-constant))))
   `(font-lock-doc-face ((t (:foreground ,github2-string))))
   `(font-lock-function-name-face ((t (:foreground ,github2-function))))
   `(font-lock-keyword-face ((t (:foreground ,github2-keyword))))
   `(font-lock-negation-char-face ((t (:foreground ,github2-keyword))))
   `(font-lock-preprocessor-face ((t (:foreground ,github2-keyword))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,github2-keyword))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,github2-comment))))
   `(font-lock-string-face ((t (:foreground ,github2-string))))
   `(font-lock-type-face ((t (:foreground ,github2-constant))))
   `(font-lock-variable-name-face ((t (:foreground ,github2-text))))
   `(font-lock-warning-face ((t (:foreground ,github2-text))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,github2-text))))
   `(newsticker-default-face ((t (:foreground ,github2-text))))
   `(newsticker-enclosure-face ((t (:foreground ,github2-html-tag))))
   `(newsticker-extra-face ((t (:foreground ,github2-white :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,github2-text))))
   `(newsticker-immortal-item-face ((t (:foreground ,github2-comment))))
   `(newsticker-new-item-face ((t (:foreground ,github2-text))))
   `(newsticker-obsolete-item-face ((t (:foreground ,github2-string))))
   `(newsticker-old-item-face ((t (:foreground ,github2-white))))
   `(newsticker-statistics-face ((t (:foreground ,github2-text))))
   `(newsticker-treeview-face ((t (:foreground ,github2-text))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,github2-comment))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,github2-text))))
   `(newsticker-treeview-new-face ((t (:foreground ,github2-text :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,github2-string))))
   `(newsticker-treeview-old-face ((t (:foreground ,github2-white))))
   `(newsticker-treeview-selection-face ((t (:background ,github2-selection :foreground ,github2-keyword))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,github2-text :background ,github2-white :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,github2-comment :background ,github2-white :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,github2-text :background ,github2-white :inverse-video nil))))
   `(aw-leading-char-face ((t (:foreground ,github2-white :background ,github2-keyword :weight bold))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,github2-text))))
   `(android-mode-error-face ((t (:foreground ,github2-text :weight bold))))
   `(android-mode-info-face ((t (:foreground ,github2-text))))
   `(android-mode-verbose-face ((t (:foreground ,github2-comment))))
   `(android-mode-warning-face ((t (:foreground ,github2-keyword))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,github2-function :weight bold))))
   `(anzu-match-1 ((t (:foreground ,github2-white :background ,github2-comment))))
   `(anzu-match-2 ((t (:foreground ,github2-white :background ,github2-text))))
   `(anzu-match-3 ((t (:foreground ,github2-white :background ,github2-text))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,github2-keyword))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,github2-string :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,github2-keyword))))
   `(font-latex-italic-face ((t (:foreground ,github2-function :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,github2-text))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,github2-keyword :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,github2-string))))
   `(agda2-highlight-symbol-face ((t (:foreground ,github2-text))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,github2-constant))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,github2-text))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,github2-text))))
   `(agda2-highlight-datatype-face ((t (:foreground ,github2-text))))
   `(agda2-highlight-function-face ((t (:foreground ,github2-text))))
   `(agda2-highlight-module-face ((t (:foreground ,github2-constant))))
   `(agda2-highlight-error-face ((t (:foreground ,github2-white :background ,github2-text))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,github2-white :background ,github2-text))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,github2-white :background ,github2-text))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,github2-white :background ,github2-text))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,github2-white :background ,github2-text))))
   `(agda2-highlight-typechecks-face ((t (:background ,github2-text))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,github2-white :foreground ,github2-text))))
   `(ac-completion-face ((t (:background ,github2-selection :foreground ,github2-text))))
   `(ac-selection-face ((t (:background ,github2-selection :foreground ,github2-text))))
   `(popup-tip-face ((t (:background ,github2-text :foreground ,github2-white))))
   `(popup-scroll-bar-foreground-face ((t (:background ,github2-text))))
   `(popup-scroll-bar-background-face ((t (:background ,github2-comment))))
   `(popup-isearch-match ((t (:background ,github2-white :foreground ,github2-text))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground ,github2-text :background ,github2-white :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,github2-html-tag :background ,github2-white :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,github2-keyword :background ,github2-white :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,github2-text :background ,github2-white :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,github2-function :background ,github2-white :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,github2-text :background ,github2-white))))
   `(company-tooltip-annotation ((t (:foreground ,github2-text :background ,github2-white))))
   `(company-tooltip-annotation-selection ((t (:foreground ,github2-text :background ,github2-selection))))
   `(company-tooltip-selection ((t (:foreground ,github2-text :background ,github2-selection))))
   `(company-tooltip-mouse ((t (:background ,github2-selection))))
   `(company-tooltip-common ((t (:foreground ,github2-comment))))
   `(company-tooltip-common-selection ((t (:foreground ,github2-comment))))
   `(company-scrollbar-fg ((t (:background ,github2-text))))
   `(company-scrollbar-bg ((t (:background ,github2-white))))
   `(company-preview ((t (:background ,github2-comment))))
   `(company-preview-common ((t (:foreground ,github2-comment :background ,github2-selection))))
;;;;; bm
   `(bm-face ((t (:background ,github2-text :foreground ,github2-white))))
   `(bm-fringe-face ((t (:background ,github2-text :foreground ,github2-white))))
   `(bm-fringe-persistent-face ((t (:background ,github2-comment :foreground ,github2-white))))
   `(bm-persistent-face ((t (:background ,github2-comment :foreground ,github2-white))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,github2-text :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,github2-text))))
   `(cider-deprecated-face ((t (:background ,github2-text))))
   `(cider-instrumented-face ((t (:box (:color ,github2-string :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,github2-function :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,github2-text))))
   `(cider-test-error-face ((t (:background ,github2-text))))
   `(cider-test-success-face ((t (:background ,github2-comment))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,github2-function))))
   `(circe-my-message-face ((t (:foreground ,github2-text))))
   `(circe-fool-face ((t (:foreground ,github2-text))))
   `(circe-topic-diff-removed-face ((t (:foreground ,github2-string :weight bold))))
   `(circe-originator-face ((t (:foreground ,github2-text))))
   `(circe-server-face ((t (:foreground ,github2-comment))))
   `(circe-topic-diff-new-face ((t (:foreground ,github2-text :weight bold))))
   `(circe-prompt-face ((t (:foreground ,github2-text :background ,github2-white :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,github2-text)))
   `(context-coloring-level-1-face ((t :foreground ,github2-function)))
   `(context-coloring-level-2-face ((t :foreground ,github2-constant)))
   `(context-coloring-level-3-face ((t :foreground ,github2-keyword)))
   `(context-coloring-level-4-face ((t :foreground ,github2-text)))
   `(context-coloring-level-5-face ((t :foreground ,github2-text)))
   `(context-coloring-level-6-face ((t :foreground ,github2-keyword)))
   `(context-coloring-level-7-face ((t :foreground ,github2-comment)))
   `(context-coloring-level-8-face ((t :foreground ,github2-text)))
   `(context-coloring-level-9-face ((t :foreground ,github2-text)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,github2-text :foreground ,github2-white))))
   `(ctbl:face-continue-bar ((t (:background ,github2-highlight :foreground ,github2-white))))
   `(ctbl:face-row-select ((t (:background ,github2-function :foreground ,github2-white))))
;;;;; diff
   `(diff-added          ((t (:background "#335533" :foreground ,github2-comment))))
   `(diff-changed        ((t (:background "#555511" :foreground ,github2-comment))))
   `(diff-removed        ((t (:background "#553333" :foreground ,github2-comment))))
   `(diff-refine-added   ((t (:background "#338833" :foreground ,github2-comment))))
   `(diff-refine-change  ((t (:background "#888811" :foreground ,github2-text))))
   `(diff-refine-removed ((t (:background "#883333" :foreground ,github2-comment))))
   `(diff-header ((,class (:background ,github2-white))
                  (t (:background ,github2-text :foreground ,github2-white))))
   `(diff-file-header
     ((,class (:background ,github2-white :foreground ,github2-text :bold t))
      (t (:background ,github2-text :foreground ,github2-white :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,github2-text :background ,github2-diff-added))))
   `(diff-hl-delete ((,class (:foreground ,github2-text :background ,github2-diff-removed-highlight))))
   `(diff-hl-insert ((,class (:foreground ,github2-text :background ,github2-diff-added-highlight))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,github2-white)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,github2-text))))
   `(diredp-compressed-file-suffix ((t (:foreground ,github2-text))))
   `(diredp-date-time ((t (:foreground ,github2-text))))
   `(diredp-deletion ((t (:foreground ,github2-keyword))))
   `(diredp-deletion-file-name ((t (:foreground ,github2-string))))
   `(diredp-dir-heading ((t (:foreground ,github2-text :background ,github2-selection))))
   `(diredp-dir-priv ((t (:foreground ,github2-function))))
   `(diredp-exec-priv ((t (:foreground ,github2-string))))
   `(diredp-executable-tag ((t (:foreground ,github2-text))))
   `(diredp-file-name ((t (:foreground ,github2-text))))
   `(diredp-file-suffix ((t (:foreground ,github2-comment))))
   `(diredp-flag-mark ((t (:foreground ,github2-keyword))))
   `(diredp-flag-mark-line ((t (:foreground ,github2-text))))
   `(diredp-ignored-file-name ((t (:foreground ,github2-string))))
   `(diredp-link-priv ((t (:foreground ,github2-keyword))))
   `(diredp-mode-line-flagged ((t (:foreground ,github2-keyword))))
   `(diredp-mode-line-marked ((t (:foreground ,github2-text))))
   `(diredp-no-priv ((t (:foreground ,github2-text))))
   `(diredp-number ((t (:foreground ,github2-text))))
   `(diredp-other-priv ((t (:foreground ,github2-text))))
   `(diredp-rare-priv ((t (:foreground ,github2-text))))
   `(diredp-read-priv ((t (:foreground ,github2-comment))))
   `(diredp-symlink ((t (:foreground ,github2-keyword))))
   `(diredp-write-priv ((t (:foreground ,github2-text))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,github2-string :weight bold))))
   `(dired-async-message ((t (:foreground ,github2-keyword :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,github2-keyword))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,github2-text :background ,github2-diff-removed))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,github2-text :background ,github2-text))))
   `(ediff-current-diff-B ((t (:foreground ,github2-text :background ,github2-diff-added))))
   `(ediff-current-diff-C ((t (:foreground ,github2-text :background ,github2-text))))
   `(ediff-even-diff-A ((t (:background ,github2-white))))
   `(ediff-even-diff-Ancestor ((t (:background ,github2-white))))
   `(ediff-even-diff-B ((t (:background ,github2-white))))
   `(ediff-even-diff-C ((t (:background ,github2-white))))
   `(ediff-fine-diff-A ((t (:foreground ,github2-text :background ,github2-diff-removed-highlight :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,github2-text :background ,github2-text weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,github2-text :background ,github2-diff-added-highlight :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,github2-text :background ,github2-text :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,github2-white))))
   `(ediff-odd-diff-Ancestor ((t (:background ,github2-white))))
   `(ediff-odd-diff-B ((t (:background ,github2-white))))
   `(ediff-odd-diff-C ((t (:background ,github2-white))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,github2-text))))
   `(egg-help-header-1 ((t (:foreground ,github2-keyword))))
   `(egg-help-header-2 ((t (:foreground ,github2-html-tag))))
   `(egg-branch ((t (:foreground ,github2-keyword))))
   `(egg-branch-mono ((t (:foreground ,github2-keyword))))
   `(egg-term ((t (:foreground ,github2-keyword))))
   `(egg-diff-add ((t (:foreground ,github2-constant))))
   `(egg-diff-del ((t (:foreground ,github2-text))))
   `(egg-diff-file-header ((t (:foreground ,github2-text))))
   `(egg-section-title ((t (:foreground ,github2-keyword))))
   `(egg-stash-mono ((t (:foreground ,github2-constant))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,github2-string))))
   `(elfeed-log-info-level-face ((t (:foreground ,github2-text))))
   `(elfeed-log-warn-level-face ((t (:foreground ,github2-keyword))))
   `(elfeed-search-date-face ((t (:foreground ,github2-text :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,github2-comment))))
   `(elfeed-search-feed-face ((t (:foreground ,github2-function))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,github2-keyword :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,github2-text
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,github2-text :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,github2-keyword
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,github2-comment :background ,github2-white))))
   `(w3m-lnum-match ((t (:background ,github2-selection
                                     :foreground ,github2-text
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,github2-keyword))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,github2-text :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,github2-text))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,github2-keyword))))
   `(erc-keyword-face ((t (:foreground ,github2-text :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,github2-keyword :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,github2-string :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,github2-comment))))
   `(erc-pal-face ((t (:foreground ,github2-text :weight bold))))
   `(erc-prompt-face ((t (:foreground ,github2-text :background ,github2-white :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,github2-constant))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,github2-constant :background ,github2-white))))
   `(ert-test-result-unexpected ((t (:foreground ,github2-string :background ,github2-white))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,github2-keyword :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,github2-text :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,github2-keyword :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,github2-text :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,github2-text))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,github2-keyword :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,github2-function :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,github2-comment :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github2-text) :inherit unspecified))
      (t (:foreground ,github2-text :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github2-keyword) :inherit unspecified))
      (t (:foreground ,github2-keyword :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github2-function) :inherit unspecified))
      (t (:foreground ,github2-function :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,github2-text :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,github2-keyword :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,github2-function :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github2-string)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,github2-text :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github2-text)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,github2-text :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github2-comment)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,github2-comment :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github2-text) :inherit unspecified))
      (t (:foreground ,github2-text :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github2-string) :inherit unspecified))
      (t (:foreground ,github2-text :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,github2-text))))
   `(ack-file ((t (:foreground ,github2-text))))
   `(ack-line ((t (:foreground ,github2-keyword))))
   `(ack-match ((t (:foreground ,github2-text :background ,github2-selection :weight bold))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,github2-text :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,github2-keyword  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,github2-keyword  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,github2-constant :weight bold))))
   `(git-gutter:deleted ((t (:foreground ,github2-keyword :weight bold))))
   `(git-gutter:modified ((t (:foreground ,github2-string :weight bold))))
   `(git-gutter:unchanged ((t (:foreground ,github2-text :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,github2-comment  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,github2-string :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,github2-text :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, github2-text))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,github2-comment :weight bold))))
   `(gnus-server-denied ((t (:foreground ,github2-text :weight bold))))
   `(gnus-server-closed ((t (:foreground ,github2-text :slant italic))))
   `(gnus-server-offline ((t (:foreground ,github2-keyword :weight bold))))
   `(gnus-server-agent ((t (:foreground ,github2-text :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,github2-text))))
   `(gnus-summary-high-ancient ((t (:foreground ,github2-text))))
   `(gnus-summary-high-read ((t (:foreground ,github2-comment :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,github2-text :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,github2-text :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,github2-text))))
   `(gnus-summary-low-read ((t (:foreground ,github2-comment))))
   `(gnus-summary-low-ticked ((t (:foreground ,github2-text :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,github2-text))))
   `(gnus-summary-normal-ancient ((t (:foreground ,github2-text))))
   `(gnus-summary-normal-read ((t (:foreground ,github2-comment))))
   `(gnus-summary-normal-ticked ((t (:foreground ,github2-text :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,github2-text))))
   `(gnus-summary-selected ((t (:foreground ,github2-keyword :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,github2-text))))
   `(gnus-cite-10 ((t (:foreground ,github2-text))))
   `(gnus-cite-11 ((t (:foreground ,github2-keyword))))
   `(gnus-cite-2 ((t (:foreground ,github2-constant))))
   `(gnus-cite-3 ((t (:foreground ,github2-text))))
   `(gnus-cite-4 ((t (:foreground ,github2-comment))))
   `(gnus-cite-5 ((t (:foreground ,github2-text))))
   `(gnus-cite-6 ((t (:foreground ,github2-comment))))
   `(gnus-cite-7 ((t (:foreground ,github2-string))))
   `(gnus-cite-8 ((t (:foreground ,github2-text))))
   `(gnus-cite-9 ((t (:foreground ,github2-text))))
   `(gnus-group-news-1-empty ((t (:foreground ,github2-keyword))))
   `(gnus-group-news-2-empty ((t (:foreground ,github2-html-tag))))
   `(gnus-group-news-3-empty ((t (:foreground ,github2-text))))
   `(gnus-group-news-4-empty ((t (:foreground ,github2-text))))
   `(gnus-group-news-5-empty ((t (:foreground ,github2-text))))
   `(gnus-group-news-6-empty ((t (:foreground ,github2-white))))
   `(gnus-group-news-low-empty ((t (:foreground ,github2-white))))
   `(gnus-signature ((t (:foreground ,github2-keyword))))
   `(gnus-x ((t (:background ,github2-text :foreground ,github2-white))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,github2-text))))
   `(guide-key/key-face ((t (:foreground ,github2-comment))))
   `(guide-key/prefix-command-face ((t (:foreground ,github2-text))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,github2-comment
                      :background ,github2-white
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,github2-keyword
                      :background ,github2-selection
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,github2-highlight :underline nil))))
   `(helm-selection-line ((t (:background ,github2-white))))
   `(helm-visible-mark ((t (:foreground ,github2-white :background ,github2-text))))
   `(helm-candidate-number ((t (:foreground ,github2-constant :background ,github2-selection))))
   `(helm-separator ((t (:foreground ,github2-string :background ,github2-white))))
   `(helm-time-zone-current ((t (:foreground ,github2-comment :background ,github2-white))))
   `(helm-time-zone-home ((t (:foreground ,github2-string :background ,github2-white))))
   `(helm-bookmark-addressbook ((t (:foreground ,github2-text :background ,github2-white))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,github2-text :background ,github2-white))))
   `(helm-bookmark-info ((t (:foreground ,github2-comment :background ,github2-white))))
   `(helm-bookmark-man ((t (:foreground ,github2-keyword :background ,github2-white))))
   `(helm-bookmark-w3m ((t (:foreground ,github2-text :background ,github2-white))))
   `(helm-buffer-not-saved ((t (:foreground ,github2-string :background ,github2-white))))
   `(helm-buffer-process ((t (:foreground ,github2-function :background ,github2-white))))
   `(helm-buffer-saved-out ((t (:foreground ,github2-text :background ,github2-white))))
   `(helm-buffer-size ((t (:foreground ,github2-text :background ,github2-white))))
   `(helm-ff-directory ((t (:foreground ,github2-function :background ,github2-white :weight bold))))
   `(helm-ff-file ((t (:foreground ,github2-text :background ,github2-white :weight normal))))
   `(helm-ff-executable ((t (:foreground ,github2-comment :background ,github2-white :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,github2-string :background ,github2-white :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,github2-keyword :background ,github2-white :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,github2-white :background ,github2-keyword :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,github2-function :background ,github2-white))))
   `(helm-grep-file ((t (:foreground ,github2-text :background ,github2-white))))
   `(helm-grep-finish ((t (:foreground ,github2-comment :background ,github2-white))))
   `(helm-grep-lineno ((t (:foreground ,github2-text :background ,github2-white))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,github2-string :background ,github2-white))))
   `(helm-match ((t (:foreground ,github2-text :background ,github2-selection :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,github2-function :background ,github2-white))))
   `(helm-mu-contacts-address-face ((t (:foreground ,github2-text :background ,github2-white))))
   `(helm-mu-contacts-name-face ((t (:foreground ,github2-text :background ,github2-white))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,github2-text :background ,github2-white))))
   `(helm-swoop-target-word-face ((t (:foreground ,github2-keyword :background ,github2-white :weight bold))))
;;;;; highlight-numbers
   `(highlight-numbers-number ((t (:foreground ,github2-constant))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,github2-highlight))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,github2-highlight)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,github2-white))
                   (t :weight bold)))
;;;;; hlinum
   `(linum-highlight-face ((t (:foreground ,github2-comment :background ,github2-highlight))))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,github2-text :background ,github2-white))))
   `(hydra-face-amaranth ((t (:foreground ,github2-text :background ,github2-white))))
   `(hydra-face-blue ((t (:foreground ,github2-text :background ,github2-white))))
   `(hydra-face-pink ((t (:foreground ,github2-text :background ,github2-white))))
   `(hydra-face-teal ((t (:foreground ,github2-function :background ,github2-white))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,github2-comment :background ,github2-white))))
   `(ivy-match-required-face ((t (:foreground ,github2-string :background ,github2-white))))
   `(ivy-remote ((t (:foreground ,github2-text :background ,github2-white))))
   `(ivy-subdir ((t (:foreground ,github2-keyword :background ,github2-white))))
   `(ivy-current-match ((t (:foreground ,github2-keyword :weight bold :underline t))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,github2-white))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,github2-comment))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,github2-comment))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,github2-text))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,github2-keyword :weight bold))))
   `(ido-only-match ((t (:foreground ,github2-text :weight bold))))
   `(ido-subdir ((t (:foreground ,github2-keyword))))
   `(ido-indicator ((t (:foreground ,github2-keyword :background ,github2-text))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,github2-white :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,github2-comment))))
   `(jabber-roster-user-online ((t (:foreground ,github2-constant))))
   `(jabber-roster-user-dnd ((t (:foreground ,github2-text))))
   `(jabber-roster-user-xa ((t (:foreground ,github2-text))))
   `(jabber-roster-user-chatty ((t (:foreground ,github2-text))))
   `(jabber-roster-user-error ((t (:foreground ,github2-text))))
   `(jabber-rare-time-face ((t (:foreground ,github2-text))))
   `(jabber-chat-prompt-local ((t (:foreground ,github2-constant))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,github2-text))))
   `(jabber-chat-prompt-system ((t (:foreground ,github2-html-tag))))
   `(jabber-activity-face((t (:foreground ,github2-text))))
   `(jabber-activity-personal-face ((t (:foreground ,github2-keyword))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,github2-text))))
   `(js2-error ((t (:foreground ,github2-string :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,github2-comment))))
   `(js2-jsdoc-type ((t (:foreground ,github2-comment))))
   `(js2-jsdoc-value ((t (:foreground ,github2-html-tag))))
   `(js2-function-param ((t (:foreground, github2-text))))
   `(js2-external-variable ((t (:foreground ,github2-text))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,github2-comment))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,github2-text))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,github2-text))))
   `(js2-object-property ((t (:foreground ,github2-keyword))))
   `(js2-magic-paren ((t (:foreground ,github2-text))))
   `(js2-private-function-call ((t (:foreground ,github2-function))))
   `(js2-function-call ((t (:foreground ,github2-function))))
   `(js2-private-member ((t (:foreground ,github2-constant))))
   `(js2-keywords ((t (:foreground ,github2-text))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,github2-text :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,github2-text :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,github2-white))))
   `(ledger-font-pending-face ((t (:foreground ,github2-text weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,github2-text))))
   `(ledger-font-posting-account-face ((t (:foreground ,github2-constant))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,github2-text))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,github2-text))))
   `(ledger-font-posting-amount-face ((t (:foreground ,github2-text))))
   `(ledger-occur-narrowed-face ((t (:foreground ,github2-text :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,github2-white))))
   `(ledger-font-comment-face ((t (:foreground ,github2-comment))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,github2-text :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,github2-text :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,github2-text :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,github2-text :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,github2-comment :background ,github2-white))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,github2-highlight :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,github2-white :background ,github2-text))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,github2-keyword))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,github2-text))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,github2-keyword))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,github2-keyword :box t))))
   `(ruler-mode-default ((t (:foreground ,github2-comment :background ,github2-white))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,github2-constant))))
   `(lui-hilight-face ((t (:foreground ,github2-comment :background ,github2-white))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,github2-comment :background ,github2-selection))))
   `(macrostep-gensym-2
     ((t (:foreground ,github2-text :background ,github2-selection))))
   `(macrostep-gensym-3
     ((t (:foreground ,github2-keyword :background ,github2-selection))))
   `(macrostep-gensym-4
     ((t (:foreground ,github2-text :background ,github2-selection))))
   `(macrostep-gensym-5
     ((t (:foreground ,github2-keyword :background ,github2-selection))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,github2-white))))
   `(magit-section-heading             ((t (:foreground ,github2-keyword :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,github2-text :weight bold))))
   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,github2-white  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,github2-white
                                                        :foreground ,github2-text :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,github2-white))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,github2-white))))
   `(magit-diff-hunk-heading-selection ((t (:background ,github2-white
                                                        :foreground ,github2-text))))
   `(magit-diff-lines-heading          ((t (:background ,github2-text
                                                        :foreground ,github2-white))))
   `(magit-diff-context-highlight      ((t (:background ,github2-white
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,github2-constant))))
   `(magit-diffstat-removed ((t (:foreground ,github2-string))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,github2-keyword  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,github2-comment :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,github2-comment   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,github2-text    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,github2-text  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,github2-comment  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,github2-string    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,github2-text))))
   `(magit-log-date      ((t (:foreground ,github2-text))))
   `(magit-log-graph     ((t (:foreground ,github2-text))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,github2-text))))
   `(magit-sequence-stop ((t (:foreground ,github2-comment))))
   `(magit-sequence-part ((t (:foreground ,github2-keyword))))
   `(magit-sequence-head ((t (:foreground ,github2-text))))
   `(magit-sequence-drop ((t (:foreground ,github2-string))))
   `(magit-sequence-done ((t (:foreground ,github2-text))))
   `(magit-sequence-onto ((t (:foreground ,github2-text))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,github2-comment))))
   `(magit-bisect-skip ((t (:foreground ,github2-keyword))))
   `(magit-bisect-bad  ((t (:foreground ,github2-string))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,github2-selection :foreground ,github2-text))))
   `(magit-blame-hash    ((t (:background ,github2-selection :foreground ,github2-text))))
   `(magit-blame-name    ((t (:background ,github2-selection :foreground ,github2-text))))
   `(magit-blame-date    ((t (:background ,github2-selection :foreground ,github2-text))))
   `(magit-blame-summary ((t (:background ,github2-selection :foreground ,github2-text
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,github2-text))))
   `(magit-hash           ((t (:foreground ,github2-text))))
   `(magit-tag            ((t (:foreground ,github2-text :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,github2-comment  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,github2-text   :weight bold))))
   `(magit-branch-current ((t (:foreground ,github2-text   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,github2-text   :weight bold))))
   `(magit-refname        ((t (:background ,github2-white :foreground ,github2-text :weight bold))))
   `(magit-refname-stash  ((t (:background ,github2-white :foreground ,github2-text :weight bold))))
   `(magit-refname-wip    ((t (:background ,github2-white :foreground ,github2-text :weight bold))))
   `(magit-signature-good      ((t (:foreground ,github2-comment))))
   `(magit-signature-bad       ((t (:foreground ,github2-string))))
   `(magit-signature-untrusted ((t (:foreground ,github2-keyword))))
   `(magit-cherry-unmatched    ((t (:foreground ,github2-function))))
   `(magit-cherry-equivalent   ((t (:foreground ,github2-text))))
   `(magit-reflog-commit       ((t (:foreground ,github2-comment))))
   `(magit-reflog-amend        ((t (:foreground ,github2-text))))
   `(magit-reflog-merge        ((t (:foreground ,github2-comment))))
   `(magit-reflog-checkout     ((t (:foreground ,github2-text))))
   `(magit-reflog-reset        ((t (:foreground ,github2-string))))
   `(magit-reflog-rebase       ((t (:foreground ,github2-text))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,github2-comment))))
   `(magit-reflog-remote       ((t (:foreground ,github2-function))))
   `(magit-reflog-other        ((t (:foreground ,github2-function))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,github2-text))))
   `(message-header-other ((t (:foreground ,github2-comment))))
   `(message-header-to ((t (:foreground ,github2-keyword :weight bold))))
   `(message-header-cc ((t (:foreground ,github2-keyword :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,github2-keyword :weight bold))))
   `(message-header-subject ((t (:foreground ,github2-text :weight bold))))
   `(message-header-xheader ((t (:foreground ,github2-comment))))
   `(message-mml ((t (:foreground ,github2-keyword :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,github2-text))))
   `(mew-face-header-from ((t (:foreground ,github2-keyword))))
   `(mew-face-header-date ((t (:foreground ,github2-comment))))
   `(mew-face-header-to ((t (:foreground ,github2-string))))
   `(mew-face-header-key ((t (:foreground ,github2-comment))))
   `(mew-face-header-private ((t (:foreground ,github2-comment))))
   `(mew-face-header-important ((t (:foreground ,github2-text))))
   `(mew-face-header-marginal ((t (:foreground ,github2-text :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,github2-string))))
   `(mew-face-header-xmew ((t (:foreground ,github2-comment))))
   `(mew-face-header-xmew-bad ((t (:foreground ,github2-string))))
   `(mew-face-body-url ((t (:foreground ,github2-text))))
   `(mew-face-body-comment ((t (:foreground ,github2-text :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,github2-comment))))
   `(mew-face-body-cite2 ((t (:foreground ,github2-text))))
   `(mew-face-body-cite3 ((t (:foreground ,github2-text))))
   `(mew-face-body-cite4 ((t (:foreground ,github2-keyword))))
   `(mew-face-body-cite5 ((t (:foreground ,github2-string))))
   `(mew-face-mark-review ((t (:foreground ,github2-text))))
   `(mew-face-mark-escape ((t (:foreground ,github2-comment))))
   `(mew-face-mark-delete ((t (:foreground ,github2-string))))
   `(mew-face-mark-unlink ((t (:foreground ,github2-keyword))))
   `(mew-face-mark-refile ((t (:foreground ,github2-comment))))
   `(mew-face-mark-unread ((t (:foreground ,github2-text))))
   `(mew-face-eof-message ((t (:foreground ,github2-comment))))
   `(mew-face-eof-part ((t (:foreground ,github2-keyword))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,github2-function :background ,github2-white :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,github2-white :background ,github2-text :weight bold))))
   `(paren-face-no-match ((t (:foreground ,github2-white :background ,github2-string :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,github2-text))))
   `(mingus-pausing-face ((t (:foreground ,github2-text))))
   `(mingus-playing-face ((t (:foreground ,github2-function))))
   `(mingus-playlist-face ((t (:foreground ,github2-function ))))
   `(mingus-song-file-face ((t (:foreground ,github2-keyword))))
   `(mingus-stopped-face ((t (:foreground ,github2-string))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,github2-keyword))))
   `(nav-face-button-num ((t (:foreground ,github2-function))))
   `(nav-face-dir ((t (:foreground ,github2-comment))))
   `(nav-face-hdir ((t (:foreground ,github2-string))))
   `(nav-face-file ((t (:foreground ,github2-text))))
   `(nav-face-hfile ((t (:foreground ,github2-text))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,github2-text    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,github2-comment :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,github2-text  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,github2-comment   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,github2-text  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,github2-comment :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,github2-text    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,github2-text))))
   `(mu4e-trashed-face ((t (:foreground ,github2-text :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,github2-selection))))
   `(mumamo-background-chunk-submode2 ((t (:background ,github2-white))))
   `(mumamo-background-chunk-submode3 ((t (:background ,github2-white))))
   `(mumamo-background-chunk-submode4 ((t (:background ,github2-white))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,github2-keyword :weight bold))))
   `(neo-header-face ((t (:foreground ,github2-text))))
   `(neo-root-dir-face ((t (:foreground ,github2-keyword :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,github2-text))))
   `(neo-file-link-face ((t (:foreground ,github2-text))))
   `(neo-expand-btn-face ((t (:foreground ,github2-text))))
   `(neo-vc-default-face ((t (:foreground ,github2-text))))
   `(neo-vc-user-face ((t (:foreground ,github2-string :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,github2-text))))
   `(neo-vc-edited-face ((t (:foreground ,github2-text))))
   `(neo-vc-needs-merge-face ((t (:foreground ,github2-text))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,github2-string :background ,github2-text))))
   `(neo-vc-added-face ((t (:foreground ,github2-text))))
   `(neo-vc-conflict-face ((t (:foreground ,github2-text))))
   `(neo-vc-missing-face ((t (:foreground ,github2-text))))
   `(neo-vc-ignored-face ((t (:foreground ,github2-text))))
;;;;; org-mode
   `(org-block-background ((t (:background "#f0f0f0"))))
   `(org-block ((t (:background "#f0f0f0"))))
   `(org-block-begin-line ((t (:background "#e0e0e0" :slant italic))))
   `(org-block-end-line ((t (:background "#e0e0e0" :slant italic))))
   `(org-agenda-clocking
     ((t (:bold t :background ,github2-highlight))) t)
   `(org-agenda-date-today
     ((t (:foreground ,github2-text :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,github2-text :weight bold))))
   `(org-checkbox ((t (:background ,github2-white :foreground ,github2-text
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,github2-text :underline t))))
   `(org-deadline-announce ((t (:foreground ,github2-text))))
   `(org-done ((t (:bold t :weight bold :foreground ,github2-html-tag))))
   `(org-formula ((t (:foreground ,github2-text))))
   `(org-headline-done ((t (:foreground ,github2-html-tag))))
   `(org-hide ((t (:foreground ,github2-selection))))
   `(org-level-1 ((t (:foreground ,github2-keyword))))
   `(org-level-2 ((t (:foreground ,github2-constant))))
   `(org-level-3 ((t (:foreground ,github2-function))))
   `(org-level-4 ((t (:foreground ,github2-text))))
   `(org-level-5 ((t (:foreground ,github2-function))))
   `(org-level-6 ((t (:foreground ,github2-comment))))
   `(org-level-7 ((t (:foreground ,github2-text))))
   `(org-level-8 ((t (:foreground ,github2-text))))
   `(org-link ((t (:foreground ,github2-text :underline t))))
   `(org-scheduled ((t (:foreground ,github2-constant))))
   `(org-scheduled-previously ((t (:foreground ,github2-string))))
   `(org-scheduled-today ((t (:foreground ,github2-keyword))))
   `(org-sexp-date ((t (:foreground ,github2-keyword :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,github2-comment))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,github2-text))))
   `(org-todo ((t (:bold t :foreground ,github2-string :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,github2-string :weight bold :underline nil))))
   `(org-column ((t (:background ,github2-selection))))
   `(org-column-title ((t (:background ,github2-selection :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,github2-text :background ,github2-selection))))
   `(org-mode-line-clock-overrun ((t (:foreground ,github2-white :background ,github2-text))))
   `(org-ellipsis ((t (:foreground ,github2-text :underline t))))
   `(org-footnote ((t (:foreground ,github2-function :underline t))))
   `(org-document-title ((t (:foreground ,github2-text))))
   `(org-document-info ((t (:foreground ,github2-text))))
   `(org-habit-ready-face ((t :background ,github2-comment)))
   `(org-habit-alert-face ((t :background ,github2-text :foreground ,github2-white)))
   `(org-habit-clear-face ((t :background ,github2-text)))
   `(org-habit-overdue-face ((t :background ,github2-text)))
   `(org-habit-clear-future-face ((t :background ,github2-text)))
   `(org-habit-ready-future-face ((t :background ,github2-comment)))
   `(org-habit-alert-future-face ((t :background ,github2-text :foreground ,github2-white)))
   `(org-habit-overdue-future-face ((t :background ,github2-text)))
;;;;; outline
   `(outline-1 ((t (:foreground ,github2-text))))
   `(outline-2 ((t (:foreground ,github2-constant))))
   `(outline-3 ((t (:foreground ,github2-constant))))
   `(outline-4 ((t (:foreground ,github2-text))))
   `(outline-5 ((t (:foreground ,github2-function))))
   `(outline-6 ((t (:foreground ,github2-comment))))
   `(outline-7 ((t (:foreground ,github2-text))))
   `(outline-8 ((t (:foreground ,github2-text))))
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
   `(persp-selected-face ((t (:foreground ,github2-keyword :weight bold))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,github2-string :inherit mode-line))))
   `(powerline-active2 ((t (:background ,github2-keyword :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,github2-white :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,github2-white :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,github2-text :background ,github2-white))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,github2-white :background ,github2-text))))
   `(proof-error-face ((t (:foreground ,github2-text :background ,github2-text))))
   `(proof-highlight-dependency-face ((t (:foreground ,github2-white :background ,github2-comment))))
   `(proof-highlight-dependent-face ((t (:foreground ,github2-white :background ,github2-comment))))
   `(proof-locked-face ((t (:background ,github2-comment))))
   `(proof-mouse-highlight-face ((t (:foreground ,github2-white :background ,github2-comment))))
   `(proof-queue-face ((t (:background ,github2-comment))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,github2-comment))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,github2-white))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,github2-white))))
   `(proof-warning-face ((t (:foreground ,github2-white :background ,github2-comment))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,github2-comment))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,github2-constant))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,github2-comment))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,github2-function))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,github2-comment))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,github2-keyword))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,github2-comment))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,github2-comment))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,github2-comment))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,github2-comment))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,github2-comment))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,github2-comment))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,github2-comment))))
   `(rcirc-other-nick ((t (:foreground ,github2-comment))))
   `(rcirc-bright-nick ((t (:foreground ,github2-keyword))))
   `(rcirc-dim-nick ((t (:foreground ,github2-comment))))
   `(rcirc-server ((t (:foreground ,github2-comment))))
   `(rcirc-server-prefix ((t (:foreground ,github2-comment))))
   `(rcirc-timestamp ((t (:foreground ,github2-comment))))
   `(rcirc-nick-in-message ((t (:foreground ,github2-keyword))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,github2-keyword :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,github2-keyword :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,github2-comment))))
   `(rpm-spec-doc-face ((t (:foreground ,github2-comment))))
   `(rpm-spec-ghost-face ((t (:foreground ,github2-string))))
   `(rpm-spec-macro-face ((t (:foreground ,github2-keyword))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,github2-string))))
   `(rpm-spec-package-face ((t (:foreground ,github2-string))))
   `(rpm-spec-section-face ((t (:foreground ,github2-keyword))))
   `(rpm-spec-tag-face ((t (:foreground ,github2-comment))))
   `(rpm-spec-var-face ((t (:foreground ,github2-string))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,github2-comment))))
   `(rst-level-2-face ((t (:foreground ,github2-comment))))
   `(rst-level-3-face ((t (:foreground ,github2-constant))))
   `(rst-level-4-face ((t (:foreground ,github2-comment))))
   `(rst-level-5-face ((t (:foreground ,github2-function))))
   `(rst-level-6-face ((t (:foreground ,github2-comment))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,github2-keyword :bold t))))
   `(sh-quoted-exec ((t (:foreground ,github2-string))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,github2-comment :background ,github2-white :weight bold))))
   `(show-paren-match ((t (:foreground ,github2-white :background ,github2-keyword :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Github2 for sml
   `(sml/global ((,class (:foreground ,github2-comment :weight bold))))
   `(sml/modes ((,class (:foreground ,github2-keyword :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,github2-comment :weight bold))))
   `(sml/filename ((,class (:foreground ,github2-keyword :weight bold))))
   `(sml/line-number ((,class (:foreground ,github2-comment :weight bold))))
   `(sml/col-number ((,class (:foreground ,github2-keyword :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,github2-constant :weight bold))))
   `(sml/prefix ((,class (:foreground ,github2-comment))))
   `(sml/git ((,class (:foreground ,github2-html-tag))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,github2-comment :weight bold))))
   `(sml/read-only ((,class (:foreground ,github2-comment))))
   `(sml/outside-modified ((,class (:foreground ,github2-comment))))
   `(sml/modified ((,class (:foreground ,github2-string))))
   `(sml/vc-edited ((,class (:foreground ,github2-comment))))
   `(sml/charging ((,class (:foreground ,github2-constant))))
   `(sml/discharging ((,class (:foreground ,github2-comment))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:inverse-video t :foreground ,github2-keyword :background ,github2-white :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,github2-selection :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,github2-string))))
   `(slime-repl-inputed-output-face ((t (:foreground ,github2-comment))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github2-string)))
      (t
       (:underline ,github2-string))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github2-comment)))
      (t
       (:underline ,github2-comment))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github2-keyword)))
      (t
       (:underline ,github2-keyword))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,github2-comment)))
      (t
       (:underline ,github2-comment))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,github2-comment))))
   `(speedbar-directory-face ((t (:foreground ,github2-function))))
   `(speedbar-file-face ((t (:foreground ,github2-comment))))
   `(speedbar-highlight-face ((t (:foreground ,github2-white :background ,github2-comment))))
   `(speedbar-selected-face ((t (:foreground ,github2-string))))
   `(speedbar-separator-face ((t (:foreground ,github2-white :background ,github2-constant))))
   `(speedbar-tag-face ((t (:foreground ,github2-keyword))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,github2-comment
                                    :background ,github2-white))))
   `(tabbar-selected ((t (:foreground ,github2-comment
                                      :background ,github2-white
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,github2-comment
                                        :background ,github2-white
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,github2-white
                                       :background ,github2-selection))))
   `(term-color-red ((t (:foreground ,github2-comment
                                     :background ,github2-comment))))
   `(term-color-green ((t (:foreground ,github2-comment
                                       :background ,github2-comment))))
   `(term-color-yellow ((t (:foreground ,github2-comment
                                        :background ,github2-keyword))))
   `(term-color-blue ((t (:foreground ,github2-constant
                                      :background ,github2-comment))))
   `(term-color-magenta ((t (:foreground ,github2-comment
                                         :background ,github2-string))))
   `(term-color-cyan ((t (:foreground ,github2-function
                                      :background ,github2-comment))))
   `(term-color-white ((t (:foreground ,github2-comment
                                       :background ,github2-comment))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,github2-comment :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,github2-comment :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,github2-comment))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,github2-keyword))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,github2-function))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,github2-highlight))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,github2-comment ))))
   `(web-mode-css-prop-face ((t (:foreground ,github2-constant))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,github2-html-tag :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,github2-html-tag))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,github2-comment))))
   `(web-mode-html-attr-name-face ((t (:foreground ,github2-function))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,github2-html-tag))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,github2-white))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,github2-string))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,github2-white :foreground ,github2-white))))
   `(whitespace-hspace ((t (:background ,github2-white :foreground ,github2-white))))
   `(whitespace-tab ((t (:background ,github2-comment))))
   `(whitespace-newline ((t (:foreground ,github2-white))))
   `(whitespace-trailing ((t (:background ,github2-string))))
   `(whitespace-line ((t (:background ,github2-white :foreground ,github2-comment))))
   `(whitespace-space-before-tab ((t (:background ,github2-comment :foreground ,github2-comment))))
   `(whitespace-indentation ((t (:background ,github2-keyword :foreground ,github2-string))))
   `(whitespace-empty ((t (:background ,github2-keyword))))
   `(whitespace-space-after-tab ((t (:background ,github2-keyword :foreground ,github2-string))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,github2-comment))))
   `(wl-highlight-folder-many-face ((t (:foreground ,github2-comment))))
   `(wl-highlight-folder-path-face ((t (:foreground ,github2-comment))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,github2-comment))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,github2-comment))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,github2-comment))))
   `(wl-highlight-message-citation-header ((t (:foreground ,github2-comment))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,github2-string))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,github2-comment))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,github2-comment))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,github2-keyword))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,github2-comment))))
   `(wl-highlight-message-headers-face ((t (:foreground ,github2-comment))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,github2-comment))))
   `(wl-highlight-message-header-contents ((t (:foreground ,github2-comment))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,github2-comment))))
   `(wl-highlight-message-signature ((t (:foreground ,github2-comment))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,github2-comment))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,github2-comment))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,github2-comment
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,github2-comment))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,github2-comment))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,github2-keyword))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,github2-comment))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,github2-comment))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,github2-constant))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,github2-keyword :weight bold))))
   `(cscope-function-face ((t (:foreground ,github2-function :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,github2-string :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,github2-white :background ,github2-keyword))))
   `(cscope-separator-face ((t (:foreground ,github2-string :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,github2-selection))))
   `(yascroll:thumb-fringe ((t (:background ,github2-selection :foreground ,github2-selection))))

;;;;; elscreen
  `(elscreen-tab-background-face ((t (:background ,github2-keyword))))
  `(elscreen-tab-control-face ((t (:foreground ,github2-white :background ,github2-keyword))))
  `(elscreen-tab-current-screen-face ((t (:foreground ,github2-black :background ,github2-selection))))
  `(elscreen-tab-other-screen-face ((t (:foreground ,github2-text :background ,github2-highlight))))
  ))

;;; Theme Variables
(github2-with-color-variables
  (custom-theme-set-variables
   'github2
;;;;; ansi-color
   `(ansi-color-names-vector [,github2-white ,github2-string ,github2-comment ,github2-keyword
                                          ,github2-comment ,github2-comment ,github2-function ,github2-comment])
;;;;; fill-column-indicator
   `(fci-rule-color ,github2-comment)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,github2-string ,github2-comment ,github2-keyword ,github2-comment ,github2-constant
                    ,github2-function ,github2-keyword ,github2-comment))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,github2-comment . ,github2-highlight))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,github2-comment)
       ( 40. . ,github2-string)
       ( 60. . ,github2-comment)
       ( 80. . ,github2-comment)
       (100. . ,github2-comment)
       (120. . ,github2-keyword)
       (140. . ,github2-comment)
       (160. . ,github2-comment)
       (180. . ,github2-comment)
       (200. . ,github2-comment)
       (220. . ,github2-html-tag)
       (240. . ,github2-constant)
       (260. . ,github2-function)
       (280. . ,github2-comment)
       (300. . ,github2-constant)
       (320. . ,github2-comment)
       (340. . ,github2-keyword)
       (360. . ,github2-comment)))
   `(vc-annotate-very-old-color ,github2-comment)
   `(vc-annotate-background ,github2-selection)
   ))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'github2)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; github2-theme.el ends here
