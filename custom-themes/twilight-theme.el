;;; twilight-theme.el --- Twilight Color Theme for Emacs.

;; Copyright (C) 2008 Marcus Crafter <crafterm@redartisan.com>
;; Copyright (C) 2015-2017 Jason Blevins <jrblevin@sdf.org>

;; Author: Marcus Crafter
;;	Jason Blevins
;; Adapted-By: Yesudeep Mangalapilly
;; Keywords: textmate twilight color theme
;; URL: https://github.com/jrblevin/twilight-emacs
;; Version: 1.1

;; This file is NOT a part of GNU Emacs.

;;; License:

;; MIT License
;; -----------
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or
;; sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice shall
;; be included in all copies or substantial portions of the
;; Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; Usage:
;;
;; Defines a colour scheme resembling that of the original TextMate
;; Twilight colour theme.  To use add the file to your
;; `custom-theme-directory' and run `M-x customize-themes'.
;; Alternatively, you may add the following to your init file:
;;
;;     (load-theme 'twilight t)
;;
;; Credits due to the excellent TextMate Twilight theme.
;;
;; Thanks to Travis Jeffery for ido-mode and fixes to the
;; minibuffer-prompt to fit in with the rest of the theme.

;;; Code:

(deftheme twilight
  "TextMate Twilight theme for GNU Emacs.")

(let ((tw-bg-2        "#000000")
      (tw-bg-1        "#111111")
      (tw-bg          "#141414")
      (tw-bg+1        "#191919")
      (tw-bg+2        "#212121")
      (tw-bg+3        "#313131")
      (tw-bg+4        "#4b474c")
      (tw-bg+5        "#5f5a60")
      (tw-bg+8        "#8f8a80")
      (tw-fg-d        "#d4d0c8")
      (tw-fg-c        "#cacaca")
      (tw-fg          "#d8d8d8")
      (tw-fg+1        "#e8e8e8")
      (tw-fg+2        "#f8f8f8")
      (tw-wt          "#ffffff")
      (tw-rd-1        "#a3472c")
      (tw-rd          "#cf6a4c")
      (tw-mg-1        "#ef2929")
      (tw-mg          "#ee799f")
      (tw-dark-org    "#9b703f")
      (tw-med-org     "#efa510")
      (tw-light-org   "#f2b73f")
      (tw-pale-org    "#cda869")
      (tw-dark-grn-2  "#4b5335")
      (tw-dark-grn-1  "#5a6340")
      (tw-dark-grn    "#646d4a")
      (tw-med-grn     "#8f9d6a")
      (tw-blu-1       "#5d6c84")
      (tw-blu         "#7587A6")
      (tw-cy-1        "#41595c")
      (tw-cy          "#5d8084")
      (tw-pale-pur    "#89788a")
      (tw-med-pur     "#9b859d"))

  (custom-theme-set-variables
   'twilight
   '(frame-brackground-mode (quote dark)))

  (custom-theme-set-faces
   'twilight
   `(default ((t (:background ,tw-bg+1 :foreground ,tw-fg))))
   `(cursor ((t (:background ,tw-fg))))
   `(buffers-tab ((t (:background ,tw-bg :foreground ,tw-fg-c))))

   `(font-lock-warning-face ((t (:background ,tw-mg :foreground ,tw-bg-2))))
   `(font-lock-builtin-face ((t (:foreground ,tw-pale-pur))))
   `(font-lock-comment-face ((t (:foreground ,tw-bg+5 :italic t))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,tw-bg+5))))
   `(font-lock-constant-face ((t (:foreground ,tw-rd))))
   `(font-lock-doc-string-face ((t (:foreground ,tw-med-org))))
   `(font-lock-function-name-face ((t (:foreground ,tw-dark-org))))
   `(font-lock-keyword-face ((t (:foreground ,tw-pale-org))))
   `(font-lock-preprocessor-face ((t (:foreground ,tw-rd))))
   `(font-lock-string-face ((t (:foreground ,tw-med-grn))))
   `(font-lock-type-face ((t (:foreground ,tw-pale-pur))))
   `(font-lock-variable-name-face ((t (:foreground ,tw-blu))))
   `(font-lock-warning-face ((t (:background ,tw-mg :foreground ,tw-mg-1))))
   `(font-lock-reference-face ((t (:inherit font-lock-constant-face)))) ; obsolete
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,tw-light-org))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,tw-mg-1))))

   `(minibuffer-prompt ((t (:foreground ,tw-bg+5))))
   `(fringe ((t (:background ,tw-bg+3 :foreground ,tw-bg+8))))
   `(linum ((t (:background ,tw-bg :foreground ,tw-bg+3))))
   `(linum-highlight-face ((t (:inherit linum :foreground ,tw-pale-org))))
   `(hl-line ((t (:background ,tw-bg+2))))
   `(mode-line ((t (:background ,tw-bg+3 :foreground ,tw-fg-c))))
   `(mode-line-inactive ((t (:background ,tw-bg+4 :foreground ,tw-bg+8))))
   `(mode-line-buffer-id ((t (:foreground ,tw-med-grn))))
   `(gui-element ((t (:background ,tw-fg-d :foreground ,tw-bg-2))))
   `(region ((t (:background ,tw-bg+3))))
   `(shadow ((t (:foreground ,tw-bg+4))))
   `(highlight ((t (:background ,tw-bg-1))))
   `(highline-face ((t (:background ,tw-cy))))
   `(secondary-selection ((t (:background ,tw-blu-1))))
   `(link ((t :foreground ,tw-blu, :underline t)))
   `(link-visited ((t :foreground ,tw-blu-1, :underline t)))
   `(tooltip ((t (:background ,tw-fg-d :foreground ,tw-bg))))
   `(widget-field ((t (:background ,tw-bg+8))))
   `(text-cursor ((t (:background ,tw-pale-org :foreground ,tw-bg-2))))
   `(escape-glyph ((t (:foreground ,tw-cy))))
   `(underline ((nil (:underline nil))))
   `(yas/field-highlight-face ((t (:background ,tw-bg+2))))
   `(mumamo-background-chunk-submode ((t (:background ,tw-bg+2))))
   `(trailing-whitespace ((t (:background ,tw-rd :foreground ,tw-fg))))

   ;; Ido
   `(ido-subdir ((t (:foreground ,tw-rd))))
   `(ido-first-match ((t (:foreground ,tw-med-grn))))
   `(ido-only-match ((t (:foreground ,tw-med-grn))))

   ;; Enhanced-Ruby-Mode
   `(ruby-string-delimiter-face  ((t (:foreground ,tw-dark-grn-1))))
   `(ruby-regexp-delimiter-face ((t (:foreground ,tw-light-org))))
   `(ruby-heredoc-delimiter-face ((t (:foreground ,tw-med-pur))))
   `(ruby-op-face ((t (:foreground ,tw-pale-org))))

   ;; Org
   `(org-hide ((((background dark)) (:foreground ,tw-bg))))
   `(org-clock-overlay ((t (:foreground ,tw-wt))))
   `(outline-4 ((t (:foreground ,tw-bg+8))))

   ;; Diff
   `(diff-header ((t (:background ,tw-bg+5))))
   `(diff-file-header ((t (:inherit diff-header))))
   `(diff-removed ((t (:background ,tw-rd-1))))
   `(diff-changed ((t (:background ,tw-pale-org))))
   `(diff-added ((t (:background  ,tw-dark-grn))))
   `(diff-refine-added ((t (:background ,tw-med-grn))))
   `(diff-refine-change ((t (:background ,tw-light-org))))
   `(diff-refine-removed ((t (:background ,tw-rd))))

   ;; git-commit
   `(git-commit-comment-action ((t (:foreground ,tw-med-grn :weight bold))))
   `(git-commit-comment-branch ((t (:foreground ,tw-blu :weight bold))))
   `(git-commit-comment-heading ((t (:foreground ,tw-pale-org :weight bold))))

   ;; isearch
   `(isearch ((t (:foreground ,tw-bg :background ,tw-light-org))))
   `(isearch-fail ((t (:foreground ,tw-bg :background ,tw-mg-1))))
   `(isearch-lazy-highlight-face ((t (:foreground ,tw-fg :background ,tw-blu))))

   ;; imenu-list
   `(imenu-list-entry-face-0 ((t (:foreground ,tw-pale-org))))
   `(imenu-list-entry-face-1 ((t (:foreground ,tw-light-org))))
   `(imenu-list-entry-face-2 ((t (:foreground ,tw-med-org))))
   `(imenu-list-entry-face-3 ((t (:foreground ,tw-dark-org))))
   `(imenu-list-entry-subalist-face-0 ((t (:inherit imenu-list-entry-face-0 :weight bold))))
   `(imenu-list-entry-subalist-face-1 ((t (:inherit imenu-list-entry-face-1 :weight bold))))
   `(imenu-list-entry-subalist-face-2 ((t (:inherit imenu-list-entry-face-2 :weight bold))))
   `(imenu-list-entry-subalist-face-3 ((t (:inherit imenu-list-entry-face-3 :weight bold))))

   ;; flyspell
   `(flyspell-incorrect ((t (:underline (:style wave :color ,tw-mg-1)))))
   `(flyspell-duplicate ((t (:underline (:style wave :color ,tw-rd)))))

   ;; flycheck
   `(flycheck-error ((t (:underline (:style wave :color ,tw-mg-1)))))
   `(flycheck-warning ((t (:underline (:style wave :color ,tw-rd)))))
   `(flycheck-fringe-error ((t (:foreground ,tw-bg :background ,tw-mg-1))))
   `(flycheck-fringe-info ((t (:foreground ,tw-bg :background ,tw-pale-org))))
   `(flycheck-fringe-warning ((t (:foreground ,tw-bg :background ,tw-rd))))

   ;; AUCTeX
   `(font-latex-bold-face ((t (:foreground ,tw-blu :inherit bold))))
   `(font-latex-doctex-documentation-face ((t (:background ,tw-bg+3))))
   `(font-latex-doctex-preprocessor-face ((t (:foreground ,tw-blu :inherit bold))))
   `(font-latex-italic-face ((t (:foreground ,tw-blu :inherit italic))))
   `(font-latex-math-face ((t (:foreground ,tw-med-grn))))
   `(font-latex-script-char-face ((t (:foreground ,tw-dark-grn-2))))
   `(font-latex-sectioning-5-face ((t (:foreground ,tw-med-org :inherit variable-pitch))))
   `(font-latex-sedate-face ((t (:foreground ,tw-blu))))
   `(font-latex-slide-title-face ((t (:foreground ,tw-blu))))
   `(font-latex-string-face ((t (:inherit font-lock-string-face))))
   `(font-latex-verbatim-face ((t (:foreground ,tw-rd :inherit fixed-pitch))))
   `(font-latex-warning-face ((t (:foreground ,tw-mg-1 :inherit bold))))

   ;; company-mode
   `(company-tooltip ((t (:background ,tw-bg+3 :foreground ,tw-fg))))
   `(company-tooltip-selection ((t (:background ,tw-dark-org))))
   `(company-tooltip-common ((t (:inherit company-tooltip :bold t))))
   `(company-tooltip-annotation ((t (:inherit company-tooltip :foreground ,tw-rd))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-common :background ,tw-dark-org))))
   `(company-tooltip-search ((t (:inherit company-tooltip-selection :foreground ,tw-bg-2))))
   `(company-scrollbar-fg ((t (:background ,tw-bg+8))))
   `(company-scrollbar-bg ((t (:background ,tw-bg+4))))
   `(company-preview ((t (:background ,tw-dark-org))))
   `(company-preview-common ((t (:inherit company-preview :foreground ,tw-wt))))
   `(company-preview-search ((t (:inherit company-preview-common :foreground ,tw-bg-2))))

   ;; magit: headings
   `(magit-section-highlight ((t (:background ,tw-bg+3))))
   `(magit-section-heading ((t (:foreground ,tw-pale-org :weight bold))))
   `(magit-section-heading-selection ((t (:foreground ,tw-med-org :weight bold))))
   ;; magit: diffs
   `(magit-diff-file-heading ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,tw-bg+3 :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,tw-bg+3 :foreground ,tw-med-org :weight bold))))
   `(magit-diff-hunk-heading ((t (:background ,tw-bg+2))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,tw-bg+3))))
   `(magit-diff-hunk-heading-selection ((t (:background ,tw-bg+3 :foreground ,tw-med-org))))
   `(magit-diff-lines-heading ((t (:background ,tw-med-org :foreground ,tw-bg+3))))
   `(magit-diff-context-face ((t (:foreground ,tw-fg-d))))
   `(magit-diff-context-highlight ((t (:background ,tw-bg+3 :foreground ,tw-fg-d))))
   `(magit-diff-added-face ((t (:background ,tw-dark-grn :foreground ,tw-fg))))
   `(magit-diff-added-highlight-face ((t (:background ,tw-med-grn :foreground ,tw-fg))))
   `(magit-diff-removed-face ((t (:background ,tw-rd-1 :foreground ,tw-fg))))
   `(magit-diff-removed-highlight-face ((t (:background ,tw-rd :foreground ,tw-fg))))
   `(magit-diff-base-face ((t (:background ,tw-cy-1 :foreground ,tw-fg))))
   `(magit-diff-base-highlight-face ((t (:background ,tw-cy :foreground ,tw-fg))))
   `(magit-diffstat-added ((t (:foreground ,tw-med-grn))))
   `(magit-diffstat-removed ((t (:foreground ,tw-rd-1))))
   ;; magit: popup
   `(magit-popup-heading ((t (:foreground ,tw-pale-org :weight bold))))
   `(magit-popup-key ((t (:foreground ,tw-dark-grn-1 :weight bold))))
   `(magit-popup-argument ((t (:foreground ,tw-dark-grn :weight bold))))
   `(magit-popup-disabled-argument ((t (:foreground ,tw-fg-c :weight normal))))
   `(magit-popup-option-value ((t (:foreground ,tw-blu-1 :weight bold))))
   ;; magit: process
   `(magit-process-ok ((t (:foreground ,tw-dark-grn :weight bold))))
   `(magit-process-ng ((t (:foreground ,tw-rd :weight bold))))
   ;; magit: log
   `(magit-log-author ((t (:foreground ,tw-med-org))))
   `(magit-log-date ((t (:foreground ,tw-fg-c))))
   `(magit-log-graph ((t (:foreground ,tw-fg+1))))
   ;; magit: sequence
   `(magit-sequence-pick ((t (:foreground ,tw-light-org))))
   `(magit-sequence-stop ((t (:foreground ,tw-dark-grn))))
   `(magit-sequence-part ((t (:foreground ,tw-pale-org))))
   `(magit-sequence-head ((t (:foreground ,tw-blu))))
   `(magit-sequence-drop ((t (:foreground ,tw-rd))))
   `(magit-sequence-done ((t (:foreground ,tw-fg-c))))
   `(magit-sequence-onto ((t (:foreground ,tw-fg-c))))
   ;; magit: bisect
   `(magit-bisect-good ((t (:foreground ,tw-dark-grn))))
   `(magit-bisect-skip ((t (:foreground ,tw-pale-org))))
   `(magit-bisect-bad ((t (:foreground ,tw-rd))))
   ;; magit: blame
   `(magit-blame-heading ((t (:background ,tw-bg-1 :foreground ,tw-blu-1))))
   `(magit-blame-hash ((t (:background ,tw-bg-1 :foreground ,tw-blu-1))))
   `(magit-blame-name ((t (:background ,tw-bg-1 :foreground ,tw-med-org))))
   `(magit-blame-date ((t (:background ,tw-bg-1 :foreground ,tw-med-org))))
   `(magit-blame-summary ((t (:background ,tw-bg-1 :foreground ,tw-blu-1 :weight bold))))
   ;; magit: references etc
   `(magit-dimmed ((t (:foreground ,tw-bg+8))))
   `(magit-hash ((t (:foreground ,tw-bg+8))))
   `(magit-tag ((t (:foreground ,tw-med-org :weight bold))))
   `(magit-branch-remote ((t (:foreground ,tw-dark-grn :weight bold))))
   `(magit-branch-local ((t (:foreground ,tw-blu :weight bold))))
   `(magit-branch-current ((t (:foreground ,tw-blu :weight bold :box t))))
   `(magit-head ((t (:foreground ,tw-blu :weight bold))))
   `(magit-refname ((t (:background ,tw-bg+3 :foreground ,tw-fg :weight bold))))
   `(magit-refname-stash ((t (:background ,tw-bg+3 :foreground ,tw-fg :weight bold))))
   `(magit-refname-wip ((t (:background ,tw-bg+3 :foreground ,tw-fg :weight bold))))
   `(magit-signature-good ((t (:foreground ,tw-dark-grn))))
   `(magit-signature-bad ((t (:foreground ,tw-rd))))
   `(magit-signature-untrusted ((t (:foreground ,tw-pale-org))))
   `(magit-cherry-unmatched ((t (:foreground ,tw-cy))))
   `(magit-cherry-equivalent ((t (:foreground ,tw-mg))))
   `(magit-reflog-commit ((t (:foreground ,tw-dark-grn))))
   `(magit-reflog-amend ((t (:foreground ,tw-mg))))
   `(magit-reflog-merge ((t (:foreground ,tw-dark-grn))))
   `(magit-reflog-checkout ((t (:foreground ,tw-blu))))
   `(magit-reflog-reset ((t (:foreground ,tw-rd))))
   `(magit-reflog-rebase ((t (:foreground ,tw-mg))))
   `(magit-reflog-cherry-pick ((t (:foreground ,tw-dark-grn))))
   `(magit-reflog-remote ((t (:foreground ,tw-cy))))
   `(magit-reflog-other ((t (:foreground ,tw-cy))))

   ;; term
   `(term-color-black ((t (:foreground ,tw-bg+3 :background ,tw-bg))))
   `(term-color-red ((t (:foreground ,tw-rd :background ,tw-rd-1))))
   `(term-color-green ((t (:foreground ,tw-med-grn :background ,tw-dark-grn))))
   `(term-color-yellow ((t (:foreground ,tw-med-org :background ,tw-pale-org))))
   `(term-color-blue ((t (:foreground ,tw-blu :background ,tw-blu-1))))
   `(term-color-magenta ((t (:foreground ,tw-mg :background ,tw-mg-1))))
   `(term-color-cyan ((t (:foreground ,tw-cy :background ,tw-cy-1))))
   `(term-color-white ((t (:foreground ,tw-fg-c :background ,tw-bg+5))))
   `(term-default-fg-color ((t (:inherit term-color-white))))
   `(term-default-bg-color ((t (:inherit term-color-black))))))

(provide-theme 'twilight)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;; twilight-theme.el ends here
