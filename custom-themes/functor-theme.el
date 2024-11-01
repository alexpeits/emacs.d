;;; functor-theme.el --- A dark theme inspired by vim-lucius
;;;
;;; Author: Alex Peitsinis <alexpeitsinis@gmail.com>
;;; Url: https://github.com/alexpeits/emacs-functor-theme
;;; Version: 20200708.0
;;;
;;; Changelog :
;;;
;;; 20190222.0: Initial version
;;; 20200708.0: Switch to using a palette
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

(deftheme functor
  "A dark theme inspired by vim-lucius")

(defvar functor-theme-less-colors nil)

(let* ((bg "#060606")
       (fg "#dadada")

       (grey-8 "#151515")
       (grey-7 "#242424")
       (grey-6 "#272727")
       (grey-5 "#333333")
       (grey-4 "#3a3a3a")
       (grey-3 "#444444")
       (grey-2 "#4e4e4e")
       (grey-1 "#626262")
       (grey   "#808080")
       (grey+1 "#909090")
       (grey+2 "#9e9e9e")
       (grey+3 "#b2b2b2")
       (grey+4 "#bcbcbc")
       (grey+5 "#d0d0d0")

       (olive   "#879c05")
       (olive+1 "#87af00")

       (green-2 "#183918")
       (green-1 "#5f875f")
       (green   "#4c934c")
       (green+1 "#5faf5f")
       (green+2 "#87d7af")

       (green-bright-1 "#006800")

       (green-alt-pale "#afd7af")
       (green-alt      "#afd787")

       (cyan-pale-3 "#2c3232")
       (cyan-pale-2 "#548a81")
       (cyan-pale-1 "#86b8b5")
       (cyan-pale   "#99d1ce")
       (cyan-pale+1 "#bfebe0")

       (cyan-7 "#003535")
       (cyan-6 "#003a3a")
       (cyan-5 "#004c40")
       (cyan-4 "#005757")
       (cyan-3 "#006553")
       (cyan-2 "#2da49b")
       (cyan-1 "#1da89f")
       (cyan   "#1ebab0")
       (cyan+1 "#5fd7d7")
       (cyan+2 "#87d7d7")
       (cyan+3 "#afd7d7")
       (cyan+4 "#c1eae2")

       (blue-pale-1 "#1a2d4c")
       (blue-pale   "#87afd7")

       (blue-4 "#0d1c20")
       (blue-3 "#003347")
       (blue-2 "#004065")
       (blue-1 "#005881")
       (blue   "#5fafd7")
       (blue+1 "#87d7ff")
       (blue+2 "#8ae4f2")

       (blue-bright-2 "#298ed5")
       (blue-bright-1 "#0798db")
       (blue-bright   "#00aee6")

       (magenta-pale-1 "#5f5f87")
       (magenta-pale   "#819fc7")

       (magenta   "#d7afd7")
       (magenta+1 "#e6c5df")

       (orange "#d7875f")

       (yellow-3 "#4a3a10")
       (yellow-2 "#524a37")
       (yellow-1 "#585800")
       (yellow   "#d7af5f")

       (beige-1 "#b5b594")
       (beige   "#cacaa7")
       (beige+1 "#d7d7af")

       (red-pale-1 "#6e3a50")
       (red-pale   "#875f5f")

       (red-3 "#4c1515")
       (red-2 "#870000")
       (red-1 "#bc4d4d")
       (red   "#ff5f5f")
       (red+1 "#ff8787")
       (red+2 "#ffb9ab")

       (hi-green-bg  "#3c5444") (hi-green-fg  "#9ad6ac")
       (hi-red-bg    "#543b41") (hi-red-fg    "#eb94a1")
       (hi-blue-bg   "#3d5559") (hi-blue-fg   "#90d7e8")
       (hi-yellow-bg "#574833") (hi-yellow-fg "#c9c079")

       ;; the following are copied from modus-vivendi-theme
       (diff-added-bg-1 "#002600") (diff-added-fg-1 "#94ba94")
       (diff-added-bg   "#244024") (diff-added-fg   "#b4ddb4")
       (diff-added-bg+1 "#005a00") (diff-added-fg+1 "#e0ffe0")

       (diff-changed-bg-1 "#2a2000") (diff-changed-fg-1 "#b0ba9f")
       (diff-changed-bg   "#4a3a10") (diff-changed-fg   "#d0daaf")
       (diff-changed-bg+1 "#585800") (diff-changed-fg+1 "#ffffcc")

       (diff-removed-bg-1 "#390a0a") (diff-removed-fg-1 "#bbadaa")
       (diff-removed-bg   "#542222") (diff-removed-fg   "#eebdba")
       (diff-removed-bg+1 "#7d0000") (diff-removed-fg+1 "#ffc8bb")

       (builtin-fg     (if functor-theme-less-colors cyan-pale-1 blue+1))
       (keyword-fg     (if functor-theme-less-colors cyan-pale-1 cyan-1))
       (keyword-weight (if functor-theme-less-colors 'bold 'normal))
       (string-fg      (if functor-theme-less-colors cyan-1 beige+1))
       (doc-fg         (if functor-theme-less-colors cyan-2 beige))
       (type-fg        (if functor-theme-less-colors cyan-pale-1 cyan+2))
       )
  (custom-theme-set-faces
   'functor

   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor  ((t (:background ,cyan-pale))))
   `(region  ((t (:background ,blue-1))))

   `(fringe              ((t (:foreground ,grey+2 :background ,grey-8))))
   `(vertical-border     ((t (:foreground ,grey-1))))
   `(link                ((t (:foreground ,blue-bright :underline t))))
   `(italic              ((t (:foreground ,cyan-pale+1 :slant italic))))
   `(hl-line             ((t (:background ,grey-6))))
   `(show-paren-match    ((t (:background ,red-pale-1 :foreground ,fg))))
   `(match               ((t (:background ,blue-1 :foreground ,fg))))
   `(whitespace-trailing ((t (:background ,red-2))))

   `(shadow              ((t (:foreground ,grey+2))))
   `(highlight           ((t (:background ,grey-4))))
   `(secondary-selection ((t (:inherit highlight))))
   `(escape-glyph        ((t (:foreground ,orange))))
   `(warning             ((t (:foreground ,yellow :weight bold))))

   ;; emacs >= 27
   `(fill-column-indicator ((t (:foreground ,grey-3))))

   ;; Line Numbers
   ;; emacs >= 26
   `(line-number              ((t (:foreground ,grey-1 :background ,grey-6))))
   `(line-number-current-line ((t (:inherit line-number :background ,grey-4))))
   ;; emacs < 26
   `(linum ((t (:foreground ,grey-1 :background ,grey-6))))

   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line           ((t (:foreground ,cyan+4 :background ,cyan-4
                                          :box (:line-width 1 :color ,cyan-6)))))
   `(mode-line-inactive  ((t (:foreground ,cyan-pale-2 :background ,cyan-pale-3
                                          :box (:line-width 1 :color ,cyan-6)))))

   `(powerline-active0   ((t (:background ,cyan-3 :foreground ,cyan+4))))
   `(powerline-active1   ((t (:background ,cyan-5 :foreground ,cyan+4))))
   `(powerline-active2   ((t (:background ,cyan-6 :foreground ,cyan+4))))
   `(powerline-inactive0 ((t (:background ,cyan-pale-3 :foreground ,cyan-pale-2))))
   `(powerline-inactive1 ((t (:inherit powerline-inactive0))))
   `(powerline-inactive2 ((t (:inherit powerline-inactive0))))

   `(minibuffer-prompt ((t (:foreground ,cyan))))

   `(persp-selected-face ((t (:foreground ,green-alt :weight bold))))

   `(which-func ((t (:foreground ,beige))))

   `(isearch-fail   ((t (:foreground ,red+2 :background ,red-2))))
   `(isearch        ((t (:foreground ,fg :background ,green-bright-1 :weight bold))))
   `(lazy-highlight ((t (:foreground ,blue+2 :background ,blue-2))))

   `(evil-ex-substitute-matches     ((t (:inherit hi-blue))))
   `(evil-ex-substitute-replacement ((t (:inherit hi-yellow))))

   `(font-lock-negation-char-face        ((t (:foreground ,orange))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,red))))
   `(font-lock-warning-face              ((t (:foreground ,yellow))))

   `(font-lock-preprocessor-face      ((t (:foreground ,olive))))
   `(font-lock-comment-face           ((t (:foreground ,grey+1))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-doc-face               ((t (:foreground ,doc-fg))))
   `(font-lock-string-face            ((t (:foreground ,string-fg))))
   `(font-lock-constant-face          ((t (:foreground ,green+2))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin-fg))))
   `(font-lock-variable-name-face     ((t (:inherit font-lock-builtin-face))))
   `(font-lock-keyword-face           ((t (:foreground ,keyword-fg :weight ,keyword-weight))))
   `(font-lock-function-name-face     ((t (:foreground ,green-alt))))
   `(font-lock-type-face              ((t (:foreground ,type-fg))))

   `(haskell-pragma-face ((t (:foreground ,magenta))))

   `(purescript-constructor-face ((t (:inherit haskell-constructor-face))))

   `(tuareg-font-lock-governing-face ((t (:foreground ,magenta))))
   `(tuareg-font-lock-operator-face  ((t (:inherit font-lock-variable-name-face))))

   `(coq-solve-tactics-face  ((t (:foreground ,red+1))))
   `(coq-cheat-face          ((t (:inherit hi-red-b))))
   `(coq-button-face         ((t (:inherit hi-green-b))))
   `(coq-button-face-pressed ((t (:inherit hi-green-b :background ,grey-1))))

   `(proof-tacticals-name-face   ((t (:foreground ,magenta))))
   `(proof-tactics-name-face     ((t (:foreground ,blue-bright))))
   `(proof-error-face            ((t (:inherit hi-red-b))))
   `(proof-locked-face           ((t (:background ,cyan-6))))
   `(proof-queue-face            ((t (:background ,red-3))))
   `(proof-warning-face          ((t (:background ,yellow-2))))
   `(proof-declaration-name-face ((t (:inherit font-lock-function-name-face :weight bold))))

   `(nix-attribute-face ((t (:inherit font-lock-function-name-face))))

   `(enh-ruby-heredoc-delimiter-face ((t (:inherit font-lock-string-face))))
   `(enh-ruby-op-face ((t (:foreground ,fg))))
   `(enh-ruby-regexp-delimiter-face ((t (:foreground ,green+1))))
   `(enh-ruby-regexp-face ((t (:foreground ,magenta))))
   `(enh-ruby-string-delimiter-face ((t (:inherit font-lock-string-face))))

   `(web-mode-html-tag-face ((t (:foreground ,cyan-pale))))
   `(web-mode-block-delimiter-face ((t (:foreground ,magenta))))

   `(sh-quoted-exec ((t (:inherit font-lock-preprocessor-face))))
   `(sh-heredoc     ((t (:foreground ,magenta))))

   `(diff-hl-insert ((t (:background ,green-2 :foreground ,green))))
   `(diff-hl-change ((t (:background ,blue-pale-1 :foreground ,blue-bright-2))))
   `(diff-hl-delete ((t (:background ,red-3 :foreground ,red-1))))

   `(git-gutter+-added    ((t (:foreground ,green :weight bold))))
   `(git-gutter+-modified ((t (:foreground ,blue-bright-2 :weight bold))))
   `(git-gutter+-deleted  ((t (:foreground ,red-1 :weight bold))))

   ;; diff- and magit-diff- stuff copied from modus-vivendi theme
   `(diff-added             ((t (:foreground ,diff-added-fg :background ,diff-added-bg))))
   `(diff-changed           ((t (:foreground ,diff-changed-fg :background ,diff-changed-bg))))
   `(diff-removed           ((t (:foreground ,diff-removed-fg :background ,diff-removed-bg))))
   `(diff-refine-added      ((t (:foreground ,diff-added-fg+1 :background ,diff-added-bg+1))))
   `(diff-refine-changed    ((t (:foreground ,diff-changed-fg+1 :background ,diff-changed-bg+1))))
   `(diff-refine-removed    ((t (:foreground ,diff-removed-fg+1 :background ,diff-removed-bg+1))))
   `(diff-indicator-added   ((t (:inherit diff-added))))
   `(diff-indicator-changed ((t (:inherit diff-changed))))
   `(diff-indicator-removed ((t (:inherit diff-removed))))
   `(diff-header            ((t (:foreground ,magenta-pale))))
   `(diff-hunk-header       ((t (:inherit region :weight bold))))
   `(diff-file-header       ((t (:foreground ,blue-bright :weight bold))))
   `(diff-function          ((t (:foreground ,cyan-pale ))))
   `(diff-context           ((t (:inherit shadow))))

   `(magit-diff-added                  ((t (:foreground ,diff-added-fg-1
                                                        :background ,diff-added-bg-1))))
   `(magit-diff-base                   ((t (:foreground ,diff-changed-fg-1
                                                        :background ,diff-changed-bg-1))))
   `(magit-diff-removed                ((t (:foreground ,diff-removed-fg-1
                                                        :background ,diff-removed-bg-1))))
   `(magit-diff-added-highlight        ((t (:inherit diff-added))))
   `(magit-diff-base-highlight         ((t (:inherit diff-changed))))
   `(magit-diff-removed-highlight      ((t (:inherit diff-removed))))
   `(magit-diff-hunk-heading           ((t (:background ,grey-5 :foreground ,grey+4 :weight bold))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,blue-2 :foreground ,cyan+4 :weight bold))))
   `(magit-diff-context-highlight      ((t (:background ,grey-7 region :foreground ,grey+3))))
   `(magit-section-heading             ((t (:foreground ,beige :weight bold))))
   `(magit-section-highlight           ((t (:inherit hl-line))))
   `(magit-branch-local                ((t (:foreground ,blue-bright))))
   `(magit-branch-remote               ((t (:foreground ,magenta))))
   `(magit-tag                         ((t (:foreground ,olive+1))))

   `(change-log-date           ((t (:foreground ,magenta))))
   `(change-log-name           ((t (:foreground ,cyan))))
   `(change-log-email          ((t (:inherit change-log-name))))
   `(change-log-acknowledgment ((t (:foreground ,beige))))

   `(hi-green   ((t (:background ,grey-5 :foreground ,hi-green-fg :underline t))))
   `(hi-blue    ((t (:background ,grey-5 :foreground ,hi-blue-fg :underline t))))
   `(hi-pink    ((t (:background ,grey-5 :foreground ,hi-red-fg :underline t))))
   `(hi-yellow  ((t (:background ,grey-5 :foreground ,hi-yellow-fg :underline t))))
   `(hi-green-b ((t (:background ,hi-green-bg :foreground ,hi-green-fg :weight bold))))
   `(hi-blue-b  ((t (:background ,hi-blue-bg :foreground ,hi-blue-fg :weight bold))))
   `(hi-red-b   ((t (:background ,hi-red-bg :foreground ,hi-red-fg :weight bold))))
   `(hi-black-b ((t (:background ,hi-yellow-bg :foreground ,hi-yellow-fg :weight bold))))

   `(ivy-current-match           ((t (:inherit match :weight bold))))
   `(ivy-minibuffer-match-face-1 ((t (:inherit highlight :weight bold))))
   `(ivy-minibuffer-match-face-2 ((t (:inherit hi-green-b :weight bold))))
   `(ivy-minibuffer-match-face-3 ((t (:inherit hi-black-b :weight bold))))
   `(ivy-minibuffer-match-face-4 ((t (:inherit hi-red-b :weight bold))))
   `(ivy-grep-info               ((t (:foreground ,cyan+1))))
   `(ivy-grep-line-number        ((t (:foreground ,magenta))))
   `(ivy-highlight-face          ((t (:foreground ,magenta))))

   `(swiper-match-face-1 ((t (:background ,blue-3 :foreground ,blue+2))))
   `(swiper-match-face-2 ((t (:inherit hi-gren-b :weight normal))))
   `(swiper-match-face-3 ((t (:inherit hi-red-b :weight normal))))
   `(swiper-match-face-4 ((t (:inherit hi-black-b :weight normal))))

   `(helm-source-header    ((t (:inherit hi-blue-b :height 1.3))))
   `(helm-selection        ((t (:inherit ivy-current-match :underline nil))))
   `(helm-candidate-number ((t (:inherit hi-blue-b))))

   `(ag-hit-face ((t (:foreground ,cyan+1 :weight bold))))

   `(aw-leading-char-face ((t (:foreground ,magenta :weight bold :height 2.5))))

   `(dired-directory ((t (:foreground ,blue :weight bold))))
   `(dired-symlink   ((t (:foreground ,cyan :underline t))))
   `(dired-marked    ((t (:inherit hi-black-b))))
   `(dired-flagged   ((t (:inherit hi-red-b))))
   `(dired-header    ((t (:foreground ,blue+1 :weight bold))))

   `(diredfl-dir-heading ((t (:inherit dired-header))))
   `(diredfl-rare-priv   ((t (:foreground ,magenta))))
   `(diredfl-dir-priv    ((t (:foreground ,blue))))
   `(diredfl-no-priv     ((t (:foreground ,grey+3))))
   `(diredfl-read-priv   ((t (:foreground ,red+1))))
   `(diredfl-write-priv  ((t (:foreground ,yellow))))
   `(diredfl-exec-priv   ((t (:foreground ,green+1))))
   `(diredfl-number      ((t (:foreground ,cyan-2))))
   `(diredfl-date-time   ((t (:foreground ,cyan-pale))))
   `(diredfl-file-name   ((t (:foreground ,fg))))
   `(diredfl-file-suffix ((t (:foreground ,beige-1))))
   `(diredfl-dir-name    ((t (:inherit dired-directory))))
   `(diredfl-symlink     ((t (:inherit dired-symlink))))

   `(eshell-ls-directory  ((t (:inherit dired-directory))))
   `(eshell-ls-symlink    ((t (:inherit dired-symlink))))
   `(eshell-ls-executable ((t (:foreground ,olive :weight bold))))

   `(header-line ((t (:foreground ,grey+3 :background ,grey-4))))

   `(Info-quoted      ((t (:foreground ,magenta))))
   `(info-header-node ((t (:foreground ,grey+2 :weight bold))))
   `(info-title-1     ((t (:foreground ,fg :weight bold :height 1.2))))
   `(info-title-2     ((t (:foreground ,cyan-pale+1 :weight bold))))
   `(info-title-3     ((t (:foreground ,magenta+1 :weight bold))))
   `(info-title-4     ((t (:foreground ,beige+1 :weight bold))))

   `(eww-valid-certificate   ((t (:foreground ,green-alt :weight bold))))
   `(eww-invalid-certificate ((t (:foreground ,red+1 :weight bold))))

   `(compilation-info           ((t (:foreground ,green-alt))))
   `(compilation-warning        ((t (:foreground ,yellow))))
   `(compilation-error          ((t (:foreground ,red+1))))
   `(compilation-mode-line-exit ((t (:inherit compilation-info :weight bold))))

   `(flycheck-info           ((t (:underline (:color ,blue :style wave)))))
   `(flycheck-fringe-info    ((t (:foreground ,blue))))
   `(flycheck-warning        ((t (:underline (:color ,yellow :style wave)))))
   `(flycheck-fringe-warning ((t (:foreground ,yellow))))
   `(flycheck-error          ((t (:underline (:color ,red :style wave)))))
   `(flycheck-fringe-error   ((t (:foreground ,red))))

   `(flyspell-incorrect ((t (:underline (:color ,red :style wave)))))
   `(flyspell-duplicate ((t (:underline (:color ,orange :style wave)))))

   `(anzu-mode-line          ((t (:foreground ,magenta :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,red :weight bold))))

   `(rainbow-delimiters-depth-1-face    ((t (:foreground ,beige+1))))
   `(rainbow-delimiters-depth-2-face    ((t (:foreground ,cyan+2))))
   `(rainbow-delimiters-depth-3-face    ((t (:foreground ,magenta))))
   `(rainbow-delimiters-depth-4-face    ((t (:foreground ,green+2))))
   `(rainbow-delimiters-depth-5-face    ((t (:inherit rainbow-delimiters-depth-1-face))))
   `(rainbow-delimiters-depth-6-face    ((t (:inherit rainbow-delimiters-depth-2-face))))
   `(rainbow-delimiters-depth-7-face    ((t (:inherit rainbow-delimiters-depth-3-face))))
   `(rainbow-delimiters-depth-8-face    ((t (:inherit rainbow-delimiters-depth-4-face))))
   `(rainbow-delimiters-depth-9-face    ((t (:inherit rainbow-delimiters-depth-1-face))))
   `(rainbow-delimiters-unmatched-face  ((t (:inherit font-lock-warning-face))))
   `(rainbow-delimiters-mismatched-face ((t (:inherit font-lock-warning-face))))

   `(org-block            ((t (:background ,blue-4 :extend t))))
   `(org-block-begin-line ((t (:foreground ,blue-bright-1 :background ,blue-3 :extend t))))
   `(org-block-end-line   ((t (:inherit org-block-begin-line))))
   `(org-hide             ((t (:foreground ,bg))))
   `(org-indent           ((t (:foreground ,bg :background ,bg))))
   `(org-ellipsis         ((t (:foreground ,magenta :underline t))))
   `(org-date             ((t (:foreground ,green+2 :underline t))))
   `(org-verbatim         ((t (:foreground ,magenta+1))))
   `(org-list-dt          ((t (:foreground ,cyan-pale+1 :weight bold))))
   `(org-level-1          ((t (:inherit outline-1 :height 1.2))))

   `(org-roam-link         ((t (:foreground ,cyan+3 :background ,blue-2 :underline t))))
   `(org-roam-link-invalid ((t (:inherit font-lock-warning-face :underline t))))

   `(org-ref-cite-face ((t (:inherit link :foreground ,green+1))))

   `(outline-1 ((t (:foreground ,blue-pale))))
   `(outline-2 ((t (:foreground ,cyan+2))))
   `(outline-3 ((t (:foreground ,beige+1))))
   `(outline-4 ((t (:foreground ,cyan-1))))
   `(outline-5 ((t (:foreground ,magenta))))
   `(outline-6 ((t (:foreground ,olive+1))))
   `(outline-7 ((t (:inherit outline-1))))
   `(outline-8 ((t (:inherit outline-2))))

   `(markdown-code-face             ((t (:foreground ,cyan-pale))))
   `(markdown-inline-code-face      ((t (:foreground ,magenta))))
   `(markdown-pre-face              ((t (:inherit markdown-code-face))))
   `(markdown-url-face              ((t (:inherit link))))
   `(markdown-header-face           ((t (:weight bold))))
   `(markdown-header-delimiter-face ((t (:foreground ,cyan))))
   `(markdown-blockquote-face       ((t (:foreground ,green-alt-pale :weight normal))))
   `(markdown-language-keyword-face ((t (:foreground ,green+1))))

   `(rst-literal    ((t (:foreground ,cyan-2))))
   `(rst-directive  ((t (:foreground ,magenta))))
   `(rst-transition ((t (:foreground ,cyan+2))))
   `(rst-adornment  ((t (:foreground ,olive :weight bold))))
   `(rst-level-1    ((t (:inherit rst-adornment))))
   `(rst-level-2    ((t (:inherit rst-level-1))))
   `(rst-level-3    ((t (:inherit rst-level-1))))
   `(rst-level-4    ((t (:inherit rst-level-1))))
   `(rst-level-5    ((t (:inherit rst-level-1))))
   `(rst-level-6    ((t (:inherit rst-level-1))))

   `(markup-gen-face                 ((t (:inherit font-lock-string-face))))
   `(markup-strong-face              ((t (:inherit markup-gen-face :weight bold))))
   `(markup-italic-face              ((t (:inherit markup-gen-face :slant italic))))
   `(markup-typewriter-face          ((t (:inherit default))))
   `(markup-verbatim-face            ((t (:inherit font-lock-doc-face))))
   `(markup-list-face                ((t (:inherit font-lock-function-name-face))))
   `(markup-reference-face           ((t (:inherit link))))
   `(markup-table-face               ((t (:inherit font-lock-function-name-face))))
   `(markup-meta-face                ((t (:inherit markup-gen-face))))
   `(markup-meta-hide-face           ((t (:inherit markup-meta-face))))
   `(markup-secondary-text-face      ((t (:inherit shadow))))
   `(markup-replacement-face         ((t (:inherit font-lock-preprocessor-face))))
   `(markup-complex-replacement-face ((t (:inherit font-lock-warning-face))))
   `(markup-title-0-face             ((t (:inherit font-lock-preprocessor-face :weight bold))))
   `(markup-title-1-face             ((t (:inherit markup-title-0-face))))
   `(markup-title-2-face             ((t (:inherit markup-title-0-face))))
   `(markup-title-3-face             ((t (:inherit markup-title-0-face))))
   `(markup-title-4-face             ((t (:inherit markup-title-0-face))))
   `(markup-title-5-face             ((t (:inherit markup-title-0-face))))

   `(imenu-list-entry-face-0          ((t (:foreground ,green-alt))))
   `(imenu-list-entry-subalist-face-0 ((t (:inherit imenu-list-entry-face-0 :weight bold))))
   `(imenu-list-entry-face-1          ((t (:foreground ,cyan-pale))))
   `(imenu-list-entry-subalist-face-1 ((t (:inherit imenu-list-entry-face-1 :weight bold))))
   `(imenu-list-entry-face-2          ((t (:foreground ,cyan-pale+1))))
   `(imenu-list-entry-subalist-face-2 ((t (:inherit imenu-list-entry-face-2 :weight bold))))
   `(imenu-list-entry-face-3          ((t (:foreground ,cyan-pale+1))))
   `(imenu-list-entry-subalist-face-3 ((t (:inherit imenu-list-entry-face-3 :weight bold))))

   `(elfeed-search-title-face        ((t (:inherit default))))
   `(elfeed-search-unread-title-face ((t (:inherit default :weight bold))))
   `(elfeed-search-feed-face         ((t (:foreground ,yellow))))
   `(elfeed-search-tag-face          ((t (:foreground ,olive))))

   `(erc-timestamp-face ((t (:foreground ,beige :weight bold))))
   `(erc-prompt-face    ((t (:foreground ,cyan-1 :weight bold))))
   `(erc-action-face    ((t (:foreground ,beige+1))))

   `(message-header-name    ((t (:foreground ,olive))))
   `(message-header-subject ((t (:foreground ,green-alt :weight bold))))
   `(message-header-to      ((t (:foreground ,cyan-1))))
   `(message-header-other   ((t (:foreground ,yellow))))

   `(shr-link ((t (:foreground ,cyan+1 :underline t))))

   `(highlight-indent-guides-character-face ((t (:foreground ,grey-1))))

   `(company-preview                      ((t (:inherit default :background ,grey-4))))
   `(company-preview-common               ((t (:inherit company-preview :slant italic))))
   `(company-tooltip                      ((t (:foreground ,grey+4 :background ,grey-6))))
   `(company-tooltip-annotation           ((t (:foreground ,cyan-2))))
   `(company-tooltip-annotation-selection ((t (:foreground ,cyan-2))))
   `(company-tooltip-common               ((t (:foreground ,grey+4))))
   `(company-tooltip-common-selection     ((t (:weight bold))))
   `(company-tooltip-mouse                ((t (:background ,cyan-2 :foreground ,cyan-6))))
   `(company-tooltip-search               ((t (:foreground ,magenta))))
   `(company-tooltip-search-selection     ((t (:foreground ,magenta :weight bold))))
   `(company-tooltip-selection            ((t (:background ,grey-4 :weight bold))))
   `(company-scrollbar-bg                 ((t (:background ,grey-6 :foreground ,cyan-2))))
   `(company-scrollbar-fg                 ((t (:foreground ,grey-7 :background ,cyan-2))))

   `(term-color-black   ((t (:foreground ,grey-3))))
   `(term-color-red     ((t (:foreground ,red-1))))
   `(term-color-green   ((t (:foreground ,olive))))
   `(term-color-yellow  ((t (:foreground ,yellow))))
   `(term-color-blue    ((t (:foreground ,blue))))
   `(term-color-magenta ((t (:foreground ,magenta))))
   `(term-color-cyan    ((t (:foreground ,cyan-2))))
   `(term-color-white   ((t (:foreground ,grey+4))))

   `(projectile-tab-bar-modeline-active-face ((t (:foreground ,green-alt :weight bold))))

   `(my/elfeed-blue        ((t (:foreground ,blue))))
   `(my/elfeed-cyan        ((t (:foreground ,cyan))))
   `(my/elfeed-cyan-alt    ((t (:foreground ,cyan-pale))))
   `(my/elfeed-green       ((t (:foreground ,green+1))))
   `(my/elfeed-yellow      ((t (:foreground ,yellow))))
   `(my/elfeed-magenta     ((t (:foreground ,orange))))
   `(my/elfeed-magenta-alt ((t (:foreground ,magenta-pale))))
   `(my/elfeed-red         ((t (:foreground ,red-1))))
   `(my/elfeed-pink        ((t (:foreground ,beige))))
   )

  (custom-theme-set-variables
   'functor
   `(ibuffer-deletion-face 'dired-flagged)
   `(ibuffer-filter-group-name-face 'dired-mark)
   `(ibuffer-marked-face 'dired-marked)
   `(ibuffer-title-face 'dired-header)
   `(coq-highlighted-hyps-bg ,grey-4)
   `(ansi-color-names-vector
     [grey-3 red-1 olive yellow blue magenta cyan-2 grey+4])
   )
  )


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'functor)

;;; functor-theme.el ends here
