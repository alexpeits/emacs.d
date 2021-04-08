;;; predawn-theme.el --- predawn
;;;
;;; Author: Alex Peitsinis <alexpeitsinis@gmail.com>
;;; Url: https://github.com/alexpeits/emacs-predawn-theme
;;; Version: 20210408.0
;;;
;;; Changelog :
;;;
;;; 20210408.0: Initial version
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

(deftheme predawn
  "predawn")

(let* (
       (predawn "#f18260")
       (orange  "#f49d62")

       (pale-yellow "#f5f5ae")
       (yellow      "#ede480")

       (red      "#cf5340")
       (dark-red "#893121")
       (maroon   "#55201b")

       (pale-blue "#bddcdc")
       (blue      "#92bfbf")

       (slate        "#5f777e")
       (dark-slate   "#4f676e")
       (darker-slate "#456064")

       (green      "#b4d388")
       (pale-green "#d0eda7")

       (dark-green   "#809161")
       (darker-green "#708151")

       (black  "#151515")
       (grey-9 "#232323")
       (grey-8 "#252525")
       (grey-7 "#282828")
       (grey-6 "#353535")
       (grey-5 "#4c4c4c")
       (grey-4 "#777777")
       (grey-3 "#999999")
       (grey-2 "#b0b0b0")
       (grey-1 "#dddddd")
       (grey-0 "#e8e8e8")

       (fg grey-0)
       (bg grey-7)

       (hi-green-bg  "#3c5444") (hi-green-fg  pale-green)
       (hi-red-bg    "#543b41") (hi-red-fg    "#eb94a1")
       (hi-blue-bg   "#3d5559") (hi-blue-fg   pale-blue)
       (hi-yellow-bg "#574833") (hi-yellow-fg yellow)

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
       )
  (custom-theme-set-faces
   'predawn

   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor  ((t (:background ,grey-1))))
   `(region  ((t (:background ,dark-slate))))

   `(fringe              ((t (:foreground ,grey-1 :background ,grey-6))))
   `(vertical-border     ((t (:foreground ,grey-4))))
   `(link                ((t (:foreground ,blue :underline t))))
   `(italic              ((t (:foreground ,pale-blue :slant italic))))
   `(hl-line             ((t (:background ,grey-6))))
   `(show-paren-match    ((t (:background ,darker-slate :foreground ,fg :weight bold))))
   `(match               ((t (:background ,dark-slate :foreground ,fg))))
   `(whitespace-trailing ((t (:background ,dark-red))))

   `(shadow              ((t (:foreground ,grey-3))))
   `(highlight           ((t (:background ,grey-5))))
   `(secondary-selection ((t (:inherit highlight))))
   `(escape-glyph        ((t (:foreground ,orange))))
   `(warning             ((t (:foreground ,yellow :weight bold))))

   ;; emacs >= 27
   `(fill-column-indicator ((t (:foreground ,grey-5))))

   ;; Line Numbers
   ;; emacs >= 26
   `(line-number              ((t (:foreground ,grey-2 :background ,grey-6))))
   `(line-number-current-line ((t (:inherit line-number :background ,grey-5))))
   ;; emacs < 26
   `(linum ((t (:foreground ,grey-1 :background ,grey-6))))

   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line           ((t (:foreground ,grey-1 :background ,grey-5
                                          :box (:line-width 1 :color ,grey-4)))))
   `(mode-line-inactive  ((t (:foreground ,grey-3 :background ,grey-6
                                          :box (:line-width 1 :color ,grey-5)))))

   `(powerline-active0   ((t (:background ,grey-4 :foreground ,grey-1))))
   `(powerline-active1   ((t (:background ,grey-5 :foreground ,grey-1))))
   `(powerline-active2   ((t (:background ,grey-6 :foreground ,grey-1))))
   `(powerline-inactive0 ((t (:background ,grey-6 :foreground ,grey-3))))
   `(powerline-inactive1 ((t (:inherit powerline-inactive0))))
   `(powerline-inactive2 ((t (:inherit powerline-inactive0))))

   `(minibuffer-prompt ((t (:foreground ,pale-blue))))

   `(persp-selected-face ((t (:foreground ,green :weight bold))))

   `(isearch-fail   ((t (:foreground ,red :background ,maroon))))
   `(isearch        ((t (:foreground ,fg :background ,darker-green :weight bold))))
   `(lazy-highlight ((t (:foreground ,pale-blue :background ,darker-slate))))

   `(evil-ex-substitute-matches     ((t (:inherit hi-blue))))
   `(evil-ex-substitute-replacement ((t (:inherit hi-yellow))))

   `(font-lock-negation-char-face        ((t (:foreground ,orange))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,red))))
   `(font-lock-warning-face              ((t (:foreground ,yellow))))

   `(font-lock-preprocessor-face      ((t (:foreground ,red))))
   `(font-lock-comment-face           ((t (:foreground ,grey-3))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-doc-face               ((t (:foreground ,pale-blue))))
   `(font-lock-string-face            ((t (:foreground ,pale-blue))))
   `(font-lock-constant-face          ((t (:foreground ,yellow))))
   `(font-lock-builtin-face           ((t (:foreground ,green))))
   `(font-lock-variable-name-face     ((t (:foreground ,green))))
   `(font-lock-keyword-face           ((t (:foreground ,yellow))))
   `(font-lock-function-name-face     ((t (:foreground ,orange))))
   `(font-lock-type-face              ((t (:foreground ,blue))))

   ;; `(haskell-pragma-face ((t (:foreground ,magenta))))
   ;; `(haskell-operator-face ((t (:foreground ,orange))))

   `(purescript-constructor-face ((t (:inherit haskell-constructor-face))))

   `(tuareg-font-lock-governing-face ((t (:foreground ,blue))))
   `(tuareg-font-lock-operator-face  ((t (:inherit font-lock-variable-name-face))))

   `(coq-solve-tactics-face  ((t (:foreground ,red))))
   `(coq-cheat-face          ((t (:inherit hi-red-b))))
   `(coq-button-face         ((t (:inherit hi-green-b))))
   `(coq-button-face-pressed ((t (:inherit hi-green-b :background ,grey-0))))

   `(proof-tacticals-name-face   ((t (:foreground ,blue))))
   `(proof-tactics-name-face     ((t (:foreground ,pale-blue))))
   `(proof-error-face            ((t (:inherit hi-red-b))))
   `(proof-locked-face           ((t (:background ,hi-blue-bg))))
   `(proof-queue-face            ((t (:background ,hi-red-bg))))
   `(proof-warning-face          ((t (:background ,hi-yellow-bg))))
   `(proof-declaration-name-face ((t (:inherit font-lock-function-name-face :weight bold))))

   `(nix-attribute-face ((t (:inherit font-lock-function-name-face))))

   `(enh-ruby-heredoc-delimiter-face ((t (:inherit font-lock-string-face))))
   `(enh-ruby-op-face ((t (:foreground ,fg))))
   `(enh-ruby-regexp-delimiter-face ((t (:foreground ,green))))
   `(enh-ruby-regexp-face ((t (:foreground ,predawn))))
   `(enh-ruby-string-delimiter-face ((t (:inherit font-lock-string-face))))

   `(web-mode-html-tag-face ((t (:foreground ,blue))))
   `(web-mode-block-delimiter-face ((t (:foreground ,orange))))

   `(sh-quoted-exec ((t (:inherit font-lock-preprocessor-face))))
   `(sh-heredoc     ((t (:foreground ,green))))

   `(diff-hl-insert ((t (:background ,diff-added-bg :foreground ,diff-added-fg))))
   `(diff-hl-change ((t (:background ,diff-changed-bg :foreground ,diff-changed-fg))))
   `(diff-hl-delete ((t (:background ,diff-removed-bg :foreground ,diff-removed-fg))))

   `(git-gutter+-added    ((t (:foreground ,diff-added-fg :weight bold))))
   `(git-gutter+-modified ((t (:foreground ,diff-changed-fg :weight bold))))
   `(git-gutter+-deleted  ((t (:foreground ,diff-removed-fg :weight bold))))

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
   `(diff-header            ((t (:foreground ,pale-green))))
   `(diff-hunk-header       ((t (:inherit region :weight bold))))
   `(diff-file-header       ((t (:foreground ,blue :weight bold))))
   `(diff-function          ((t (:foreground ,green ))))
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
   `(magit-diff-hunk-heading           ((t (:background ,grey-5 :foreground ,grey-1 :weight bold))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,slate :foreground ,grey-1 :weight bold))))
   `(magit-diff-context-highlight      ((t (:background ,grey-7 :foreground ,grey-3))))
   `(magit-section-heading             ((t (:foreground ,pale-green :weight bold))))
   `(magit-section-highlight           ((t (:inherit hl-line))))
   `(magit-branch-local                ((t (:foreground ,blue))))
   `(magit-branch-remote               ((t (:foreground ,orange))))
   `(magit-tag                         ((t (:foreground ,yellow))))

   `(change-log-date           ((t (:foreground ,green))))
   `(change-log-name           ((t (:foreground ,blue))))
   `(change-log-email          ((t (:inherit change-log-name))))
   `(change-log-acknowledgment ((t (:foreground ,grey-1))))

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
   `(ivy-grep-info               ((t (:foreground ,blue))))
   `(ivy-grep-line-number        ((t (:foreground ,green))))
   `(ivy-highlight-face          ((t (:foreground ,yellow))))

   `(swiper-match-face-1 ((t (:background ,slate :foreground ,blue))))
   `(swiper-match-face-2 ((t (:inherit hi-gren-b :weight normal))))
   `(swiper-match-face-3 ((t (:inherit hi-red-b :weight normal))))
   `(swiper-match-face-4 ((t (:inherit hi-black-b :weight normal))))

   `(helm-source-header    ((t (:inherit hi-blue-b :height 1.3))))
   `(helm-selection        ((t (:inherit ivy-current-match :underline nil))))
   `(helm-candidate-number ((t (:inherit hi-blue-b))))

   `(ag-hit-face ((t (:foreground ,blue :weight bold))))

   `(aw-leading-char-face ((t (:foreground ,predawn :weight bold :height 2.5))))

   `(dired-directory ((t (:foreground ,blue))))
   `(dired-symlink   ((t (:foreground ,yellow :underline t))))
   `(dired-marked    ((t (:inherit hi-black-b))))
   `(dired-flagged   ((t (:inherit hi-red-b))))
   `(dired-header    ((t (:foreground ,green :weight bold))))

   `(diredfl-dir-heading ((t (:inherit dired-header))))
   `(diredfl-rare-priv   ((t (:foreground ,yellow))))
   `(diredfl-dir-priv    ((t (:foreground ,blue))))
   `(diredfl-no-priv     ((t (:foreground ,grey-1))))
   `(diredfl-read-priv   ((t (:foreground ,orange))))
   `(diredfl-write-priv  ((t (:foreground ,yellow))))
   `(diredfl-exec-priv   ((t (:foreground ,green))))
   `(diredfl-number      ((t (:foreground ,blue))))
   `(diredfl-date-time   ((t (:foreground ,pale-green))))
   `(diredfl-file-name   ((t (:foreground ,fg))))
   `(diredfl-file-suffix ((t (:foreground ,pale-blue))))
   `(diredfl-dir-name    ((t (:inherit dired-directory))))
   `(diredfl-symlink     ((t (:inherit dired-symlink))))

   `(eshell-ls-directory  ((t (:inherit dired-directory))))
   `(eshell-ls-symlink    ((t (:inherit dired-symlink))))
   `(eshell-ls-executable ((t (:foreground ,green :weight bold))))

   `(header-line ((t (:foreground ,grey-1 :background ,grey-5))))

   `(Info-quoted      ((t (:foreground ,pale-blue))))
   `(info-header-node ((t (:foreground ,grey-1 :weight bold))))
   `(info-title-1     ((t (:foreground ,fg :weight bold :height 1.2))))
   `(info-title-2     ((t (:foreground ,pale-blue :weight bold))))
   `(info-title-3     ((t (:foreground ,green :weight bold))))
   `(info-title-4     ((t (:foreground ,grey-1 :weight bold))))

   `(eww-valid-certificate   ((t (:foreground ,green :weight bold))))
   `(eww-invalid-certificate ((t (:foreground ,red :weight bold))))

   `(compilation-info           ((t (:foreground ,pale-green))))
   `(compilation-warning        ((t (:foreground ,yellow))))
   `(compilation-error          ((t (:foreground ,red))))
   `(compilation-mode-line-exit ((t (:inherit compilation-info :weight bold))))

   `(flycheck-info           ((t (:underline (:color ,blue :style wave)))))
   `(flycheck-fringe-info    ((t (:foreground ,blue))))
   `(flycheck-warning        ((t (:underline (:color ,yellow :style wave)))))
   `(flycheck-fringe-warning ((t (:foreground ,yellow))))
   `(flycheck-error          ((t (:underline (:color ,red :style wave)))))
   `(flycheck-fringe-error   ((t (:foreground ,red))))

   `(flyspell-incorrect ((t (:underline (:color ,red :style wave)))))
   `(flyspell-duplicate ((t (:underline (:color ,orange :style wave)))))

   `(anzu-mode-line          ((t (:foreground ,blue :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,red :weight bold))))

   `(rainbow-delimiters-depth-1-face    ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-2-face    ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-3-face    ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-4-face    ((t (:foreground ,yellow))))
   `(rainbow-delimiters-depth-5-face    ((t (:inherit rainbow-delimiters-depth-1-face))))
   `(rainbow-delimiters-depth-6-face    ((t (:inherit rainbow-delimiters-depth-2-face))))
   `(rainbow-delimiters-depth-7-face    ((t (:inherit rainbow-delimiters-depth-3-face))))
   `(rainbow-delimiters-depth-8-face    ((t (:inherit rainbow-delimiters-depth-4-face))))
   `(rainbow-delimiters-depth-9-face    ((t (:inherit rainbow-delimiters-depth-1-face))))
   `(rainbow-delimiters-unmatched-face  ((t (:inherit font-lock-warning-face))))
   `(rainbow-delimiters-mismatched-face ((t (:inherit font-lock-warning-face))))

   `(org-block            ((t (:background ,grey-6 :extend t))))
   `(org-block-begin-line ((t (:foreground ,grey-3 :background ,grey-5 :extend t))))
   `(org-block-end-line   ((t (:inherit org-block-begin-line))))
   `(org-hide             ((t (:foreground ,bg))))
   `(org-indent           ((t (:foreground ,bg :background ,bg))))
   `(org-ellipsis         ((t (:foreground ,pale-blue :underline t))))
   `(org-date             ((t (:foreground ,green :underline t))))
   `(org-verbatim         ((t (:foreground ,blue))))
   `(org-list-dt          ((t (:foreground ,pale-blue :weight bold))))
   `(org-level-1          ((t (:inherit outline-1 :height 1.2))))

   `(org-roam-link         ((t (:foreground ,pale-blue :background ,slate :underline t))))
   `(org-roam-link-invalid ((t (:inherit font-lock-warning-face :underline t))))

   `(org-ref-cite-face ((t (:inherit link :foreground ,green))))

   `(outline-1 ((t (:foreground ,blue))))
   `(outline-2 ((t (:foreground ,orange))))
   `(outline-3 ((t (:foreground ,green))))
   `(outline-4 ((t (:foreground ,yellow))))
   `(outline-5 ((t (:foreground ,pale-blue))))
   `(outline-6 ((t (:foreground ,pale-yellow))))
   `(outline-7 ((t (:inherit outline-1))))
   `(outline-8 ((t (:inherit outline-2))))

   `(markdown-code-face             ((t (:foreground ,pale-blue))))
   `(markdown-inline-code-face      ((t (:foreground ,pale-yellow :background ,grey-6))))
   `(markdown-pre-face              ((t (:inherit markdown-code-face :background ,grey-6 :extend t))))
   `(markdown-url-face              ((t (:inherit link))))
   `(markdown-header-face           ((t (:foreground ,orange :weight bold))))
   `(markdown-header-delimiter-face ((t (:inherit shadow :weight bold))))
   `(markdown-blockquote-face       ((t (:foreground ,grey-1 :weight normal))))
   `(markdown-language-keyword-face ((t (:foreground ,green))))

   `(rst-literal    ((t (:foreground ,blue))))
   `(rst-directive  ((t (:foreground ,green))))
   `(rst-transition ((t (:foreground ,blue))))
   `(rst-adornment  ((t (:foreground ,green :weight bold))))
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

   `(imenu-list-entry-face-0          ((t (:foreground ,blue))))
   `(imenu-list-entry-subalist-face-0 ((t (:inherit imenu-list-entry-face-0 :weight bold))))
   `(imenu-list-entry-face-1          ((t (:foreground ,yellow))))
   `(imenu-list-entry-subalist-face-1 ((t (:inherit imenu-list-entry-face-1 :weight bold))))
   `(imenu-list-entry-face-2          ((t (:foreground ,green))))
   `(imenu-list-entry-subalist-face-2 ((t (:inherit imenu-list-entry-face-2 :weight bold))))
   `(imenu-list-entry-face-3          ((t (:foreground ,pale-blue))))
   `(imenu-list-entry-subalist-face-3 ((t (:inherit imenu-list-entry-face-3 :weight bold))))

   `(elfeed-search-title-face        ((t (:inherit default))))
   `(elfeed-search-unread-title-face ((t (:inherit default :weight bold))))
   `(elfeed-search-feed-face         ((t (:foreground ,pale-yellow))))
   `(elfeed-search-tag-face          ((t (:foreground ,blue))))

   `(erc-timestamp-face ((t (:foreground ,grey-1 :weight bold))))
   `(erc-prompt-face    ((t (:foreground ,blue :weight bold))))
   `(erc-action-face    ((t (:foreground ,grey-1))))

   `(message-header-name    ((t (:foreground ,green))))
   `(message-header-subject ((t (:foreground ,pale-green :weight bold))))
   `(message-header-to      ((t (:foreground ,blue))))
   `(message-header-other   ((t (:foreground ,yellow))))

   `(shr-link ((t (:inherit link))))

   `(highlight-indent-guides-character-face ((t (:foreground ,grey-0))))

   `(company-preview                      ((t (:inherit default :background ,grey-4))))
   `(company-preview-common               ((t (:inherit company-preview :slant italic))))
   `(company-tooltip                      ((t (:foreground ,grey-2 :background ,grey-6))))
   `(company-tooltip-annotation           ((t (:foreground ,grey-1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,grey-1))))
   `(company-tooltip-common               ((t (:foreground ,fg))))
   `(company-tooltip-common-selection     ((t (:weight bold))))
   `(company-tooltip-mouse                ((t (:background ,slate :foreground ,pale-blue))))
   `(company-tooltip-search               ((t (:foreground ,green))))
   `(company-tooltip-search-selection     ((t (:foreground ,green :weight bold))))
   `(company-tooltip-selection            ((t (:background ,grey-5 :weight bold))))
   `(company-scrollbar-bg                 ((t (:background ,grey-6 :foreground ,blue))))
   `(company-scrollbar-fg                 ((t (:foreground ,grey-7 :background ,blue))))

   `(term-color-black   ((t (:foreground ,grey-5))))
   `(term-color-red     ((t (:foreground ,red))))
   `(term-color-green   ((t (:foreground ,green))))
   `(term-color-yellow  ((t (:foreground ,yellow))))
   `(term-color-blue    ((t (:foreground ,blue))))
   `(term-color-magenta ((t (:foreground ,orange))))
   `(term-color-cyan    ((t (:foreground ,pale-blue))))
   `(term-color-white   ((t (:foreground ,grey-1))))

   `(projectile-tab-bar-modeline-active-face ((t (:foreground ,green :weight bold))))

   `(my/elfeed-blue   ((t (:foreground ,blue))))
   `(my/elfeed-cyan   ((t (:foreground ,pale-blue))))
   `(my/elfeed-green  ((t (:foreground ,green))))
   `(my/elfeed-yellow ((t (:foreground ,yellow))))
   `(my/elfeed-red    ((t (:foreground ,red))))
   `(my/elfeed-pink   ((t (:foreground ,orange))))
   )

  (custom-theme-set-variables
   'predawn
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

(provide-theme 'predawn)

;;; predawn-theme.el ends here
