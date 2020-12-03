;;; monad-theme.el --- A dark theme
;;;
;;; Author: Alex Peitsinis <alexpeitsinis@gmail.com>
;;; Url: https://github.com/alexpeits/emacs-monad-theme
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
;;; The main inspiration comes from this screenshot found somewhere on reddit:
;;; http://ergoemacs.org/emacs/i/emacs_screenshot_Nick_Alcock_2014-03-07.png
;;;
;;; Code:

(deftheme monad
  "A dark theme")

(defvar monad-theme-distinct-haskell-constructor nil)

(let* ((bg "#181818")
       (fg "#b1cbcb")

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

       (cyan-faded-1 "#698080")
       (cyan-faded "#a0bebe")

       (cyan-pale-1 "#79b1ae")
       (cyan-pale   "#99d1ce")
       (cyan-pale+1 "#bfebe0")

       (cyan-2 "#003535")
       (cyan-1 "#005757")
       (cyan   "#00afaf")
       (cyan+1 "#5fd7d7")
       (cyan+2 "#c1eae2")

       (green-2 "#183918")
       (green-1 "#5f875f")
       (green   "#4c934c")
       (green+1 "#5faf5f")
       (green+2 "#87d7af")

       (green-bright-1 "#006800")
       (green-bright "#6abd17")
       (green-bright+1 "#79c779")

       (blue-pale-1 "#1a2d4c")
       (blue-pale   "#87afd7")

       (blue-3 "#003347")
       (blue-2 "#004065")
       (blue-1 "#00466a")
       (blue   "#5fafd7")
       (blue+1 "#7dccf0")
       (blue+2 "#8ae4f2")

       (blue-bright-1 "#298ed5")
       (blue-bright   "#00aee6")

       (magenta-pale   "#819fc7")

       (magenta   "#d7afd7")

       (yellow-1 "#524a37")
       (yellow   "#d7af5f")

       (orange "#d7875f")

       (red-pastel "#c46485")

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

       (haskell-constructor-fg
        (if monad-theme-distinct-haskell-constructor blue+1 magenta-pale))
       (haskell-operator-fg
        (if monad-theme-distinct-haskell-constructor magenta-pale blue+1))
       )
  (custom-theme-set-faces
   'monad

   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor  ((t (:background ,cyan-pale))))
   `(region  ((t (:background ,blue-2))))

   `(fringe              ((t (:foreground ,grey+2 :background ,grey-6))))
   `(vertical-border     ((t (:foreground ,grey))))
   `(link                ((t (:foreground ,blue-bright :underline t))))
   `(italic              ((t (:foreground ,cyan-pale :slant italic))))
   `(hl-line             ((t (:background ,grey-6))))
   `(show-paren-match    ((t (:background ,red-pale-1 :foreground ,fg))))
   `(match               ((t (:background ,blue-1 :foreground ,fg))))
   `(whitespace-trailing ((t (:background ,red-2))))

   `(shadow              ((t (:foreground ,grey+1))))
   `(highlight           ((t (:background ,grey-4))))
   `(secondary-selection ((t (:inherit highlight))))
   `(escape-glyph        ((t (:foreground ,orange))))
   `(warning             ((t (:foreground ,yellow :weight bold))))

   ;; emacs >= 27
   `(fill-column-indicator ((t (:foreground ,grey-2))))

   ;; Line Numbers
   ;; emacs >= 26
   `(line-number              ((t (:foreground ,grey-1 :background ,grey-6))))
   `(line-number-current-line ((t (:inherit line-number :background ,grey-4))))
   ;; emacs < 26
   `(linum ((t (:foreground ,grey-1 :background ,grey-6))))

   `(mode-line-buffer-id ((t (:weight bold))))
   `(mode-line           ((t (:foreground ,green-bright :background ,grey-4))))
   `(mode-line-inactive  ((t (:foreground ,grey :background ,grey-5))))

   `(powerline-active0               ((t (:inherit mode-line :background ,grey-3))))
   `(powerline-active1               ((t (:inherit mode-line))))
   `(powerline-active2               ((t (:inherit powerline-active0 :background ,grey-5))))
   `(powerline-inactive0             ((t (:inherit mode-line-inactive))))
   `(powerline-inactive1             ((t (:inherit powerline-inactive0))))
   `(powerline-inactive2             ((t (:inherit powerline-inactive0))))

   `(minibuffer-prompt ((t (:foreground ,cyan :weight bold))))

   '(persp-selected-face ((t (:underline t :weight bold))))

   `(isearch-fail   ((t (:foreground ,red+2 :background ,red-2))))
   `(isearch        ((t (:foreground ,fg :background ,green-bright-1 :weight bold))))
   `(lazy-highlight ((t (:foreground ,blue+2 :background ,blue-2))))

   `(evil-ex-substitute-matches     ((t (:inherit hi-blue))))
   `(evil-ex-substitute-replacement ((t (:inherit hi-yellow))))

   `(font-lock-negation-char-face        ((t (:foreground ,orange))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,yellow))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,red))))
   `(font-lock-warning-face              ((t (:foreground ,yellow))))

   `(font-lock-preprocessor-face      ((t (:foreground ,green-bright+1))))
   `(font-lock-comment-face           ((t (:foreground ,cyan-faded-1 :slant italic))))
   `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-doc-face               ((t (:inherit font-lock-string-face))))
   `(font-lock-string-face            ((t (:foreground ,red-pastel))))
   `(font-lock-constant-face          ((t (:inherit font-lock-builtin-face))))
   `(font-lock-builtin-face           ((t (:foreground ,blue+1))))
   `(font-lock-variable-name-face     ((t (:inherit font-lock-builtin-face))))
   `(font-lock-keyword-face           ((t (:foreground ,cyan-faded :weight bold))))
   `(font-lock-function-name-face     ((t (:foreground ,cyan))))
   `(font-lock-type-face              ((t (:foreground ,magenta-pale))))

   `(haskell-constructor-face ((t (:foreground ,haskell-constructor-fg))))
   `(haskell-operator-face    ((t (:foreground ,haskell-operator-fg))))

   `(purescript-constructor-face ((t (:inherit haskell-constructor-face))))

   `(tuareg-font-lock-governing-face ((t (:inherit font-lock-keyword-face))))
   `(tuareg-font-lock-operator-face  ((t (:inherit font-lock-builtin-face))))

   `(coq-solve-tactics-face  ((t (:foreground ,red-pastel))))
   `(coq-cheat-face          ((t (:inherit hi-red-b))))
   `(coq-button-face         ((t (:inherit hi-green-b))))
   `(coq-button-face-pressed ((t (:inherit hi-green-b :background ,grey-1))))

   `(proof-tactics-name-face     ((t (:foreground ,magenta))))
   `(proof-error-face            ((t (:inherit hi-red-b))))
   `(proof-locked-face           ((t (:background ,cyan-2))))
   `(proof-queue-face            ((t (:background ,red-3))))
   `(proof-warning-face          ((t (:background ,yellow-1))))
   `(proof-declaration-name-face ((t (:inherit font-lock-function-name-face :weight bold))))

   `(nix-attribute-face ((t (:inherit font-lock-function-name-face))))
   `(nix-constant-face  ((t (:inherit font-lock-type-face))))
   `(nix-builtin-face   ((t (:inherit font-lock-preprocessor-face))))

   `(enh-ruby-heredoc-delimiter-face ((t (:inherit font-lock-string-face))))
   `(enh-ruby-op-face ((t (:foreground ,fg))))
   `(enh-ruby-regexp-delimiter-face ((t (:foreground ,green+1))))
   `(enh-ruby-regexp-face ((t (:foreground ,magenta))))
   `(enh-ruby-string-delimiter-face ((t (:inherit font-lock-string-face))))

   `(sh-quoted-exec ((t (:inherit font-lock-preprocessor-face))))
   `(sh-heredoc     ((t (:foreground ,magenta))))

   `(diff-hl-insert ((t (:background ,green-2 :foreground ,green))))
   `(diff-hl-change ((t (:background ,blue-pale-1 :foreground ,blue-bright-1))))
   `(diff-hl-delete ((t (:background ,red-3 :foreground ,red-1))))

   `(git-gutter+-added    ((t (:foreground ,green :weight bold))))
   `(git-gutter+-modified ((t (:foreground ,blue-bright-1 :weight bold))))
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
   `(magit-diff-hunk-heading           ((t (:background ,grey-5 :foreground ,grey+2 :weight bold))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,blue-2 :foreground ,cyan+2 :weight bold))))
   `(magit-diff-context-highlight      ((t (:background ,grey-7 region :foreground ,grey+3))))
   `(magit-section-heading             ((t (:foreground ,fg :weight bold))))
   `(magit-section-highlight           ((t (:inherit hl-line))))
   `(magit-branch-local                ((t (:foreground ,blue-bright))))
   `(magit-branch-remote               ((t (:foreground ,magenta))))
   `(magit-tag                         ((t (:foreground ,red-pastel))))

   `(change-log-date           ((t (:foreground ,magenta))))
   `(change-log-name           ((t (:foreground ,cyan))))
   `(change-log-email          ((t (:inherit change-log-name))))
   `(change-log-acknowledgment ((t (:foreground ,fg))))

   `(hi-green   ((t (:background ,grey-5 :foreground ,hi-green-fg :underline t))))
   `(hi-blue    ((t (:background ,grey-5 :foreground ,hi-blue-fg :underline t))))
   `(hi-pink    ((t (:background ,grey-5 :foreground ,hi-red-fg :underline t))))
   `(hi-yellow  ((t (:background ,grey-5 :foreground ,hi-yellow-fg :underline t))))
   `(hi-green-b ((t (:background ,hi-green-bg :foreground ,hi-green-fg :weight bold))))
   `(hi-blue-b  ((t (:background ,hi-blue-bg :foreground ,hi-blue-fg :weight bold))))
   `(hi-red-b   ((t (:background ,hi-red-bg :foreground ,hi-red-fg :weight bold))))
   `(hi-black-b ((t (:background ,hi-yellow-bg :foreground ,hi-yellow-fg :weight bold))))

   `(ivy-current-match           ((t (:inherit match :weight bold :underline t))))
   `(ivy-minibuffer-match-face-1 ((t (:inherit highlight :weight bold))))
   `(ivy-minibuffer-match-face-2 ((t (:inherit hi-green-b :weight bold))))
   `(ivy-minibuffer-match-face-3 ((t (:inherit hi-black-b :weight bold))))
   `(ivy-minibuffer-match-face-4 ((t (:inherit hi-red-b :weight bold))))
   `(ivy-grep-info               ((t (:foreground ,cyan+1))))
   `(ivy-grep-line-number        ((t (:foreground ,red-pastel))))
   `(ivy-highlight-face          ((t (:foreground ,magenta))))

   `(swiper-match-face-1 ((t (:background ,blue-3 :foreground ,blue+2))))
   `(swiper-match-face-2 ((t (:inherit hi-gren-b :weight normal))))
   `(swiper-match-face-3 ((t (:inherit hi-red-b :weight normal))))
   `(swiper-match-face-4 ((t (:inherit hi-black-b :weight normal))))

   `(helm-source-header    ((t (:inherit hi-blue-b :height 1.3))))
   `(helm-selection        ((t (:inherit ivy-current-match :underline nil))))
   `(helm-candidate-number ((t (:inherit hi-blue-b))))

   `(ag-hit-face ((t (:foreground ,cyan+1 :weight bold))))

   `(aw-leading-char-face ((t (:foreground ,green-bright+1 :weight bold :height 2.5))))

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
   `(diredfl-number      ((t (:foreground ,cyan))))
   `(diredfl-date-time   ((t (:foreground ,cyan-pale-1))))
   `(diredfl-file-name   ((t (:foreground ,fg))))
   `(diredfl-file-suffix ((t (:foreground ,cyan-pale-1))))
   `(diredfl-dir-name    ((t (:inherit dired-directory))))
   `(diredfl-symlink     ((t (:inherit dired-symlink))))

   `(eshell-ls-directory  ((t (:inherit dired-directory))))
   `(eshell-ls-symlink    ((t (:inherit dired-symlink))))
   `(eshell-ls-executable ((t (:foreground ,green-bright+1 :weight bold))))

   `(header-line ((t (:foreground ,grey+3 :background ,grey-4))))

   `(Info-quoted      ((t (:foreground ,magenta))))
   `(info-header-node ((t (:foreground ,grey+2 :weight bold))))
   `(info-title-1     ((t (:foreground ,fg :weight bold :height 1.2))))
   `(info-title-2     ((t (:foreground ,cyan-pale+1 :weight bold))))
   `(info-title-3     ((t (:foreground ,magenta :weight bold))))
   `(info-title-4     ((t (:foreground ,grey+1 :weight bold))))

   `(eww-valid-certificate   ((t (:foreground ,green+2 :weight bold))))
   `(eww-invalid-certificate ((t (:foreground ,red+1 :weight bold))))

   `(compilation-info           ((t (:foreground ,green+2))))
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

   `(anzu-mode-line          ((t (:foreground ,cyan :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,red-pastel :weight bold))))

   `(rainbow-delimiters-depth-1-face    ((t (:inherit font-lock-builtin-face))))
   `(rainbow-delimiters-depth-2-face    ((t (:inherit font-lock-function-name-face))))
   `(rainbow-delimiters-depth-3-face    ((t (:inherit font-lock-preprocessor-face))))
   `(rainbow-delimiters-depth-4-face    ((t (:inherit font-lock-type-face))))
   `(rainbow-delimiters-depth-5-face    ((t (:inherit rainbow-delimiters-depth-1-face))))
   `(rainbow-delimiters-depth-6-face    ((t (:inherit rainbow-delimiters-depth-2-face))))
   `(rainbow-delimiters-depth-7-face    ((t (:inherit rainbow-delimiters-depth-3-face))))
   `(rainbow-delimiters-depth-8-face    ((t (:inherit rainbow-delimiters-depth-4-face))))
   `(rainbow-delimiters-depth-9-face    ((t (:inherit rainbow-delimiters-depth-1-face))))
   `(rainbow-delimiters-unmatched-face  ((t (:inherit font-lock-warning-face))))
   `(rainbow-delimiters-mismatched-face ((t (:inherit font-lock-warning-face))))

   `(org-block            ((t (:foreground ,grey+4 :background ,grey-7 :extend t))))
   `(org-block-begin-line ((t (:foreground ,grey :background ,grey-6 :slant italic :extend t))))
   `(org-block-end-line   ((t (:inherit org-block-begin-line))))
   `(org-hide             ((t (:foreground ,bg))))
   `(org-indent           ((t (:foreground ,bg :background ,bg))))
   `(org-ellipsis         ((t (:foreground ,green-bright+1 :underline t))))
   `(org-list-dt          ((t (:foreground ,magenta-pale))))
   `(org-level-1          ((t (:inherit outline-1 :height 1.2))))

   `(org-roam-link         ((t (:foreground ,green-bright+1 :underline t))))
   `(org-roam-link-invalid ((t (:inherit font-lock-warning-face :underline t))))

   `(outline-1 ((t (:foreground ,cyan))))
   `(outline-2 ((t (:foreground ,red-pastel))))
   `(outline-3 ((t (:foreground ,green-bright+1))))
   `(outline-4 ((t (:foreground ,magenta-pale))))
   `(outline-5 ((t (:foreground ,blue+1))))
   `(outline-6 ((t (:foreground ,cyan-faded :weight normal))))
   `(outline-7 ((t (:inherit outline-1))))
   `(outline-8 ((t (:inherit outline-2))))

   `(markdown-code-face             ((t (:foreground ,magenta-pale))))
   `(markdown-inline-code-face      ((t (:foreground ,blue+1))))
   `(markdown-pre-face              ((t (:inherit markdown-code-face))))
   `(markdown-url-face              ((t (:inherit link))))
   `(markdown-header-face           ((t (:foreground ,red-pastel :weight bold))))
   `(markdown-language-keyword-face ((t (:foreground ,cyan))))
   `(markdown-blockquote-face       ((t (:foreground ,cyan-faded :weight normal :slant italic))))

   `(rst-literal    ((t (:foreground ,magenta-pale))))
   `(rst-directive  ((t (:foreground ,green-bright+1))))
   `(rst-transition ((t (:foreground ,green-bright+1))))
   `(rst-adornment  ((t (:foreground ,red-pastel :weight bold))))
   `(rst-level-1    ((t (:inherit rst-adornment))))
   `(rst-level-2    ((t (:inherit rst-level-1))))
   `(rst-level-3    ((t (:inherit rst-level-1))))
   `(rst-level-4    ((t (:inherit rst-level-1))))
   `(rst-level-5    ((t (:inherit rst-level-1))))
   `(rst-level-6    ((t (:inherit rst-level-1))))

   `(markup-gen-face                 ((t (:inherit font-lock-type-face))))
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

   `(imenu-list-entry-face-0          ((t (:inherit font-lock-function-name-face ))))
   `(imenu-list-entry-subalist-face-0 ((t (:inherit imenu-list-entry-face-0 :weight bold))))
   `(imenu-list-entry-face-1          ((t (:inherit font-lock-type-face))))
   `(imenu-list-entry-subalist-face-1 ((t (:inherit imenu-list-entry-face-1 :weight bold))))
   `(imenu-list-entry-face-2          ((t (:foreground ,grey+4))))
   `(imenu-list-entry-subalist-face-2 ((t (:inherit imenu-list-entry-face-2 :weight bold))))
   `(imenu-list-entry-face-3          ((t (:foreground ,grey+3))))
   `(imenu-list-entry-subalist-face-3 ((t (:inherit imenu-list-entry-face-3 :weight bold))))

   `(elfeed-search-title-face        ((t (:inherit default :slant italic))))
   `(elfeed-search-unread-title-face ((t (:inherit default :weight bold))))
   `(elfeed-search-feed-face         ((t (:foreground ,yellow))))
   `(elfeed-search-tag-face          ((t (:foreground ,green-bright+1))))

   `(erc-timestamp-face ((t (:foreground ,red-pastel :weight bold))))
   `(erc-prompt-face    ((t (:foreground ,cyan-faded :weight bold))))
   `(erc-action-face    ((t (:foreground ,red-pastel))))

   `(message-header-name    ((t (:foreground ,green-bright+1))))
   `(message-header-subject ((t (:foreground ,cyan :weight bold))))
   `(message-header-to      ((t (:foreground ,cyan))))
   `(message-header-other   ((t (:foreground ,yellow))))

   `(shr-link ((t (:foreground ,cyan+1 :underline t))))

   `(highlight-indent-guides-character-face ((t (:foreground ,grey-1))))

   `(company-preview                      ((t (:inherit default :background ,grey-4))))
   `(company-preview-common               ((t (:inherit company-preview :slant italic))))
   `(company-tooltip                      ((t (:foreground ,grey+4 :background ,grey-6))))
   `(company-tooltip-annotation           ((t (:foreground ,cyan-1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,cyan-1))))
   `(company-tooltip-common               ((t (:foreground ,grey+4))))
   `(company-tooltip-common-selection     ((t (:weight bold))))
   `(company-tooltip-mouse                ((t (:background ,cyan-1 :foreground ,cyan-2))))
   `(company-tooltip-search               ((t (:foreground ,red-pastel))))
   `(company-tooltip-search-selection     ((t (:foreground ,red-pastel :weight bold))))
   `(company-tooltip-selection            ((t (:background ,grey-4 :weight bold))))
   `(company-scrollbar-bg                 ((t (:background ,grey-6 :foreground ,cyan-1))))
   `(company-scrollbar-fg                 ((t (:foreground ,grey-7 :background ,cyan-1))))

   `(term-color-black   ((t (:foreground ,grey-3))))
   `(term-color-red     ((t (:foreground ,red-pastel))))
   `(term-color-green   ((t (:foreground ,green-bright+1))))
   `(term-color-yellow  ((t (:foreground ,yellow))))
   `(term-color-blue    ((t (:foreground ,blue))))
   `(term-color-magenta ((t (:foreground ,magenta-pale))))
   `(term-color-cyan    ((t (:foreground ,cyan))))
   `(term-color-white   ((t (:foreground ,grey+4))))
   )

  (custom-theme-set-variables
   'monad
   `(ibuffer-deletion-face 'dired-flagged)
   `(ibuffer-filter-group-name-face 'dired-mark)
   `(ibuffer-marked-face 'dired-marked)
   `(ibuffer-title-face 'dired-header)
   `(coq-highlighted-hyps-bg ,grey-4)
   `(ansi-color-names-vector
     [grey-3 red-pastel green-bright+1 yellow blue magenta-pale cyan grey+4])
   )
  )


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'monad)

;;; monad-theme.el ends here
