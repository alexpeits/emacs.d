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

(custom-theme-set-faces
 'monad

 '(cursor              ((t (:background "#99d1ce"))))
 '(default             ((t (:foreground "#b1cbcb" :background "#181818"))))
 '(escape-glyph        ((t (:foreground "orange2"))))
 '(highlight           ((t (:background "#3c464a"))))
 '(hl-line             ((t (:background "#252a2a"))))
 '(region              ((t (:background "#004870"))))
 '(fringe              ((t (:foreground "#91abab" :background "#242424"))))
 '(vertical-border     ((t (:foreground "#6e6e6e"))))
 '(shadow              ((t (:foreground "#949898"))))
 '(secondary-selection ((t (:inherit highlight))))
 '(show-paren-match    ((t (:background "#456665" :weight bold))))
 '(link                ((t (:foreground "#56dbdb" :underline t))))
 '(warning             ((t (:foreground "orange2" :weight bold))))
 '(whitespace-trailing ((t (:background "#602020"))))
 '(fixed-pitch         ((t (:inherit default))))

 ;; emacs >= 27
 '(fill-column-indicator ((t (:foreground "#494b4b"))))

 ;; Line Numbers
 ;; emacs >= 26
 '(line-number              ((t (:foreground "#626868" :background "#202121"))))
 '(line-number-current-line ((t (:inherit line-number :background "#2e2f2f"))))
 ;; emacs < 26
 '(linum ((t (:foreground "#626868" :background "#202121"))))

 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line           ((t (:foreground "#6abd17" :background "#2e342e"))))
 '(mode-line-inactive  ((t (:foreground "#676d61" :background "#262c26"))))

 `(powerline-active0               ((t (:inherit mode-line :background "#3c473c"))))
 `(powerline-active1               ((t (:inherit mode-line))))
 `(powerline-active2               ((t (:inherit powerline-active0 :background "#232823"))))
 `(powerline-inactive0             ((t (:inherit mode-line-inactive))))
 `(powerline-inactive1             ((t (:inherit powerline-inactive0))))
 `(powerline-inactive2             ((t (:inherit powerline-inactive0))))
 '(powerline-active0-evil-insert   ((t (:inherit powerline-active0 :foreground "#0dd1b8"))))
 '(powerline-active0-evil-visual   ((t (:inherit powerline-active0 :foreground "#2faef5"))))
 '(powerline-active0-evil-replace  ((t (:inherit powerline-active0 :foreground "#ed4563"))))
 '(powerline-active0-evil-emacs    ((t (:inherit powerline-active0 :foreground "#bcbf0b"))))
 '(powerline-active0-evil-operator ((t (:inherit powerline-active0 :foreground "#bcbf0b"))))
 '(powerline-active2-edited        ((t (:inherit powerline-active2 :foreground "#bd3e54"))))
 ;; '(powerline-active0-edited        ((t (:inherit powerline-active0 :foreground "#ed4563"))))

 `(minibuffer-prompt ((t (:weight bold :inherit font-lock-function-name-face))))

 '(persp-selected-face ((t (:underline t :weight bold))))

 '(isearch-fail   ((t (:foreground "#3f4758" :background "salmon"))))
 '(isearch        ((t (:foreground "black" :background "#c46485"))))
 '(lazy-highlight ((t (:foreground "#a0a8b0" :background "#3d464f"))))

 '(font-lock-negation-char-face        ((t (:foreground "#cb5a37"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#f66500"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "red"))))
 '(font-lock-warning-face              ((t (:foreground "#cb5a37"))))

 '(font-lock-preprocessor-face      ((t (:foreground "#79c779"))))
 `(font-lock-comment-face           ((t (:foreground "#698080" :slant italic))))
 `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 `(font-lock-function-name-face     ((t (:foreground "#00afaf"))))
 `(font-lock-constant-face          ((t (:inherit font-lock-builtin-face))))
 `(font-lock-builtin-face           ((t (:foreground "#7dccf0"))))
 `(font-lock-keyword-face           ((t (:foreground "#a0bebe" :weight bold))))
 `(font-lock-string-face            ((t (:foreground "#c46485"))))
 `(font-lock-doc-face               ((t (:inherit font-lock-string-face))))
 `(font-lock-type-face              ((t (:foreground "#819fc7"))))
 `(font-lock-variable-name-face     ((t (:inherit font-lock-builtin-face))))

 `(haskell-constructor-face ((t (:inherit
                                 ,(if monad-theme-distinct-haskell-constructor
                                      font-lock-builtin-face
                                    font-lock-type-face)))))
 `(haskell-operator-face    ((t (:inherit
                                 ,(if monad-theme-distinct-haskell-constructor
                                      font-lock-type-face
                                    font-lock-builtin-face)))))

 `(purescript-constructor-face ((t (:inherit haskell-constructor-face))))
 `(purescript-pragma-face      ((t (:inherit haskell-pragma-face))))

 '(tuareg-font-lock-governing-face ((t (:inherit font-lock-keyword-face))))
 '(tuareg-font-lock-operator-face  ((t (:inherit font-lock-builtin-face))))

 '(coq-solve-tactics-face  ((t (:foreground "#d15681"))))
 '(coq-cheat-face          ((t (:inherit hi-red-b))))
 '(coq-button-face         ((t (:inherit hi-green-b))))
 '(coq-button-face-pressed ((t (:inherit hi-green-b :background "grey43"))))

 '(proof-tactics-name-face     ((t (:foreground "#c17bc9"))))
 '(proof-error-face            ((t (:inherit hi-red-b))))
 '(proof-locked-face           ((t (:background "#222f27"))))
 '(proof-queue-face            ((t (:background "#36262c"))))
 '(proof-warning-face          ((t (:background "#524a37"))))
 '(proof-declaration-name-face ((t (:inherit font-lock-function-name-face :weight bold))))

 `(nix-attribute-face ((t (:inherit font-lock-function-name-face))))
 `(nix-constant-face  ((t (:inherit font-lock-type-face))))
 `(nix-builtin-face   ((t (:inherit font-lock-preprocessor-face))))

 '(sh-quoted-exec ((t (:inherit font-lock-preprocessor-face))))
 '(sh-heredoc     ((t (:inherit font-lock-preprocessor-face))))

 `(diff-hl-insert ((t (:background "#183918" :foreground "#4c934c"))))
 `(diff-hl-change ((t (:background "#1a2d4c" :foreground "#298ed5"))))
 `(diff-hl-delete ((t (:background "#4c1515" :foreground "#bc4d4d"))))

 `(git-gutter+-added    ((t (:foreground "#4c934c" :weight bold))))
 `(git-gutter+-modified ((t (:foreground "#298ed5" :weight bold))))
 `(git-gutter+-deleted  ((t (:foreground "#bc4d4d" :weight bold))))

 ;; diff- and magit-diff- stuff copied from modus-vivendi theme
 `(diff-added             ((t (:foreground "#b4ddb4" :background "#244024"))))
 `(diff-changed           ((t (:foreground "#d0daaf" :background "#4a3a10"))))
 `(diff-removed           ((t (:foreground "#eebdba" :background "#542222"))))
 `(diff-refine-added      ((t (:foreground "#e0ffe0" :background "#005a00"))))
 `(diff-refine-changed    ((t (:foreground "#ffffcc" :background "#585800"))))
 `(diff-refine-removed    ((t (:foreground "#ffc8bb" :background "#7d0000"))))
 `(diff-indicator-added   ((t (:inherit diff-added))))
 `(diff-indicator-changed ((t (:inherit diff-changed))))
 `(diff-indicator-removed ((t (:inherit diff-removed))))
 `(diff-header            ((t (:inherit font-lock-type-face))))
 `(diff-hunk-header       ((t (:inherit region :weight bold))))
 `(diff-file-header       ((t (:foreground "#33beff" :weight bold))))
 `(diff-function          ((t (:foreground "#99d1ce"))))
 `(diff-context           ((t (:inherit shadow))))

 `(magit-diff-added                  ((t (:foreground "#94ba94" :background "#002600"))))
 `(magit-diff-base                   ((t (:foreground "#b0ba9f" :background "#2a2000"))))
 `(magit-diff-removed                ((t (:foreground "#bbadaa" :background "#390a0a"))))
 `(magit-diff-added-highlight        ((t (:inherit diff-added))))
 `(magit-diff-base-highlight         ((t (:inherit diff-changed))))
 `(magit-diff-removed-highlight      ((t (:inherit diff-removed))))
 `(magit-diff-hunk-heading           ((t (:background "grey25" :foreground "grey70" :weight bold))))
 `(magit-diff-hunk-heading-highlight ((t (:inherit region :foreground "grey80" :weight bold))))
 `(magit-diff-context-highlight      ((t (:background "grey15" region :foreground "grey80"))))
 `(magit-section-heading             ((t (:inherit font-lock-function-name-face :weight bold))))
 `(magit-section-highlight           ((t (:inherit hl-line))))

 `(change-log-date           ((t (:foreground "#d7afd7"))))
 `(change-log-name           ((t (:inherit font-lock-function-name-face))))
 `(change-log-email          ((t (:inherit change-log-name))))
 `(change-log-acknowledgment ((t (:foreground "#b3ae86"))))

 '(hi-green   ((t (:background "#6d997a" :foreground "black"))))
 '(hi-blue    ((t (:background "#659fad" :foreground "black"))))
 '(hi-pink    ((t (:background "#c9717a" :foreground "black"))))
 '(hi-yellow  ((t (:background "#99986d" :foreground "black"))))
 '(hi-green-b ((t (:background "#354a3c" :foreground "#8ec19d" :weight bold))))
 '(hi-blue-b  ((t (:background "#374b4f" :foreground "#8bc8d6" :weight bold))))
 '(hi-red-b   ((t (:background "#453237" :foreground "#cf7e8a" :weight bold))))
 '(hi-black-b ((t (:background "#444532" :foreground "#b5af82" :weight bold))))

 `(match ((t (:foreground "#b4e0de" :background "#004d4d"))))

 `(ivy-current-match           ((t (:inherit match :weight bold :underline t))))
 `(ivy-minibuffer-match-face-1 ((t (:inherit highlight :weight bold))))
 `(ivy-minibuffer-match-face-2 ((t (:inherit hi-black-b))))
 `(ivy-minibuffer-match-face-3 ((t (:inherit hi-blue-b))))
 `(ivy-minibuffer-match-face-4 ((t (:inherit hi-red-b))))
 `(ivy-grep-info               ((t (:inherit font-lock-type-face :weight bold))))
 `(ivy-grep-line-number        ((t (:inherit font-lock-string-face))))

 `(rg-line-number-face ((t (:inherit font-lock-type-face))))

 `(ag-hit-face ((t (:inherit font-lock-type-face :weight bold))))

 '(dired-directory ((t (:foreground "#499ec4" :weight bold))))
 '(dired-symlink   ((t (:foreground "#298D86" :weight bold))))
 '(dired-marked    ((t (:inherit hi-black-b))))
 '(dired-flagged   ((t (:inherit hi-red-b))))

 `(header-line ((t (:foreground "#a0b5b5" :background "#304545"))))

 `(eww-valid-certificate   ((t (:inherit font-lock-preprocessor-face :weight bold))))
 `(eww-invalid-certificate ((t (:foreground "#fb4933" :weight bold))))

 `(compilation-info           ((t (:foreground "#9bd164"))))
 `(compilation-warning        ((t (:foreground "#d09f07"))))
 `(compilation-error          ((t (:foreground "salmon"))))
 `(compilation-mode-line-exit ((t (:inherit compilation-info :weight bold))))
 `(compilation-mode-line-fail ((t (:inherit compilation-error :weight bold))))

 `(flycheck-error          ((t (:underline (:color "red1" :style wave)))))
 `(flycheck-fringe-error   ((t (:foreground "#FB4933"))))
 `(flycheck-info           ((t (:underline (:color "DeepSkyBlue2" :style wave)))))
 `(flycheck-fringe-info    ((t (:foreground "DeepSkyBlue2"))))
 `(flycheck-warning        ((t (:underline (:color "orange1" :style wave)))))
 `(flycheck-fringe-warning ((t (:foreground "orange1"))))

 `(flyspell-incorrect ((t (:underline (:color "#dc322f" :style wave)))))
 `(flyspell-duplicate ((t (:underline (:color "#bd9108" :style wave)))))

 '(anzu-mode-line          ((t (:inherit mode-line :foreground "magenta" :weight bold))))
 '(anzu-mode-line-no-match ((t (:inherit mode-line :foreground "red" :weight bold))))

 '(org-block            ((t (:foreground "#bbccc5" :background "#191e1d"))))
 '(org-block-begin-line ((t (:foreground "#768e84" :background "#1e2623" :slant italic))))
 '(org-block-end-line   ((t (:inherit org-block-begin-line))))
 '(org-hide             ((t (:inherit fill-column-indicator))))
 '(org-ellipsis         ((t (:inherit font-lock-preprocessor-face :underline t))))

 '(outline-1 ((t (:inherit font-lock-function-name-face))))
 '(outline-2 ((t (:inherit font-lock-builtin-face))))
 '(outline-3 ((t (:inherit font-lock-type-face))))
 '(outline-4 ((t (:inherit font-lock-keyword-face :weight normal))))
 '(outline-5 ((t (:inherit outline-4))))
 '(outline-6 ((t (:inherit outline-4))))

 `(markdown-code-face             ((t (:inherit font-lock-type-face))))
 `(markdown-inline-code-face      ((t (:inherit font-lock-builtin-face))))
 `(markdown-pre-face              ((t (:inherit markdown-code-face))))
 `(markdown-url-face              ((t (:inherit link))))
 `(markdown-header-face           ((t (:inherit font-lock-string-face :weight bold))))
 `(markdown-language-keyword-face ((t (:inherit font-lock-function-name-face))))
 `(markdown-blockquote-face       ((t (:inherit font-lock-keyword-face :weight normal :slant italic))))

 `(rst-literal    ((t (:inherit font-lock-type-face))))
 `(rst-directive  ((t (:inherit font-lock-preprocessor-face))))
 `(rst-transition ((t (:inherit font-lock-preprocessor-face))))
 `(rst-adornment  ((t (:inherit font-lock-string-face :weight bold))))
 `(rst-level-1    ((t (:inherit font-lock-string-face :weight bold))))
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

 '(imenu-list-entry-face-0          ((t (:inherit font-lock-function-name-face ))))
 '(imenu-list-entry-subalist-face-0 ((t (:inherit imenu-list-entry-face-0 :weight bold))))
 '(imenu-list-entry-face-1          ((t (:inherit font-lock-type-face))))
 '(imenu-list-entry-subalist-face-1 ((t (:inherit imenu-list-entry-face-1 :weight bold))))
 '(imenu-list-entry-face-2          ((t (:foreground "#c1eae2"))))
 '(imenu-list-entry-subalist-face-2 ((t (:inherit imenu-list-entry-face-2 :weight bold))))
 '(imenu-list-entry-face-3          ((t (:foreground "#b2d1cb"))))
 '(imenu-list-entry-subalist-face-3 ((t (:inherit imenu-list-entry-face-3 :weight bold))))

 '(elfeed-search-title-face        ((t (:inherit shadow :slant italic :background "#3a4040"))))
 '(elfeed-search-unread-title-face ((t (:inherit default :slant normal :foreground "#bcc6ce"))))
 '(elfeed-search-feed-face         ((t (:foreground "#b5a52b"))))
 '(elfeed-search-tag-face          ((t (:foreground "#79c779"))))

 '(erc-timestamp-face ((t (:inherit font-lock-string-face :weight bold))))
 '(erc-prompt-face    ((t (:inherit font-lock-keyword-face :weight bold))))
 '(erc-action-face    ((t (:inherit font-lock-string-face))))

 '(message-header-name    ((t (:foreground "#79c779"))))
 '(message-header-subject ((t (:inherit font-lock-function-name-face :weight bold))))
 '(message-header-to      ((t (:inherit font-lock-function-name-face))))
 '(message-header-other   ((t (:foreground "#b5a52b"))))

 '(shr-link ((t (:foreground "#47bcb3" :underline t))))

 '(highlight-indent-guides-character-face ((t (:foreground "#606060"))))

 ;; TODO? (solarized)
 '(company-preview                      ((t (:inherit default :background "#304540"))))
 '(company-preview-common               ((t (:inherit company-preview :slant italic))))
 '(company-tooltip                      ((t (:foreground "#b1cbcb" :background "#252525"))))
 '(company-tooltip-annotation           ((t (:foreground "#00afaf"))))
 '(company-tooltip-annotation-selection ((t (:foreground "#00afaf"))))
 '(company-tooltip-common               ((t (:foreground "#b1cbcb"))))
 '(company-tooltip-common-selection     ((t (:weight bold))))
 '(company-tooltip-mouse                ((t (:background "#00afaf" :foreground "#304540"))))
 '(company-tooltip-search               ((t (:foreground "#c46485"))))
 '(company-tooltip-search-selection     ((t (:foreground "#c46485" :weight bold))))
 '(company-tooltip-selection            ((t (:background "#353535" :weight bold))))
 '(company-scrollbar-bg                 ((t (:background "#252525" :foreground "#00afaf"))))
 '(company-scrollbar-fg                 ((t (:foreground "#1b1b1b" :background "#00afaf"))))

 '(term-color-black   ((t (:foreground "#808080"))))
 '(term-color-red     ((t (:foreground "#c46485"))))
 '(term-color-green   ((t (:foreground "#79c779"))))
 '(term-color-yellow  ((t (:foreground "#b5a52b"))))
 '(term-color-blue    ((t (:foreground "#499ec4"))))
 '(term-color-magenta ((t (:foreground "#819fc7"))))
 '(term-color-cyan    ((t (:foreground "#00afaf"))))
 '(term-color-white   ((t (:foreground "#b1cbcb"))))
 )

(custom-theme-set-variables
 'monad
 '(ibuffer-deletion-face 'dired-flagged)
 '(ibuffer-filter-group-name-face 'dired-mark)
 '(ibuffer-marked-face 'dired-marked)
 '(ibuffer-title-face 'dired-header)
 '(coq-highlighted-hyps-bg "#374b4f")
 '(ansi-color-names-vector
   ["#808080" "#c46485" "#79c779" "#b5a52b" "#499ec4" "#819fc7" "#00afaf" "#b1cbcb"]))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'monad)

;;; monad-theme.el ends here
