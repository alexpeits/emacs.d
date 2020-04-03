;;; functor-theme.el --- A dark theme inspired by vim-lucius
;;;
;;; Author: Alex Peitsinis <alexpeitsinis@gmail.com>
;;; Url: https://github.com/alexpeits/emacs-functor-theme
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
;;; Code:

(deftheme functor
  "A dark theme inspired by vim-lucius")

(defvar functor-theme-alt nil)

(defun -functor-theme-alt (reg alt)
  "Return REG if `functor-theme-alt' is nil, else ALT."
  (if functor-theme-alt alt reg))

(custom-theme-set-faces
 'functor

 '(cursor              ((t (:background "#99d1ce"))))
 '(default             ((t (:foreground "#bed3d3" :background "#1a1b1b"))))
 '(escape-glyph        ((t (:foreground "orange2"))))
 '(highlight           ((t (:background "#3d494d"))))
 '(hl-line             ((t (:background "#232828"))))
 '(region              ((t (:background "#004870"))))
 '(fringe              ((t (:background "#232828"))))
 '(vertical-border     ((t (:foreground "#758a86"))))
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
 '(mode-line           ((t (:foreground "#c1eae2" :background "#005555" :box (:line-width 1 :color "#003535")))))
 '(mode-line-inactive  ((t (:foreground "#387a71" :background "#272b2a" :box (:line-width 1 :color "#003535")))))

 `(powerline-active0   ((t (:background "#006553" :foreground "#c1eae2"))))
 `(powerline-active1   ((t (:background "#004c40" :foreground "#c1eae2"))))
 `(powerline-active2   ((t (:background "#003535" :foreground "#c1eae2"))))
 `(powerline-inactive0 ((t (:background "#1e2827" :foreground "#387a71"))))
 `(powerline-inactive1 ((t (:inherit powerline-inactive0))))
 `(powerline-inactive2 ((t (:inherit powerline-inactive0))))

 '(minibuffer-prompt ((t (:foreground "#12a89f" :weight bold))))

 '(persp-selected-face ((t (:inherit font-lock-function-name-face :weight bold))))

 '(isearch-fail   ((t (:foreground "#3f4758" :background "salmon"))))
 '(isearch        ((t (:inverse-video nil :foreground "black" :background "#db7e4c"))))
 '(lazy-highlight ((t (:foreground "#a0a8b0" :background "#3d464f"))))

 '(font-lock-negation-char-face        ((t (:foreground "#cb5a37"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#f66500"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "red"))))
 '(font-lock-warning-face              ((t (:foreground "#cb5a37"))))

 '(font-lock-preprocessor-face      ((t (:foreground "#8ba004"))))
 `(font-lock-comment-face           ((t (:foreground "#828787" :slant italic))))
 `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 `(font-lock-function-name-face     ((t (:foreground "#afd787"))))
 `(font-lock-constant-face          ((t (:foreground "#87d7af"))))
 `(font-lock-builtin-face           ((t (:foreground ,(-functor-theme-alt "#87d7ff" "#b4dbd3")))))
 `(font-lock-keyword-face           ((t ,(-functor-theme-alt '(:foreground "#30b0a6") '(:foreground "#86b8b5" :weight bold)))))
 `(font-lock-string-face            ((t (:foreground ,(-functor-theme-alt "#d7d7af" "#1da89f")))))
 `(font-lock-doc-face               ((t ,(-functor-theme-alt '(:foreground "#cacaa7") '(:inherit font-lock-string-face)))))
 `(font-lock-type-face              ((t (:foreground ,(-functor-theme-alt "#87d7d7" "#86b8b5")))))
 `(font-lock-variable-name-face     ((t (:inherit font-lock-builtin-face))))

 `(haskell-pragma-face         ((t (:foreground "#d7afd7"))))
 `(purescript-constructor-face ((t (:inherit haskell-constructor-face))))
 `(purescript-pragma-face      ((t (:inherit haskell-pragma-face))))

 '(tuareg-font-lock-governing-face ((t (:foreground "#d7afd7"))))
 '(tuareg-font-lock-operator-face  ((t (:inherit font-lock-variable-name-face))))

 ;; '(coq-solve-tactics-face  ((t (:foreground "red2"))))
 '(coq-solve-tactics-face  ((t (:foreground "#d15681"))))
 '(coq-cheat-face          ((t (:inherit hi-red-b))))
 '(proof-tactics-name-face ((t (:foreground "#bf8ade"))))
 '(proof-error-face        ((t (:inherit hi-red-b))))
 '(proof-locked-face       ((t (:background "#1f2923"))))
 '(proof-queue-face        ((t (:background "#36262c"))))
 '(proof-warning-face      ((t (:background "#524a37"))))

 `(nix-attribute-face ((t (:inherit font-lock-function-name-face))))

 '(sh-quoted-exec ((t (:inherit font-lock-preprocessor-face))))
 '(sh-heredoc     ((t (:foreground "#d7afd7"))))

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
 `(diff-header            ((t (:foreground "#819fc7"))))
 `(diff-hunk-header       ((t (:inherit region :weight bold))))
 `(diff-file-header       ((t (:foreground "#33beff" :weight bold))))
 `(diff-function          ((t (:foreground "#99d1ce"))))
 `(diff-context           ((t (:inherit shadow))))

 `(magit-diff-added             ((t (:foreground "#94ba94" :background "#002600"))))
 `(magit-diff-base              ((t (:foreground "#b0ba9f" :background "#2a2000"))))
 `(magit-diff-removed           ((t (:foreground "#bbadaa" :background "#390a0a"))))
 `(magit-diff-added-highlight   ((t (:inherit diff-added))))
 `(magit-diff-base-highlight    ((t (:inherit diff-changed))))
 `(magit-diff-removed-highlight ((t (:inherit diff-removed))))
 `(magit-section-heading        ((t (:foreground "#b3ae86":weight bold))))
 `(magit-section-highlight      ((t (:inherit hl-line))))

 `(change-log-date           ((t (:foreground "#d7afd7"))))
 `(change-log-name           ((t (:inherit font-lock-keyword-face))))
 `(change-log-email          ((t (:inherit change-log-name))))
 `(change-log-acknowledgment ((t (:foreground "#b3ae86"))))

 '(hi-green   ((t (:background "#6d997a" :foreground "black"))))
 '(hi-blue    ((t (:background "#659fad" :foreground "black"))))
 '(hi-pink    ((t (:background "#c9717a" :foreground "black"))))
 '(hi-yellow  ((t (:background "#99986d" :foreground "black"))))
 '(hi-green-b ((t (:background "#324538" :foreground "#8ec19d" :weight bold))))
 '(hi-blue-b  ((t (:background "#374b4f" :foreground "#8bc8d6" :weight bold))))
 '(hi-red-b   ((t (:background "#453237" :foreground "#cf7e8a" :weight bold))))
 '(hi-black-b ((t (:background "#3c3d2d" :foreground "#b5af82" :weight bold))))

 `(match ((t (:foreground "#abdedb" :background "#004c40"))))

 `(ivy-current-match           ((t (:inherit match :weight bold :underline t))))
 `(ivy-minibuffer-match-face-1 ((t (:inherit highlight :weight bold))))
 `(ivy-minibuffer-match-face-2 ((t (:inherit hi-black-b))))
 `(ivy-minibuffer-match-face-3 ((t (:inherit hi-blue-b))))
 `(ivy-minibuffer-match-face-4 ((t (:inherit hi-red-b))))
 `(ivy-grep-info               ((t (:foreground "#afd787" :weight bold))))

 `(ag-hit-face ((t (:foreground "#afd787" :weight bold))))

 '(dired-directory ((t (:foreground "#499ec4" :weight bold))))
 '(dired-symlink   ((t (:foreground "#2da49b" :weight bold))))
 '(dired-marked    ((t (:inherit hi-black-b))))
 '(dired-flagged   ((t (:inherit hi-red-b))))

 `(header-line ((t (:foreground "#a0b5b5" :background "#304545"))))

 `(eww-valid-certificate   ((t (:foreground "#8bd067" :weight bold))))
 `(eww-invalid-certificate ((t (:foreground "#fb4933" :weight bold))))

 `(compilation-info           ((t (:foreground "#afd787"))))
 `(compilation-warning        ((t (:foreground "#d09f07"))))
 `(compilation-error          ((t (:foreground "salmon"))))
 `(compilation-mode-line-exit ((t (:foreground "#afd787" :weight bold))))

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
 '(org-ellipsis         ((t (:foreground "#d7afd7" :underline t))))

 '(outline-1 ((t (:inherit font-lock-function-name-face))))
 '(outline-2 ((t (:foreground "#87d7ff"))))
 '(outline-3 ((t (:foreground "#62b49f"))))
 '(outline-4 ((t (:foreground "#2da49b"))))
 '(outline-5 ((t (:inherit font-lock-preprocessor-face))))
 '(outline-6 ((t (:inherit font-lock-preprocessor-face))))

 ;; `(markdown-code-face   ((t ,(-functor-theme-alt'(:inherit font-lock-keyword-face) '(:inherit font-lock-string-face)))))
 `(markdown-code-face       ((t (:inherit font-lock-doc-face))))
 `(markdown-pre-face        ((t (:inherit markdown-code-face))))
 `(markdown-url-face        ((t (:inherit link))))
 `(markdown-header-face     ((t (:inherit font-lock-preprocessor-face :weight bold))))
 `(markdown-blockquote-face ((t (:foreground "#95c2c0" :weight normal :slant italic))))

 `(rst-literal    ((t (:inherit font-lock-doc-face))))
 `(rst-directive  ((t (:foreground "#d7afd7"))))
 `(rst-transition ((t (:inherit font-lock-type-face))))
 `(rst-adornment  ((t (:inherit font-lock-preprocessor-face :weight bold))))
 `(rst-level-1    ((t (:inherit font-lock-preprocessor-face :weight bold))))
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

 '(imenu-list-entry-face-0          ((t (:inherit font-lock-function-name-face ))))
 '(imenu-list-entry-subalist-face-0 ((t (:inherit imenu-list-entry-face-0 :weight bold))))
 '(imenu-list-entry-face-1          ((t (:foreground "#62b49f"))))
 '(imenu-list-entry-subalist-face-1 ((t (:inherit imenu-list-entry-face-1 :weight bold))))
 '(imenu-list-entry-face-2          ((t (:foreground "#c1eae2"))))
 '(imenu-list-entry-subalist-face-2 ((t (:inherit imenu-list-entry-face-2 :weight bold))))
 '(imenu-list-entry-face-3          ((t (:foreground "#b2d1cb"))))
 '(imenu-list-entry-subalist-face-3 ((t (:inherit imenu-list-entry-face-3 :weight bold))))

 '(elfeed-search-title-face        ((t (:inherit shadow :slant italic :background "#3a4040"))))
 '(elfeed-search-unread-title-face ((t (:inherit default :slant normal :foreground "#bcc6ce"))))
 '(elfeed-search-feed-face         ((t (:foreground "#d09f07"))))
 '(elfeed-search-tag-face          ((t (:foreground "#8ba004"))))

 '(erc-timestamp-face ((t (:inherit font-lock-string-face :weight bold))))
 '(erc-prompt-face    ((t (:inherit font-lock-keyword-face :weight bold))))
 '(erc-action-face    ((t (:inherit font-lock-string-face))))

 '(message-header-name    ((t (:foreground "#8ba004"))))
 '(message-header-subject ((t (:inherit font-lock-function-name-face :weight bold))))
 '(message-header-to      ((t (:inherit font-lock-function-name-face))))
 '(message-header-other   ((t (:foreground "#d09f07"))))

 '(shr-link ((t (:foreground "#47bcb3" :underline t))))

 '(highlight-indent-guides-character-face ((t (:foreground "#606060"))))

 '(company-preview                      ((t (:inherit default :background "#304540"))))
 '(company-preview-common               ((t (:inherit company-preview :slant italic))))
 '(company-tooltip                      ((t (:foreground "#bed3d3" :background "#252525"))))
 '(company-tooltip-annotation           ((t (:foreground "#2da49b"))))
 '(company-tooltip-annotation-selection ((t (:foreground "#2da49b"))))
 '(company-tooltip-common               ((t (:foreground "#bed3d3"))))
 '(company-tooltip-common-selection     ((t (:weight bold))))
 '(company-tooltip-mouse                ((t (:background "#2da49b" :foreground "#304540"))))
 '(company-tooltip-search               ((t (:foreground "#d7afd7"))))
 '(company-tooltip-search-selection     ((t (:foreground "#d7afd7" :weight bold))))
 '(company-tooltip-selection            ((t (:background "#353535" :weight bold))))
 '(company-scrollbar-bg                 ((t (:background "#252525" :foreground "#2da49b"))))
 '(company-scrollbar-fg                 ((t (:foreground "#1b1b1b" :background "#2da49b"))))

 '(term-color-black   ((t (:foreground "#808080"))))
 '(term-color-red     ((t (:foreground "#cb5a37"))))
 '(term-color-green   ((t (:foreground "#8ba004"))))
 '(term-color-yellow  ((t (:foreground "#d09f07"))))
 '(term-color-blue    ((t (:foreground "#499ec4"))))
 '(term-color-magenta ((t (:foreground "#d7afd7"))))
 '(term-color-cyan    ((t (:foreground "#2da49b"))))
 '(term-color-white   ((t (:foreground "#bed3d3"))))
 )

(custom-theme-set-variables
 'functor
 '(ibuffer-deletion-face 'dired-flagged)
 '(ibuffer-filter-group-name-face 'dired-mark)
 '(ibuffer-marked-face 'dired-marked)
 '(ibuffer-title-face 'dired-header)
 '(ansi-color-names-vector
   ["#808080" "#cb5a37" "#8ba004" "#d09f07" "#499ec4" "#d7afd7" "#47bcb3" "#bed3d3"]))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'functor)

;;; functor-theme.el ends here
