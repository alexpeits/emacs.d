;;; lucius-theme.el --- a dark theme for Emacs 24
;;;
;;; Author: Jason Milkins <jasonm23@gmail.com>
;;; Url: https://github.com/jasonm23/emacs-lucius-theme 
;;; Version: 20130715.621
;;;
;;; Changelog :
;;;
;;; 20130715.621: Inital version
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

(deftheme lucius
  "lucius theme - a dark theme for Emacs 24")

(custom-theme-set-faces
 'lucius

 '(cursor                              ((t (                      :background "#99d1ce"                                                          ))))
 '(default                             ((t (:foreground "#CFE0DA" :background "#1a1a1a"                                                          ))))
 '(linum                               ((t (:foreground "#53676b" :background "#09150F"                                                          ))))
 '(minibuffer-prompt                   ((t (:foreground "#12A89F"                          :weight bold                                          ))))
 '(escape-glyph                        ((t (:foreground "orange"                                                                                 ))))
 '(highlight                           ((t (                      :background "#283438"                                                          ))))
 '(hl-line                             ((t (                      :background "#252a2a"                                                          ))))
 '(region                              ((t (                      :background "#264842"                                                          ))))
 '(fringe                              ((t (                      :background "#171f20"                                                          ))))
 '(shadow                              ((t (:foreground "#777777"                                                                                ))))
 '(secondary-selection                 ((t (                      :background "#132125"                                                          ))))
 '(trailing-whitespace                 ((t (                      :background "#C74000"                                                          ))))

 '(show-paren-match ((t (:background "#456665" :weight bold))))

 `(font-lock-comment-face              ((t (:foreground "#808080" :background nil :slant italic                                                  ))))
 `(font-lock-comment-delimiter-face    ((t (                                                                   :inherit font-lock-comment-face   ))))
 `(font-lock-constant-face             ((t (:foreground "#87D7FF" :background nil                                                                ))))
 `(font-lock-builtin-face              ((t (:foreground "#87D7FF" :background nil                                                                ))))
 `(font-lock-function-name-face        ((t (:foreground "#AFD787" :background nil                                                                ))))
 ;; `(font-lock-variable-name-face        ((t (:foreground "#64adbc" :background nil                                                                ))))
 `(font-lock-variable-name-face        ((t (:foreground "#87D7FF" :background nil                                                                ))))
 `(font-lock-keyword-face              ((t (:foreground "#2aa198" :background nil                                                                ))))
 `(font-lock-string-face               ((t (:foreground "#D7D7AF" :background nil                                                                ))))
 ;; `(font-lock-string-face               ((t (:foreground "#ffc63f" :background nil                                                                ))))
 `(font-lock-doc-string-face           ((t (:foreground "#1e8ece" :background nil                                                                ))))
 `(font-lock-doc-face                  ((t (:foreground "#62B49F" :background nil                                                                ))))
 `(font-lock-type-face                 ((t (:foreground "#87D7D7" :background nil                                                                ))))
 ;; `(haskell-type-face                   ((t (:foreground "#55bdcc" :background nil                                                                ))))
 `(haskell-pragma-face            ((t (:foreground "#859900" :slant italic))))
 `(purescript-pragma-face            ((t (:foreground "#859900" :slant italic))))

 '(font-lock-negation-char-face        ((t (:foreground "#C75311" :background nil                                                                ))))
 '(font-lock-comment-delimiter-face    ((t (:foreground "#3499aa" :background nil                              :inherit (font-lock-comment-face) ))))
 '(font-lock-preprocessor-face         ((t (:foreground "#ff5050" :background nil                              :inherit (font-lock-builtin-face) ))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#f66500" :background nil                              :inherit (bold)                   ))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "red"     :background nil                              :inherit (bold)                   ))))
 '(font-lock-doc-face                  ((t (:foreground "#90A0A0" :background nil                              :inherit (font-lock-string-face)  ))))
 '(font-lock-warning-face              ((t (:foreground "salmon"  :background nil                              :inherit (error)                  ))))

 '(link                                ((t (:foreground "#00b7f0" :background nil       :underline t                                             ))))
 '(link-visited                        ((t (:foreground "magenta4"                      :underline t           :inherit (link)                   ))))
 '(button                              ((t (:foreground "#FFFFFF" :background "#333333" :underline t           :inherit (link)                   ))))
 '(vertical-border                     ((t (:foreground "#758A86"                                                                                ))))
 '(next-error                          ((t (                                                                   :inherit (region)                 ))))
 '(query-replace                       ((t (                                                                   :inherit (isearch)                ))))
 '(header-line                         ((t (:foreground "#222222" :background "#bbbbbb" :box nil               :inherit (mode-line)              ))))
 '(mode-line-highlight                 ((t (                                            :box nil                                                 ))))
 '(mode-line-emphasis                  ((t (                                                     :weight bold                                    ))))
 '(mode-line-buffer-id                 ((t (                                                     :weight bold                                    ))))
 '(mode-line ((t (:foreground "#C1EAE2" :background "#005555" :box (:line-width 1 :color "#005555")))))
 '(mode-line-inactive ((t (:foreground "#387A71" :background "#1e2827" :box (:line-width 1 :color "#004040")))))
 '(persp-selected-face                 ((t (:inherit font-lock-function-name-face :weight bold))))
 '(isearch-fail                        ((t (:foreground "#3f4758" :background "salmon"                                                          ))))
 '(isearch                             ((t (:inverse-video nil :foreground "black" :background "#db7e4c"))))
 '(lazy-highlight                      ((t (:foreground "#a0a8b0" :background "#3d464f"))))
 '(match                               ((t (                      :background "#3388cc"                                                          ))))
 '(tooltip                             ((t (:foreground "black"   :background "LightYellow"                    :inherit (variable-pitch)         ))))
 '(js3-function-param-face             ((t (:foreground "#AFD3C9"                                                                                ))))
 '(js3-external-variable-face          ((t (:foreground "#A0B0B0"                                                                                ))))
 '(cua-rectangle                       ((t (:foreground "white"   :background "#DD6600"                                                          ))))

 '(markdown-code-face ((t (:inherit font-lock-keyword-face))))
 '(markdown-pre-face ((t (:inherit font-lock-keyword-face))))

 '(org-block ((t (:foreground "#b8cfd3" :background "#12181a"))))
 '(org-block-begin-line ((t (:background "#192224"))))
 '(org-block-end-line ((t (:background "#192224"))))

 `(diff-hl-insert ((t (:background "#143514" :foreground "#4c934c"))))
 `(diff-hl-change ((t (:background "#122544" :foreground "#466daf"))))
 `(diff-hl-delete ((t (:background "#491111" :foreground "#bc4d4d"))))

 '(outline-1 ((t (:foreground "#AFD787"))))
 '(outline-2 ((t (:foreground "#2aa198"))))
 '(outline-3 ((t (:foreground "#62B49F"))))
 '(outline-4 ((t (:foreground "#87D7FF"))))
 '(outline-5 ((t (:foreground "#AFD787"))))
 '(outline-6 ((t (:foreground "#62B49F"))))
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'lucius)

;;; lucius-theme.el ends here
