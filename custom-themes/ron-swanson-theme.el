;;; ron-swanson-theme --- Give me all the bacon and eggs you have.

;; Copyright (C) 2018 Alex Peitsinis

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
;;
;; I can do what I want.
;; Ron.
;;
;;; Code:

(deftheme ron-swanson
  "Never half-ass two things. Whole-ass one thing.")

(custom-theme-set-faces
 'ron-swanson
 '(default ((t (:background "#202020" :foreground "#e5e5e5"))))
 '(org-block ((t (:background "#242424" :foreground "#d8d8d8"))))
 '(org-block-begin-line ((t (:background "#2a2a2a"))))
 '(org-block-end-line ((t (:background "#2a2a2a"))))
 '(fixed-pitch ((t nil)))
 '(font-lock-comment-face ((t (:foreground "chocolate1"))))
 '(show-paren-match ((t (:background "RoyalBlue3"))))
 '(region ((t (:background "#253b76"))))
 '(fringe ((t (:background "#2b2b2b"))))
 '(hl-line ((t (:background "#333333"))))
 '(my/fci-rule ((t (:background "#404040"))))
 '(persp-selected-face ((t (:foreground "RoyalBlue" :weight bold))))
 '(whitespace-trailing ((t (:background "#602020"))))
 '(flycheck-warning ((t (:underline (:color "orange1" :style wave)))))
 '(flycheck-fringe-warning ((t (:foreground "orange1"))))
 '(flycheck-error ((t (:underline (:color "red1" :style wave)))))
 '(flycheck-fringe-error ((t (:foreground "red1"))))
 '(flycheck-info ((t (:underline (:color "DeepSkyBlue2" :style wave)))))
 '(flycheck-fringe-info ((t (:foreground "DeepSkyBlue2")))))

(provide-theme 'ron-swanson)
;;; ron-swanson-theme ends here
