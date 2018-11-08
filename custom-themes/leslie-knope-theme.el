;;; leslie-knope-theme --- TODO.

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
;; TODO
;;
;;; Code:

(deftheme leslie-knope
  "I'm big enough to admit that I am often inspired by myself.")

(custom-theme-set-faces
 'leslie-knope
 '(default ((t (:background "White" :foreground "Black"))))
 '(org-block ((t (:background "#fcfcfc" :foreground "#101010"))))
 '(org-block-begin-line ((t (:background "#eeeeee"))))
 '(org-block-end-line ((t (:background "#eeeeee"))))
 '(fixed-pitch ((t nil)))
 '(hl-line ((t (:background "#f5f5f5"))))
 '(whitespace-trailing ((t (:background "#602020"))))
 '(flycheck-warning ((t (:underline (:color "orange1" :style wave)))))
 '(flycheck-fringe-warning ((t (:foreground "orange1"))))
 '(flycheck-error ((t (:underline (:color "red1" :style wave)))))
 '(flycheck-fringe-error ((t (:foreground "red1"))))
 '(flycheck-info ((t (:underline (:color "DeepSkyBlue2" :style wave)))))
 '(flycheck-fringe-info ((t (:foreground "DeepSkyBlue2")))))

(provide-theme 'leslie-knope)
;;; leslie-knope-theme ends here
