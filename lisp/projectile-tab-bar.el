;;; projectile-tab-bar.el --- projectile integration with tab-bar-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Alex Peitsinis

;; Author: Alex Peitsinis <alexpeitsinis@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27") (projectile "0.11.0"))
;; Keywords: project, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(defun projectile-tab-bar-all-tab-names ()
  (mapcar (lambda (tab) (alist-get 'name tab))
          (tab-bar-tabs)))

(defun projectile-tab-bar-all-tab-names-recent ()
  (mapcar (lambda (tab) (alist-get 'name tab))
          (tab-bar--tabs-recent)))

(defun projectile-tab-bar-tab-exists-p (name)
  (not (null (member name (projectile-tab-bar-all-tab-names)))))

(defun projectile-tab-bar-current-tab ()
  (alist-get 'name (tab-bar--current-tab)))

(defun projectile-tab-bar-scratch-buffer-name (name)
  (format "*scratch* (%s)" name))

(defun projectile-tab-bar-create-scratch-buffer (name)
  (let ((bufname (projectile-tab-bar-scratch-buffer-name name)))
    (switch-to-buffer bufname)
    (funcall initial-major-mode)
    (when initial-scratch-message
      (insert initial-scratch-message))))

(defun projectile-tab-bar-switch-to-tab (name)
  (interactive
   (list
    (completing-read "Tab name: " (projectile-tab-bar-all-tab-names-recent))))
  (tab-bar-select-tab-by-name name))

(defun projectile-tab-bar-switch-to-next-tab ()
  (interactive)
  (tab-bar-switch-to-next-tab))

(defun projectile-tab-bar-switch-to-prev-tab ()
  (interactive)
  (tab-bar-switch-to-prev-tab))

(defun projectile-tab-bar-kill-scratch-buffer (name)
  (kill-buffer (projectile-tab-bar-scratch-buffer-name name)))

(defun projectile-tab-bar--return-t (orig &rest args)
  t)

(defun projectile-tab-bar--disable-y-or-n-p (orig &rest args)
  (advice-add 'yes-or-no-p :around #'projectile-tab-bar--return-t)
  (advice-add 'y-or-n-p :around #'projectile-tab-bar--return-t)
  (let ((res (apply orig args)))
    (advice-remove 'yes-or-no-p #'projectile-tab-bar--return-t)
    (advice-remove 'y-or-n-p #'projectile-tab-bar--return-t)
    res))

(defun projectile-tab-bar-kill-projectile-buffers ()
  (advice-add 'projectile-kill-buffers :around #'projectile-tab-bar--disable-y-or-n-p)
  (projectile-kill-buffers)
  (advice-remove 'projectile-kill-buffers #'projectile-tab-bar--disable-y-or-n-p))

(defun projectile-tab-bar-close-tab (name)
  (interactive
   (list
    (completing-read
     "Tab name: "
     (projectile-tab-bar-all-tab-names)
     nil nil nil nil
     (projectile-tab-bar-current-tab))))
  (ignore-errors (projectile-tab-bar-kill-scratch-buffer name))
  (projectile-tab-bar-kill-projectile-buffers)
  (tab-bar-close-tab-by-name name))

(defun projectile-tab-bar-rename-tab (name)
  (interactive "sNew name: ")
  (tab-bar-rename-tab name))

(defun projectile-tab-bar-create-tab (name)
  "Switch to tab with name NAME, or create one if it does not exist."
  (let ((tab-bar-new-tab-to 'rightmost))
    (tab-bar-new-tab)
    (tab-bar-rename-tab name)
    (projectile-tab-bar-create-scratch-buffer name)))

(defun projectile-tab-bar-switch-or-create-tab (name)
  "Switch to tab with name NAME, or create one if it does not exist."
  (interactive
   (list (completing-read "Tab name: " (projectile-tab-bar-all-tab-names))))
  (let ((tab-bar-new-tab-to 'rightmost))
    (if (not (projectile-tab-bar-tab-exists-p name))
        (projectile-tab-bar-create-tab name)
      (projectile-tab-bar-switch-to-tab name))))

;;;###autoload
(defun projectile-tab-bar-switch-project (project-to-switch)
  (interactive
   (list
    (projectile-completing-read "Switch to project: "
                                (projectile-relevant-known-projects))))
  (let* ((name (or projectile-project-name
                   (funcall projectile-project-name-function project-to-switch)))
         (tab-exists (projectile-tab-bar-tab-exists-p name))
         (current-tab (projectile-tab-bar-current-tab)))
    (cond
     ;; project-specific tab already exists
     ((and tab-exists (not (equal name current-tab)))
      (projectile-tab-bar-switch-to-tab name))
     ;; project-specific tab doesn't exist
     ((not tab-exists)
      (projectile-tab-bar-create-tab name)
      (projectile-switch-project-by-name project-to-switch)))))

(defface projectile-tab-bar-modeline-active-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for highlighting active projectile tab in modeline"
  :group 'projectile-tab-bar)

(defun projectile-tab-bar-modeline ()
  (let* ((all-tabs (projectile-tab-bar-all-tab-names))
         (cur-tab (projectile-tab-bar-current-tab))
         (propertized-tabs
          (mapcar (lambda (tab)
                    (if (string-equal tab cur-tab)
                        (propertize tab 'face 'projectile-tab-bar-modeline-active-face)
                      tab))
                  all-tabs)))
    (format "[%s]" (mapconcat 'identity propertized-tabs "|"))))

(provide 'projectile-tab-bar)
;;; projectile-tab-bar.el ends here
