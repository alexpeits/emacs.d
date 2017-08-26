;;;;;;;
;; UI

(defun my/smartparens-pair-newline (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various

(defun face-p (face-symbol)
  "XE ad Emacs compatibility, checks if the FACE-SYMBOL exists."
  (cond
   ((fboundp 'face-list)  ;Emacs
    (memq face-symbol (funcall (symbol-function 'face-list))))
   ((fboundp 'facep)      ;Xemacs
    (funcall (symbol-function 'facep) face-symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm

(defun my/helm-find-files (arg)
  "Custom spacemacs implementation for calling helm-find-files-1.
Removes the automatic guessing of the initial value based on thing at point. "
  (interactive "P")
  (let* ((hist (and arg helm-ff-history (helm-find-files-history)))
         (default-input hist)
         (input (cond ((and (eq major-mode 'dired-mode) default-input)
                       (file-name-directory default-input))
                      ((and (not (string= default-input ""))
                            default-input))
                      (t (expand-file-name (helm-current-directory))))))
    (set-text-properties 0 (length input) nil input)
    (helm-find-files-1 input)))

(defun my/helm-file-switch-other-window-horizontally ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (candidate)
       (let ((split-height-threshold 0)
             (split-width-threshold nil))
         (helm-find-files-other-window candidate))))))

(defun my/helm-file-switch-other-window-vertically ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (candidate)
       (let ((split-height-threshold nil)
             (split-width-threshold 0))
         (helm-find-files-other-window candidate))))))

(defun my/helm-buffer-switch-other-window-horizontally ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (candidate)
       (let ((split-height-threshold 0)
             (split-width-threshold nil))
         (helm-switch-to-buffers-other-window candidate))))))

(defun my/helm-buffer-switch-buffer-other-window-vertically ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (candidate)
       (let ((split-height-threshold nil)
             (split-width-threshold 0))
         (helm-switch-to-buffers-other-window candidate))))))

(provide 'myfuncs)
