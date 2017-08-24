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
;; Jump to definition

(defvar my-default-jump-handlers '()
  "List of jump handlers available in every mode.")

(defvar-local my-jump-handlers '()
  "List of jump handlers local to this buffer.")

(defmacro my|define-jump-handlers (mode &rest handlers)
  "Defines jump handlers for the given MODE.
This defines a variable `my-jump-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `my-jump-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "my//init-jump-handlers-%S" mode)))
        (handlers-list (intern (format "my-jump-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific jump handlers for %S. "
                          "These take priority over those in "
                          "`my-default-jump-handlers'.")
                  mode))
       (defun ,func ()
         (setq my-jump-handlers
               (append ,handlers-list
                       my-default-jump-handlers)))
       (add-hook ',mode-hook ',func)
       )))

(defun my/jump-to-definition ()
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler my-jump-handlers)
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or async
                    (not (eq old-point (point)))
                    (not (equal old-buffer (current-buffer))))
            (throw 'done t)))))
    (message "No jump handler was able to find this symbol.")))

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
