;;;;;;;
;; UI

(defun my/smartparens-pair-newline (id action context)
  (save-excursion
    (newline)
    (indent-according-to-mode)))

(defun my/smartparens-pair-newline-and-indent (id action context)
  (my/smartparens-pair-newline id action context)
  (indent-according-to-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various

(defun my/shell-command-to-string ()
  (interactive)
  (let ((cmd (read-string "Command: ")))
  (insert (shell-command-to-string cmd)))
  )

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
;; major modes

(defun my/org-insert-template ()
  (interactive)
  (let* ((templ-dir (expand-file-name "~/.emacs.d/org-templates/"))
         (ls (directory-files templ-dir nil "^[^.]"))
         (file (completing-read "Template: " ls))
         (path (concat templ-dir file)))
    (insert-file-contents path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; error checking

(defun my/toggle-flycheck-error-list ()
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))

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

;; helm-projectile-persp-switch-project!
(defun my/switch-project ()
  (interactive)
  (persp-switch (let ((temp-charset "1234567890abcdefghijklmnopqrstuvwxyz")
                      (random-string ""))
                  (dotimes (i 6 random-string)
                    (setq random-string
                          (concat
                           random-string
                           (char-to-string (elt temp-charset (random (length temp-charset)))))
                          ))
                  ))
  (helm-projectile-switch-project)
  (persp-rename (projectile-project-name)))

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

;;;;;;;;;;
;; helpers

(defun my/round-time (time mult)
  "Return a hh:mm formatted time duration as a decimal (string)
denoting hours,rounding it to the nearest multiple of `mult'.

For example, the duration 4:47, with rounding to a quarter of
an hour (mult=25), gives 4,75 hours, and 4:55 gives 5.00"
  (let* ((hm (mapcar 'string-to-int (split-string time ":")))
         (h (car hm))
         (m (cadr hm))
         (perc (round (* 100 (/ m 60.0))))
         (rperc (* mult (/ (+ perc (/ mult 2)) mult))))
    (format "%d.%02d"
            (if (and (= 0 (mod rperc 100))
                     (> rperc perc))
                (1+ h)
              h)
            (mod rperc 100))))

(provide 'myfuncs)
