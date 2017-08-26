(defmacro my/add-hooks (hooks &rest body)
  `(dolist (hook ,hooks)
     (add-hook hook (lambda () ,@body))))

(defmacro my/execute-f-with-hook (f winf)
  `(lambda (&rest args)
     (interactive)
     (,winf)
     (apply (quote ,f) args)))

(defmacro my/control-function-window-split (f height width)
  `(lambda (&rest args)
     (interactive)
     (let ((split-height-threshold ,height)
           (split-width-threshold ,width))
       (apply (quote ,f) args))))

;; what it says
(defun my/revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
          (message "Refreshed open files"))

;; read file as string
(defun my/read-file-contents (path)
  (with-temp-buffer
    (insert-file-contents (expand-file-name path))
    (buffer-string)))

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

(provide 'my-utils)

