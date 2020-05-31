(require 'org)
(require 'org-roam)

(defvar my/org-roam-publish-directory "~/org-roam-publish")

(setq
 org-publish-project-alist
 `(
   ("roam-files"
    :base-directory ,my/org-directory
    :base-extension "org"
    :publishing-directory ,my/org-roam-publish-directory
    :recursive t
    :publishing-function org-html-publish-to-html
    :headline-levels 4
    :auto-preamble t
    :makeindex t
    :with-toc 1
    :section-numbers 1
    :html-postamble my/org-roam-html-postamble
    :html-head-extra ,(string-join
                       '("<link href=\"/static/css/htmlize.css\" rel=\"stylesheet\">"
                         "<link href=\"/static/css/readtheorg.css\" rel=\"stylesheet\">"
                         "<link href=\"/static/css/custom.css\" rel=\"stylesheet\">"
                         "<script src=\"/static/js/jquery.min.js\"></script>"
                         "<script src=\"/static/js/bootstrap.min.js\"></script>"
                         "<script type=\"text/javascript\" src=\"/static/js/jquery.stickytableheaders.min.js\"></script>"
                         "<script type=\"text/javascript\" src=\"/static/js/readtheorg.js\"></script>"
                         "<script type=\"text/javascript\" src=\"/static/js/custom.js\"></script>"
                         "<link rel=\"icon\" href=\"/static/favicon.ico\">"
                         ) "\n")
    )
   ("roam-static"
    :base-directory ,my/org-directory
    :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|ico"
    :publishing-directory ,my/org-roam-publish-directory
    :recursive t
    :publishing-function org-publish-attachment
    )
   ("roam-all" :components ("roam-files" "roam-static"))
   )
 )

(defun my/org-roam-publish (force)
  (interactive "P")
  (condition-case ex
      (let ((org-html-htmlize-output-type 'css)
            (hooks '(my/org-roam-export-add-backlinks
                     my/org-roam-export-add-index))
            (old-file-link-export-param (org-link-get-parameter "file" :export))
            (old-cite-link-export-param (org-link-get-parameter "cite" :export)))
        (unwind-protect
            (progn
              (dolist (hook hooks)
                (add-hook 'org-export-before-processing-hook hook))
              (org-link-set-parameters "file" :export #'my/org-roam-file-link-export)
              (org-link-set-parameters "cite" :export #'my/org-roam-cite-link-export)
              (when force (org-publish-remove-all-timestamps))
              (org-publish "roam-all" force)
              (org-roam-graph--build nil
                                     #'(lambda (path)
                                         (copy-file path
                                                    (expand-file-name "graph.html" my/org-roam-publish-directory)
                                                    :overwrite)))
              (when (executable-find "notify-send")
                (shell-command "notify-send --urgency=low emacs 'org-roam-publish finished'")))
          (progn
            (dolist (hook hooks)
              (remove-hook 'org-export-before-processing-hook hook))
            (org-link-set-parameters "file" :export old-file-link-export-param)
            (org-link-set-parameters "cite" :export old-cite-link-export-param))))
    ('error (shell-command (format "notify-send --urgency=critical 'org-roam-publish failed' '%s'" ex)))))

(defun my/org-roam-file-link-export (path desc backend)
  (if (org-roam--org-roam-file-p path)
      (let* ((fn (file-relative-name path my/org-directory))
             (html-fn (format "%s.html" (file-name-sans-extension fn))))
        (format "<a href=\"/%s\" class=\"org-roam-file-link\">%s</a>" html-fn desc))
    nil))

(defun my/org-roam-cite-link-export (ref desc backend)
  (condition-case ex
      (let* ((completions (org-roam--get-ref-path-completions))
             (path (plist-get (cdr (assoc ref completions)) :path))
             (fn (file-relative-name path my/org-directory))
             (html-fn (format "%s.html" (file-name-sans-extension fn))))
        (format "<a href=\"/%s\" class=\"org-roam-cite-link\">%s</a>" html-fn ref))
    ('error (org-ref-ref-export ref desc backend))))

(defun my/org-roam-html-postamble (opts)
  (let* ((fn (buffer-file-name))
         (edit-link (format "org-protocol://roam-file?file=%s" fn))
         (graph-server-link (format "http://localhost:%d" org-roam-server-port))
         (graph-link "/graph.html")
         (home-link "/")
         (index-link "/theindex.html"))
    (string-join `("<div id=\"postamble\" class=\"status\">"
                   ,(format "<p class=\"postamble-row\"><a id=\"postamble-edit\" href=\"%s\">Edit file</a></p>" edit-link)
                   ,(format "<p class=\"postamble-row\"><a id=\"postamble-graph-server\" href=\"%s\" target=\"_blank\">Graph server</a></p>" graph-server-link)
                   ,(format "<p class=\"postamble-row\"><a id=\"postamble-graph\" href=\"%s\" target=\"_blank\">Graph</a></p>" graph-link)
                   ,(format "<p class=\"postamble-row\"><a id=\"postamble-home\" href=\"%s\">Back to home</a></p>" home-link)
                   ,(format "<p class=\"postamble-row\"><a id=\"postamble-index\" href=\"%s\">Back to index</a></p>" index-link)
                   "</div>"
                   )
                 "\n")))

(defun my/org-roam-export-add-index (backend)
  (let ((titles (org-roam--extract-titles)))
    (save-excursion
      (dolist (title titles)
        (goto-char (point-min))
        (newline)
        (goto-char (point-min))
        (insert (format "#+INDEX: %s" title))))))

(defun my/org-roam-export-add-backlinks (backend)
  (let* ((fn (buffer-file-name))
         ;; TODO (ref (cdr (org-roam--extract-ref)))
         (links (with-temp-buffer
                  (if-let* ((backlinks (org-roam--get-backlinks fn))
                            (grouped-backlinks (--group-by (nth 0 it) backlinks)))
                      (progn
                        (insert (format "\n\n* Backlinks (%d)\n" (length backlinks)))
                        (insert ":PROPERTIES:\n")
                        (insert ":UNNUMBERED: t\n")
                        (insert ":CUSTOM_ID: org-roam-backlinks\n")
                        (insert ":END:\n")
                        (dolist (group grouped-backlinks)
                          (let ((file-from (car group))
                                (bls (cdr group)))
                            (insert (format "** [[file:%s][%s]]\n"
                                            file-from
                                            (org-roam--get-title-or-slug file-from)))
                            (dolist (backlink bls)
                              (pcase-let ((`(,file-from _ ,props) backlink))
                                (insert (s-trim (s-replace "\n" " " (plist-get props :content))))
                                (insert "\n\n")))))))
                  (buffer-string))))
    (unless (string= links "")
      (save-excursion
        (goto-char (point-max))
        (insert "-----\n")
        (insert links)))))

(provide 'my-org-roam-publish)
;;; my-org-roam-publish.el ends here
