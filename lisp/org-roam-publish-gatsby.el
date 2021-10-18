(when noninteractive
  (require 'package)

  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))

  (package-initialize)

  (defun og/package-install (package)
    (unless (package-installed-p package)
      (package-install package)))

  (og/package-install 'org-roam)
  (og/package-install 's)
  (og/package-install 'dash))

(setq org-roam-v2-ack t)

(require 'org)
(require 'ol)
(require 'ox-publish)
(require 'org-roam)
(require 'ox-md)
(require 's)
(require 'dash)

(defvar og/org-directory
  (expand-file-name "~/Dropbox/emacs/org/"))
(defvar og/publish-base-directory
  (expand-file-name (or (getenv "GATSBY_NOTES_DIR") "~/tmp/gatsby-tutorial/")))
(defvar og/publish-content-directory
  (expand-file-name "content/notes" og/publish-base-directory))
(defvar og/publish-static-directory
  (expand-file-name "static/" og/publish-base-directory))

;; (setq org-roam-directory og/org-directory)
;; (setq make-backup-files nil)

(defun org-kindle-follow (path _)
  "TODO"
  nil)

(defun org-kindle-export (link description format _)
  (let ((desc (or description link)))
      (pcase format
        (`md (format "[%s](%s)" desc link))
        (t (user-error "org-kindle-export: Not implemented yet")))))

(defun org-kindle-store ()
  "TODO"
  nil)

(org-link-set-parameters "kindle"
                         :follow #'org-kindle-follow
                         :export #'org-kindle-export
                         :store #'org-kindle-store)

(org-export-define-derived-backend 'gatsby 'md
  :filters-alist '((:filter-parse-tree . og/separate-elements))
  :menu-entry
  '(?g "Gatsby: export to Markdown with YAML front matter."
       ((?G "As markdown buffer" (lambda (a s v b) (og/export-as-md a s v)))
        (?g "As markdown file" (lambda (a s v b) (og/export-to-md a s v)))))
  :translate-alist
  '((headline . og/headline-offset)
    (inner-template . og/inner-template)
    (footnote-reference . og/footnote-reference)
    (src-block . og/src-block)
    (latex-environment . og/latex-environment)
    (latex-fragment . og/latex-fragment)
    (link . og/link)
    (underline . og/underline)
    (table . og/table)
    (table-cell . og/table-cell)
    (table-row . og/table-row)
    ;; (timestamp . og/timestamp)
    (dlist . og/timestamp)
    (template . og/template))
  :options-alist
  `((:roam-tags "ROAM_TAGS" nil "")))

(defun og/file-is-image? (raw-link)
  "Test whether RAW-LINK is a link to an image file."
  (let* ((image-suffixes '(".apng" ".bmp" ".gif" ".ico" ".cur" ".jpg" ".jpeg" ".jfif"
                           ".pjpeg" ".pjp" ".png" ".svg" ".webp"))
         (suffix-matches (--filter (s-ends-with? it raw-link t) image-suffixes)))
    (not (null suffix-matches))))

(defun og/link-maybe-image (desc path)
  "If PATH is to a known image type, render the link as an image (with a bang).
Otherwise, render the image as a usual link. DESC PATH"
  (if (og/file-is-image? path)
      ;; it's an image, so prepend a bang
      (format "![%s](%s)" desc path)
    ;; non-image file, so no bang
    (format "[%s](%s)" desc path)))

(defun og/timestamp (&rest a) "TODO")

(defun og/link (link desc info)
  "Transcode a LINK.

DESC is the links description.
INFO is a plist used as a communication channel.

This function will render images appropriately"
  (let* ((raw-link (org-element-property :raw-link link))
         (path (org-element-property :path link))
         (type (org-element-property :type link)))
    (if (string= type "id")
        (let* ((org-id-prop (org-id-find path))
               (relpath (if (null org-id-prop)
                            (error "Cannot find entry with ID '%s'" orgid)
                          (car org-id-prop)))
               (abspath (expand-file-name relpath))
               (filename (file-relative-name abspath og/org-directory)))
          ;; org-roam file, format as markdown [[wiki-link]]
          (format
           "[[%s]]"
           (file-name-sans-extension filename)))
      (let* ((resolved-path
              (cond
               ;; if the path is to a file, make it treat the root directory as system
               ((string= type "file")
                (format "/%s" (string-remove-prefix "/" path)))
               ;; if not a file, then just use the raw link itself.
               (t raw-link)))
             (desc (if desc desc resolved-path)))
        (og/link-maybe-image desc resolved-path)))))

(defun og/underline (underline contents info)
  "Transcode UNDERLINE from Org to Markdown.
CONTENTS is the text with underline markup.  INFO is a plist
holding contextual information."
  (format "<u>%s</u>" contents))

;; TODO
(defun og/footnote-reference (footnote-reference _contents info)
  "Transcode a FOOTNOTE-REFERENCE element into Markdown format.
CONTENTS is nil.  INFO is a plist holding contextual information.

Adapted from ox-blackfriday, via ox-hugo."
  (concat
   ;; Insert separator between two footnotes in a row.
   (let ((prev (org-export-get-previous-element footnote-reference info)))
     (and (eq (org-element-type prev) 'footnote-reference)
          (plist-get info :html-footnote-separator)))
   (format "[^fn%d]" (org-export-get-footnote-number footnote-reference info))))

(defun og/footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel.

Adapted from ox-blackfriday, via ox-hugo."
  (let ((fn-alist (org-export-collect-footnote-definitions info))
        fn-alist-stripped)
    (let ((n 1)
          def)
      (dolist (fn fn-alist)
        (setq def (org-trim (org-export-data (nth 2 fn) info)))
        ;; Support multi-line footnote definitions by folding all
        ;; footnote definition lines into a single line as Blackfriday
        ;; does not support that.
        (setq def (replace-regexp-in-string "\n" " " def))
        ;; Replace multiple consecutive spaces with a single space.
        (setq def (replace-regexp-in-string "[[:blank:]]+" " " def))
        (push (cons n def) fn-alist-stripped)
        (setq n (1+ n))))
    (when fn-alist-stripped
      (mapconcat (lambda (fn)
                   ;; (message "dbg: fn: %0d -- %s" (car fn) (cdr fn))
                   (format "[^fn%d]: %s"
                           (car fn)     ;footnote number
                           (cdr fn)))   ;footnote definition
                 (nreverse fn-alist-stripped)
                 "\n"))))

(defun og/latex-environment (latex-environment _contents info)
  "Transcode a LATEX-ENVIRONMENT object into Markdown format.
INFO is a plist holding contextual information.

Adapted from ox-blackfriday via ox-hugo."
  (let ((processing-type (plist-get info :with-latex)))
    (cond
     ((memq processing-type '(t mathjax))
      (let* ((latex-env (org-remove-indentation
                         (org-element-property :value latex-environment)))
             (env (org-html-format-latex latex-env 'mathjax info))
             (env (og/escape-chars-in-equation env)))
        env))
     (t
      (org-html-latex-environment latex-environment nil info)))))

(defun og/latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object into Markdown format.
INFO is a plist holding contextual information.

Adapted from ox-blackfriday via ox-hugo."
  (let ((processing-type (plist-get info :with-latex)))
    (cond
     ((memq processing-type '(t mathjax))
      (let* ((latex-frag (org-element-property :value latex-fragment))
             (frag (org-html-format-latex latex-frag 'mathjax info))
             (frag (og/escape-chars-in-equation frag)))
        ;; (message "[ox-bf-latex-frag DBG] frag: %s" frag)
        frag))
     (t
      (org-html-latex-fragment latex-fragment nil info)))))

(defun og/escape-chars-in-equation (str)
  "Escape few characters in STR so that Markdown doesn't parse them.

Adapted from ox-blackfriday via ox-hugo.

Do not interpret underscores and asterisks in equations as
Markdown formatting
characters (https://gohugo.io/content-management/formats#solution):
  \"_\" -> \"\\=\\_\"
  \"*\" -> \"\\=\\*\"
https://github.com/kaushalmodi/ox-hugo/issues/104
Blackfriday converts \"(r)\" to Registered Trademark symbol,
\"(c)\" to Copyright symbol, and \"(tm)\" to Trademark symbol if
the SmartyPants extension is enabled (and there is no way to
disable just this).  So insert an extra space after the opening
parentheses in those strings to trick Blackfriday/smartParens
from activating inside equations.  That extra space anyways
doesn't matter in equations.
  \"(c)\" -> \"( c)\"
  \"(r)\" -> \"( r)\"
  \"(tm)\" -> \"( tm)\"
https://gohugo.io/content-management/formats#solution
https://github.com/kaushalmodi/ox-hugo/issues/138
Need to escape the backslash in \"\\(\", \"\\)\", .. to make
Blackfriday happy.  So:
  \"\\(\" -> \"\\\\(\"
  \"\\)\" -> \"\\\\)\"
  \"\\\\=[\" -> \"\\\\\\=[\"
  \"\\\\=]\" -> \"\\\\\\=]\"
  \"\\\\={\" -> \"\\\\\\={\"
  \"\\\\=}\" -> \"\\\\\\=}\"
  \"\\|\" -> \"\\\\|\"
and finally:
  \"\\\\\" -> \"\\\\\\\\\\\\\"."
  (let* (;; _ -> \_, * -> \*
         (escaped-str (replace-regexp-in-string "[_*]" "\\\\\\&" str))
         ;; (c) -> ( c), (r) -> ( r), (tm) -> ( tm)
         (escaped-str (replace-regexp-in-string "(\\(c\\|r\\|tm\\))" "( \\1)" escaped-str))
         ;; \( -> \\(, \) -> \\), \[ -> \\[, \] -> \\], \{ -> \\{, \} -> \\}, \| -> \\|
         (escaped-str (replace-regexp-in-string "\\(\\\\[](){}[|]\\)" "\\\\\\1" escaped-str))
         (escaped-str (replace-regexp-in-string
                       "\\([^\\]\\)\\\\\\{2\\}[[:blank:]]*$" ;Replace "\\" at EOL with:
                       "\\1\\\\\\\\\\\\\\\\\\\\\\\\"             ;"\\\\\\"
                       escaped-str)))
    escaped-str))

(defun og/headline-offset (headline contents info)
  "Proper headline offset"
  (let* ((info (plist-put info :headline-offset 0)))
    (org-md-headline headline contents info)))

(defun og/separate-elements (tree _backend info)
  "Fix blank lines between elements.

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Enforce a blank line between elements.  There are three exceptions
to this rule:

  1. Preserve blank lines between sibling items in a plain list.

  2. In an item, remove any blank line before the very first
     paragraph and the next sub-list when the latter ends the
     current item.

  3. Do not insert blank lines between table rows.

Assume BACKEND is `gatsby'."
  (org-element-map tree (remq 'item org-element-all-elements)
    (lambda (e)
      (org-element-put-property
       e :post-blank
       (if (and (eq (org-element-type e) 'paragraph)
		(eq (org-element-type (org-element-property :parent e)) 'item)
		(org-export-first-sibling-p e info)
		(let ((next (org-export-get-next-element e info)))
		  (and (eq (org-element-type next) 'plain-list)
		       (not (org-export-get-next-element next info)))))
	   0
	 (if (eq (org-element-type e) 'table-row)
             0
           1)))))
  ;; Return updated tree.
  tree)

(defun og/clean-language (language)
  "Clean up the language tag from a source block.

At the moment, only translate 'ipython' to 'python'."
  (if (string= language "ipython") "python" language))

(defun og/src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Markdown format. CONTENTS is
nil.  INFO is a plist used as a communication channel.

Adapted from ox-gfm."
  (let* ((lang (og/clean-language
                (org-element-property :language src-block)))
         (code (org-export-format-code-default src-block info))
         (prefix (concat "```" lang "\n"))
         (suffix "```"))
    (concat prefix code suffix)))

(defun og/table (table contents info)
  "Empty transformation. Org tables should be valid kramdown syntax."
  contents)

(defun og/table-cell (table-cell contents info)
  "Empty transformation. Org tables should be valid kramdown syntax."
  (if contents
      (format "| %s " contents)
    "| "))

(defun og/table-row (table-row contents info)
  "Empty transformation. Org tables should be valid kramdown syntax."
  (if contents
      (format "%s|" contents)
    (let* ((table (org-export-get-parent table-row))
           (rc (org-export-table-dimensions table info)))
      (concat (apply 'concat (make-list (cdr rc) "|---"))
              (identity "|")))))

(defun og/template (contents info)
  "Return complete document string after MD conversion.

CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (concat (og/yaml-front-matter info) contents))

(defun og/inner-template (contents info)
  "Return body of document string after MD conversion.

CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-md--build-toc info (and (wholenump depth) depth))))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (og/footnote-section info)))

(defun og/get-option (info property-name)
  "Get org export options, or an (optional) user-provided
default, or (if user does not provide a default) an empty
string."
  (org-export-data (plist-get info property-name) info))

(defun og/nonempty (s)
  "Check if S is not `nil' or an empty string."
  (not (or (null s) (string-empty-p s))))

(defun og/yaml-front-matter (info)
  "Creat YAML frontmatter content."
  (let ((convert-to-yaml-list
         (lambda (arg)
           (mapconcat #'(lambda (text) (concat "\n- " text))
                      (split-string arg) " "))))
    (let* ((title (og/get-option info :title))
           (excerpt (og/get-option info :subtitle))
           (tags nil)
           (date
            (let ((input-file
                   (file-name-nondirectory
                    (og/get-option info :input-file))))
              (when (string-match
                     "^\\([[:digit:]]\\{4\\}\\)\\([[:digit:]]\\{2\\}\\)\\([[:digit:]]\\{2\\}\\)"
                     input-file
                     )
                (format "%s-%s-%s"
                        (match-string 1 input-file)
                        (match-string 2 input-file)
                        (match-string 3 input-file))))
            ))
      (concat
       "---\n"
       (when (og/nonempty title) (format "title: '%s'\n" title))
       (when (og/nonempty excerpt) (format "excerpt: %s\n" excerpt))
       (when (og/nonempty date) (format "date: %s\n" date))
       (when tags "tags:\n")
       "---\n"))))

(defun og/export-as-md (&optional async subtreep visible-only)
  "Export current buffer as a Markdown buffer adding some YAML front matter."
  (interactive)
  (org-export-to-buffer 'gatsby "*Org Gatsby-Markdown Export*"
    async subtreep visible-only nil nil (lambda () (markdown-mode))))

;;;###autoload
(defun og/export-to-md (&optional async subtreep visible-only)
  "Export current buffer to a Markdown file adding some YAML front matter."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'gatsby outfile async subtreep visible-only)))

;;;###autoload
(defun og/publish-to-md (plist filename pub-dir)
  "Publish an org file to Markdown with YAML front matter.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'gatsby filename ".md" plist pub-dir))


(defun og/get-publish-alist (&optional all)
  (let ((to-exclude
         (if all
             "mobile_capture.org\\|index.org\\|theindex.org\\|all_files_sorted_by_date.org"
           ".*"))
        (to-include
         (if all
             nil
           `[,(file-name-nondirectory (buffer-file-name))]
           )))
    `(
      ("gatsby-files"
       :base-directory ,og/org-directory
       :base-extension "org"
       :publishing-directory ,og/publish-content-directory
       :recursive t
       :publishing-function og/publish-to-md
       :headline-levels 5
       :section-numbers nil
       :exclude ,to-exclude
       :include ,to-include
       )
      ("gatsby-static"
       :base-directory ,og/org-directory
       :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|ico"
       :publishing-directory ,og/publish-static-directory
       :recursive t
       :publishing-function org-publish-attachment
       )
      ("gatsby-all" :components ("gatsby-files" "gatsby-static"))
      )))

(defun og/publish (all)
  (interactive "P")
  (let ((org-publish-project-alist (og/get-publish-alist all)))
    (org-publish "gatsby-all" :force)))

(defun og/get-publish-alist (&rest files)
  (let* ((everything (null files))
         (to-exclude
          (if everything
              "mobile_capture.org\\|index.org\\|theindex.org\\|all_files_sorted_by_date.org"
            ".*"))
         (to-include
          (if everything
              nil
            (apply 'vector files))))
    (setq foo to-include)
    `(
      ("gatsby-files"
       :base-directory ,og/org-directory
       :base-extension "org"
       :publishing-directory ,og/publish-content-directory
       :recursive t
       :publishing-function og/publish-to-md
       :headline-levels 5
       :section-numbers nil
       :exclude ,to-exclude
       :with-toc nil
       :include ,to-include
       )
      ("gatsby-static"
       :base-directory ,og/org-directory
       :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|ico\\|html"
       :publishing-directory ,og/publish-static-directory
       :recursive t
       :publishing-function org-publish-attachment
       )
      ("gatsby-all" :components ("gatsby-files" "gatsby-static"))
      )))

(defun og/publish-files (&rest files)
  (let ((org-publish-project-alist (apply 'og/get-publish-alist files))
        (org-roam-directory og/org-directory)
        (make-backup-files nil)
        (create-lockfiles nil))
    (org-publish "gatsby-all" :force)))

(defun og/publish-all (force)
  (interactive "P")
  (let ((org-publish-project-alist (og/get-publish-alist))
        (org-roam-directory og/org-directory)
        (make-backup-files nil)
        (create-lockfiles nil))
    (org-publish "gatsby-all" force)))

(defun og/publish-current-file (force)
  (interactive "P")
  (let ((org-publish-project-alist (og/get-publish-alist (buffer-file-name)))
        (org-roam-directory og/org-directory)
        (make-backup-files nil)
        (create-lockfiles nil))
    (org-publish "gatsby-all" force)))

(provide 'org-roam-publish-gatsby)
