(require 'org)
(require 'org-element)
(require 'ox)
(require 'ox-html)
(require 'ox-rss)

(defvar website-html-head)
(defvar website-html-preamble)
(defvar website-html-postamble)
(defvar website-html-postamble-disqus)

(setq website-html-head
  "<meta name='viewport' content='width=device-width, initial-scale=1.0'>
<link rel='stylesheet' href='/static/css/bootstrap.min.css'/>
<link rel='stylesheet' href='/static/css/bootstrap-theme.min.css'/>
<link rel='stylesheet' href='/static/css/code.css'/>
<link rel='stylesheet' href='/static/css/site.css'/>
<script src='https://use.fontawesome.com/a04fa9be28.js'></script>
<link href='https://fonts.googleapis.com/css?family=Inconsolata' rel='stylesheet'/>")

(setq website-html-preamble
  "<nav class='navbar navbar-default'>
    <div class='container container-fluid'>
      <div class='navbar-header'>
        <a class='navbar-brand' href='/'>alexpeits</a>
      </div>
      <ul class='nav navbar-nav'>
        <li><a href='/'>Home</a></li>
        <li><a href='/blog'>Blog</a></li>
        <li><a href='/presentations.html'>Presentations</a></li>
        <li><a href='/about.html'>About</a></li>
      </ul>
      <ul class='nav navbar-nav navbar-right'>
        <li><a href='https://github.com/alexpeits'><i class='fa fa-github fa-lg' aria-hidden='true'></i></a></li>
        <li><a href='https://twitter.com/alexpeits'><i class='fa fa-twitter fa-lg' aria-hidden='true'></i></a></li>
        <li><a href='https://www.linkedin.com/in/alexandros-peitsinis-591624114/'><i class='fa fa-linkedin fa-lg' aria-hidden='true'></i></a></li>
        <li><a href='/blog/index.xml'><i class='fa fa-rss fa-lg' aria-hidden='true'></i></a></li>
      </ul>
    </div>
  </nav>
")

(setq website-html-postamble
  "
<footer class='footer'>
    <div class='container'>
        <div class='row vcenter'>
                <p>blog created with <a href='http://orgmode.org'>org-mode</a></p>
        </div>
    </div>
</footer>
<script src='/static/js/jquery-3.1.1.min.js'></script>
<script src='/static/js/bootstrap.min.js'></script>
<script>
    document.title = 'alexpeits - ' + document.title;
    var content = document.getElementById('content');
    content.className += ' container container-fluid';
</script>
")

(setq website-html-postamble-disqus
      (concat website-html-postamble
              "
<div id='disqus_thread'></div>
<script>

var disqus_config = function () {
  var url = window.location.href;
  this.page.url = url;
  this.page.identifier = url.substr(url.lastIndexOf('/') + 1);
};

(function() {
  var d = document, s = d.createElement('script');
  s.src = 'https://alexpeits-github-io.disqus.com/embed.js';
  s.setAttribute('data-timestamp', +new Date());
  (d.head || d.body).appendChild(s);
})();
</script>
<noscript>Please enable JavaScript to view the <a href='https://disqus.com/?ref_noscript'>comments powered by Disqus.</a></noscript>"))


(setq org-publish-project-alist
      `(("org"
         :base-directory "~/Documents/blog/"
         :base-extension "org"
         :publishing-directory "~/projects/alexpeits.github.io"
         :publishing-function my/org-html-publish
         :section-numbers nil
         :with-toc nil
         :html-head ,website-html-head
         :html-preamble ,website-html-preamble
         :html-postamble ,website-html-postamble
         )
        ("blog"
         :base-directory "~/Documents/blog/blog/"
         :base-extension "org"
         :publishing-directory "~/projects/alexpeits.github.io/blog"
         :publishing-function my/org-html-publish
         :section-numbers nil
         :with-toc nil
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Blog"
         :sitemap-sort-files anti-chronologically
         :sitemap-file-entry-format "%d | %t"
         :sitemap-date-format "%Y.%m.%d"
         :sitemap-function my-blog-sitemap
         :html-head ,website-html-head
         :html-preamble ,website-html-preamble
         :html-postamble ,website-html-postamble
         :html-inline-images t
         )
        ("css"
         :base-directory "~/Documents/blog/static/css/"
         :base-extension "css"
         :publishing-directory "~/projects/alexpeits.github.io/static/css/"
         :publishing-function org-publish-attachment)
        ("js"
         :base-directory "~/Documents/blog/static/js/"
         :base-extension "js"
         :publishing-directory "~/projects/alexpeits.github.io/static/js/"
         :publishing-function org-publish-attachment)
        ("img"
         :base-directory "~/Documents/blog/static/img/"
         :base-extension "jpg\\|gif\\|png\\|svg"
         :publishing-directory "~/projects/alexpeits.github.io/static/img/"
         :publishing-function org-publish-attachment)
        ("rss"
         :base-directory "~/Documents/blog/blog"
         :base-extension "org"
         :publishing-directory "~/projects/alexpeits.github.io/blog"
         :publishing-function (org-rss-publish-to-rss)
         :section-numbers nil
         :table-of-contents nil
         :exclude ".*"
         :include ("index.org")
         :rss-extension "xml"
         :rss-image-url "http://assets-cdn.github.com/favicon.ico"
         :html-link-home "http://alexpeits.github.io/blog/"
         :html-link-use-abs-url t)
        ("website"
         :components ("org" "blog" "css" "js" "img" "rss"))))


;; Path for pygments or command name
(defvar pygments-path "pygmentize")

(defun pygments-org-html-code (code contents info)
  ;; Generating tmp file path.
  ;; Current date and time hash will ideally pass our needs.
  (let ((tmp "/tmp/pygmentblock"))
    (with-temp-file tmp (insert (org-element-property :value code)))
    (shell-command-to-string (format "%s -l \"%s\" -f html %s && rm %s"
                                     pygments-path
                                     (or (org-element-property :language code)
                                         "text")
                                     tmp
                                     tmp))
    ))

(defun org-html-abs-file-link (link desc info)
  (let ((lnk (org-html-link link desc info)))
    (if (string-match "file://" lnk)
        (let ((abs-link (replace-regexp-in-string "file://" "" lnk)))
          (if (string-prefix-p "/" abs-link)
              abs-link
            (concat "/" abs-link)))
      lnk)))

(org-export-define-derived-backend 'pygments-html 'html
  :translate-alist '((src-block .  pygments-org-html-code)
		     (example-block . pygments-org-html-code)
                     (link . org-html-abs-file-link)))

(defun my/org-html-publish (plist filename pub-dir)
  (org-publish-org-to 'pygments-html filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension
                                      "html"))
                      plist pub-dir))
(defun my-blog-get-preview (file)
  "The comments in FILE have to be on their own lines, prefereably before and after paragraphs."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((beg (+ 1 (re-search-forward "^#\\+BEGIN_PREVIEW$")))
          (end (progn (re-search-forward "^#\\+END_PREVIEW$")
                      (match-beginning 0))))
      (buffer-substring beg end))))

(defun my-blog-sitemap (project &optional sitemap-filename)
  "Generate the sitemap for my blog."
  (let* ((project-plist (cdr project))
         (dir (file-name-as-directory
               (plist-get project-plist :base-directory)))
         (localdir (file-name-directory dir))
         (exclude-regexp (plist-get project-plist :exclude))
         (files (nreverse
                 (org-publish-get-base-files project exclude-regexp)))
         (sitemap-filename (concat dir (or sitemap-filename "sitemap.org")))
         (sitemap-sans-extension
          (plist-get project-plist :sitemap-sans-extension))
         (visiting (find-buffer-visiting sitemap-filename))
         file sitemap-buffer)
    (with-current-buffer
        (let ((org-inhibit-startup t))
          (setq sitemap-buffer
                (or visiting (find-file sitemap-filename))))
      (erase-buffer)
      ;; loop through all of the files in the project
      (while (setq file (pop files))
        (let ((fn (file-name-nondirectory file))
              (link ;; changed this to fix links. see postprocessor.
               (file-relative-name file))
               ;; (file-relative-name file (file-name-as-directory
                                         ;; (expand-file-name (concat (file-name-as-directory dir) "..")))))
              (oldlocal localdir))
          (when sitemap-sans-extension
            (setq link (file-name-sans-extension link)))
          ;; sitemap shouldn't list itself
          (unless (equal (file-truename sitemap-filename)
                         (file-truename file))
            (let (;; get the title and date of the current file
                  (title (org-publish-format-file-entry "%t" file project-plist))
                  (date (org-publish-format-file-entry "%d" file project-plist))
                  ;; get the preview section from the current file
                  (preview (my-blog-get-preview file))
                  (regexp "\\(.*\\)\\[\\([^][]+\\)\\]\\(.*\\)"))
              ;; insert a horizontal line before every post, kill the first one
              ;; before saving
              (insert "-----\n")
              (cond ((string-match-p regexp title)
                     (string-match regexp title)
                     ;; insert every post as headline
                     (insert (concat"* " (match-string 1 title)
                                    "[[file:" link "]["
                                    (match-string 2 title)
                                    "]]" (match-string 3 title) "\n")))
                    (t (insert (concat "* [[file:" link "][" title "]]\n"))))
              ;; add properties for `ox-rss.el' here
              (let ((rss-permalink (concat (file-name-sans-extension link) ".html"))
                    (rss-pubdate (format-time-string
                                  (car org-time-stamp-formats)
                                  (org-publish-find-date file))))
                (org-set-property "RSS_PERMALINK" rss-permalink)
                (org-set-property "PUBDATE" rss-pubdate))
              ;; insert the date, preview, & read more link
              (insert (concat date "\n\n"))
              (insert preview)
              (insert (concat "[[file:" link "][Read More...]]\n"))))))
      ;; kill the first hrule to make this look OK
      (goto-char (point-min))
      (let ((kill-whole-line t)) (kill-line))
      (goto-char (point-min))
      (insert "#+TITLE: alexpeits blog\n")
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))


;; (setq website-html-postamble
  ;; "
;; <footer class='footer'>
    ;; <div class='container'>
        ;; <div class='row vcenter'>
            ;; <div class='pull-left col-lg-4 footer-left'>
                ;; <p>blog created with <a href='http://orgmode.org'>org-mode</a></p>
            ;; </div>
            ;; <div class='pull-right col-lg-offset-4 footer-right'>
                ;; <p>
                ;; <a href='http://github.com/alexpeits'>github</a><span> | </span>
                ;; <a href='http://twitter.com/alexpeits'>twitter</a><span> | </span>
                ;; <a href='https://www.linkedin.com/in/alexandros-peitsinis-591624114/'>linkedin</a>
                ;; </p>
            ;; </div>
        ;; </div>
    ;; </div>
;; </footer>
;; <script src='/static/js/jquery-3.1.1.min.js'></script>
;; <script src='/static/js/bootstrap.min.js'></script>
;; <script>
    ;; document.title = 'alexpeits - ' + document.title;
    ;; var content = document.getElementById('content');
    ;; content.className += ' container container-fluid';
;; </script>")

(provide 'my-org-blog)
