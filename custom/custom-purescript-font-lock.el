;;; purescript-font-lock.el --- Font locking module for Purescript Mode -*- lexical-binding: t -*-

;; Copyright 2003, 2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc.
;; Copyright 1997-1998  Graeme E Moss, and Tommy Thorn

;; Author: 1997-1998 Graeme E Moss <gem@cs.york.ac.uk>
;;         1997-1998 Tommy Thorn <thorn@irisa.fr>
;;         2003      Dave Love <fx@gnu.org>
;; Keywords: faces files Purescript

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'etags)
(require 'ring)
(require 'outline)
(require 'xref nil t)
(require 'cl-lib)
(require 'rx)
(require 'font-lock)

(eval-when-compile
  (setq byte-compile-warnings '(not cl-functions obsolete)))


;;; SECTION
;;; purescript-compat

;; Cross-referencing commands have been replaced since Emacs 25.1.
;; These aliases are required to provide backward compatibility.
(unless (fboundp 'xref-push-marker-stack)
  (defalias 'xref-pop-marker-stack 'pop-tag-mark)

  (defun xref-push-marker-stack (&optional m)
    "Add point M (defaults to `point-marker') to the marker stack."
    (ring-insert find-tag-marker-ring (or m (point-marker)))))

(unless (fboundp 'outline-hide-sublevels)
  (defalias 'outline-hide-sublevels 'hide-sublevels))

(unless (fboundp 'outline-show-subtree)
  (defalias 'outline-show-subtree 'show-subtree))

(unless (fboundp 'outline-hide-sublevels)
  (defalias 'outline-hide-sublevels 'hide-sublevels))

(unless (fboundp 'outline-show-subtree)
  (defalias 'outline-show-subtree 'show-subtree))

(unless (fboundp 'xref-find-definitions)
  (defun xref-find-definitions (ident)
    (let ((next-p (and (boundp 'xref-prompt-for-identifier)
                       xref-prompt-for-identifier)))
      (find-tag ident next-p))))

(unless (fboundp 'font-lock-ensure)
  (defalias 'font-lock-ensure 'font-lock-fontify-buffer))

;;; SECTION
;;; purescript-lexeme

(unless (category-docstring ?P)
  (define-category ?P "Purescript symbol constituent characters")
  (map-char-table
   #'(lambda (key val)
       (if (or
            (and (consp key) (> (car key) 128))
            (and (numberp key) (> key 128)))
           (if (member val '(Pc Pd Po Sm Sc Sk So))
               (modify-category-entry key ?P))))
   unicode-category-table)

  (dolist (key (string-to-list "!#$%&*+./<=>?@^|~\\-:"))
    (modify-category-entry key ?P)))

(defconst purescript-lexeme-modid
  "[[:upper:]][[:alnum:]'_]*"
  "Regexp matching a valid Purescript module identifier.

Note that GHC accepts Unicode category UppercaseLetter as a first
character. Following letters are from Unicode categories
UppercaseLetter, LowercaseLetter, OtherLetter, TitlecaseLetter,
ModifierLetter, DecimalNumber, OtherNumber, backslash or
underscore.")

(defconst purescript-lexeme-id
  "[[:alpha:]_][[:alnum:]'_]*"
  "Regexp matching a valid Purescript identifier.

GHC accepts a string starting with any alphabetic character or
underscore followed by any alphanumeric character or underscore
or apostrophe.")

(defconst purescript-lexeme-sym
  "\\cP+"
  "Regexp matching a valid Purescript variable or constructor symbol.

GHC accepts a string of chars from the set
[:!#$%&*+./<=>?@^|~\\-] or Unicode category Symbol for chars with
codes larger than 128 only.")

(defconst purescript-lexeme-idsym-first-char
  "\\(?:[[:alpha:]_]\\|\\cP\\)"
  "Regexp matching first character of a qualified or unqualified
identifier or symbol.

Useful for `re-search-forward'.")

(defconst purescript-lexeme-modid-opt-prefix
  (concat "\\(?:" purescript-lexeme-modid "\\.\\)*")
  "Regexp matching a valid Purescript module prefix, potentially empty.

Module path prefix is separated by dots and finishes with a
dot. For path component syntax see `purescript-lexeme-modid'.")

(defconst purescript-lexeme-qid-or-qsym
  (rx-to-string `(: (regexp ,purescript-lexeme-modid-opt-prefix)
                    (group (| (regexp ,purescript-lexeme-id) (regexp ,purescript-lexeme-sym)
                              ))))
  "Regexp matching a valid qualified identifier or symbol.

Note that (match-string 1) returns the unqualified part.")

(defun purescript-lexeme-looking-at-qidsym ()
  "Non-nil when point is just in front of an optionally qualified
identifier or symbol.

Using this function is more efficient than matching against the
regexp `purescript-lexeme-qid-or-qsym'.

Returns:
  'qid - if matched a qualified id: 'Data.Map' or 'Map'
  'qsym - if matched a qualified id: 'Monad.>>=' or '>>='
  'qprefix - if matched only modid prefix: 'Data.'

After successful 'qid or 'qsym match (match-string 1) will return
the unqualified part (if any)."
  (let ((begin (point))
        (match-data-old (match-data)))
    (save-excursion
      (while (looking-at (concat purescript-lexeme-modid "\\."))
        (goto-char (match-end 0)))
      (cond
       ((looking-at purescript-lexeme-id)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))

          ;; check is MagicHash is present at the end of the token
          (goto-char end)
          (when (looking-at "#+")
            (setq end (match-end 0)))

          (set-match-data
           (list begin end
                 beg end)))
        'qid)
       ((looking-at purescript-lexeme-sym)
        (set-match-data
          (list begin (match-end 0)
                (match-beginning 0) (match-end 0)))
        'qsym)
       ((equal begin (point))
        (set-match-data match-data-old)
        nil)
       (t
        (set-match-data
         (list begin (point)
               nil nil))
        'qprefix)))))

(defun purescript-lexeme-looking-at-backtick ()
  "Non-nil when point is just in front of an identifier quoted with backticks.

When match is successful, match-data will contain:
  (match-text 1) - opening backtick
  (match-text 2) - whole qualified identifier
  (match-text 3) - unqualified part of identifier
  (match-text 4) - closing backtick"
  (let ((match-data-old (match-data))
        first-backtick-start
        last-backtick-start
        qid-start
        id-start
        id-end
        result)
    (save-excursion
      (when (looking-at "`")
        (setq first-backtick-start (match-beginning 0))
        (goto-char (match-end 0))
        (forward-comment (buffer-size))
        (when (purescript-lexeme-looking-at-qidsym)
          (setq qid-start (match-beginning 0))
          (setq id-start (match-beginning 1))
          (setq id-end (match-end 1))
          (goto-char (match-end 0))
          (forward-comment (buffer-size))
          (when (looking-at "`")
            (setq last-backtick-start (match-beginning 0))
            (set-match-data
             (mapcar
              (lambda (p)
                (set-marker (make-marker) p))
              (list
               first-backtick-start (1+ last-backtick-start)
               first-backtick-start (1+ first-backtick-start)
               qid-start id-end
               id-start id-end
               last-backtick-start (1+ last-backtick-start))))
            (setq result t)))))
    (unless result
      (set-match-data match-data-old))
    result))

(defconst purescript-lexeme-qid
  (rx-to-string `(: (regexp "'*")
                    (regexp ,purescript-lexeme-modid-opt-prefix)
                    (group (regexp ,purescript-lexeme-id))))
  "Regexp matching a valid qualified identifier.

Note that (match-string 1) returns the unqualified part.")

(defconst purescript-lexeme-qsym
  (rx-to-string `(: (regexp "'*")
                    (regexp ,purescript-lexeme-modid-opt-prefix)
                    (group (regexp ,purescript-lexeme-id))))
  "Regexp matching a valid qualified symbol.

Note that (match-string 1) returns the unqualified part.")

(defconst purescript-lexeme-number
  (rx (| (: (regexp "[0-9]+\\.[0-9]+") (opt (regexp "[eE][-+]?[0-9]+")))
         (regexp "[0-9]+[eE][-+]?[0-9]+")
         (regexp "0[xX][0-9a-fA-F]+")
         (regexp "0[oO][0-7]+")
         (regexp "[0-9]+")))
  "Regexp matching a floating point, decimal, octal or hexadecimal number.

Note that negative sign char is not part of a number.")

(defconst purescript-lexeme-char-literal-inside
  (rx (| (not (any "\n'\\"))
         (: "\\"
            (| "a" "b" "f" "n" "r" "t" "v" "\\" "\"" "'"
               "NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK"
               "BEL" "BS" "HT" "LF" "VT" "FF" "CR" "SO" "SI" "DLE"
               "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN"
               "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "SP" "DEL"
               (regexp "[0-9]+")
               (: "x" (regexp "[0-9a-fA-F]+"))
               (: "o" (regexp "[0-7]+"))
               (: "^" (regexp "[]A-Z@^_\\[]"))))))
  "Regexp matching an inside of a character literal.

Note that `purescript-lexeme-char-literal-inside' matches strictly
only escape sequences defined in Purescript Report.")

(defconst purescript-lexeme--char-literal-rx
  (rx-to-string `(: (group "'")
                    (| (: (group (regexp "[[:alpha:]_([]")) (group "'")) ; exactly one char
                       (: (group (| (regexp "\\\\[^\n][^'\n]*") ; allow quote just after first backslash
                                    (regexp "[^[:alpha:]_:(['\n][^'\n]*")))
                          (| (group "'") "\n" (regexp "\\'"))))))
  "Regexp matching a character literal lookalike.

Note that `purescript-lexeme--char-literal-rx' matches more than
Purescript Report specifies because we want to support also code
under edit.

Character literals end with a quote or a newline or end of
buffer.

Regexp has subgroup expressions:
 (match-text 1) matches the opening quote.
 (match-text 2) matches the inside of the character literal.
 (match-text 3) matches the closing quote or an empty string
                at the end of line or the end buffer.")

(defun purescript-lexeme-looking-at-char-literal ()
  "Non-nil when point is at a char literal lookalike.

Note that this function matches more than Purescript Report
specifies because we want to support also code under edit.

Char literals end with a quote or an unescaped newline or end
of buffer.

After successful match:
 (match-text 1) matches the opening quote.
 (match-text 2) matches the inside of the char literla.
 (match-text 3) matches the closing quote, or a closing
                newline or is nil when at the end of the buffer."
  (when (looking-at purescript-lexeme--char-literal-rx)
    (set-match-data
     (list (match-beginning 0) (match-end 0)
           (match-beginning 1) (match-end 1)
           (or (match-beginning 2) (match-beginning 4)) (or (match-end 2) (match-end 4))
           (or (match-beginning 3) (match-beginning 5)) (or (match-end 3) (match-end 5))))
    t))

(defconst purescript-lexeme-string-literal-inside-item
  (rx (| (not (any "\n\"\\"))
         (: "\\"
            (| "a" "b" "f" "n" "r" "t" "v" "\\" "\"" "'" "&"
               "NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK"
               "BEL" "BS" "HT" "LF" "VT" "FF" "CR" "SO" "SI" "DLE"
               "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN"
               "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "SP" "DEL"
               (regexp "[0-9]+")
               (: "x" (regexp "[0-9a-fA-F]+"))
               (: "o" (regexp "[0-7]+"))
               (: "^" (regexp "[]A-Z@^_\\[]"))
               (regexp "[ \t\n\r\v\f]*\\\\")))))
  "Regexp matching an item that is a single character or a single
escape sequence inside of a string literal.

Note that `purescript-lexeme-string-literal-inside-item' matches
strictly only escape sequences defined in Purescript Report.")

(defconst purescript-lexeme-string-literal
  (rx (: (group "\"")
         (group (* (| (regexp "\\\\[ \t\n\r\v\f]*\\\\")
                      (regexp "\\\\[ \t\n\r\v\f]+")
                      (regexp "\\\\[^ \t\n\r\v\f]")
                      (* (regexp "[^\"\n\\]")))))
         (group (| "\"" (regexp "$") (regexp "\\\\?\\'")
                   ))))
  "Regexp matching a string literal lookalike.

Note that `purescript-lexeme-string-literal' matches more than
Purescript Report specifies because we want to support also code
under edit.

String literals end with double quote or unescaped newline or end
of buffer.

Regexp has subgroup expressions:
 (match-text 1) matches the opening double quote.
 (match-text 2) matches the inside of the string.
 (match-text 3) matches the closing double quote or an empty string
                at the end of line or the end buffer.")

(defun purescript-lexeme-looking-at-string-literal ()
  "Non-nil when point is at a string literal lookalike.

Note that this function matches more than Purescript Report
specifies because we want to support also code under edit.

String literals end with double quote or unescaped newline or end
of buffer.

After successful match:
 (match-text 1) matches the opening doublequote.
 (match-text 2) matches the inside of the string.
 (match-text 3) matches the closing quote, or a closing
                newline or is nil when at the end of the buffer."
  (when (looking-at "\"")
    (save-excursion
      (let ((begin (point)))
        (goto-char (match-end 0))
        (let (finish)
          (while (and (not finish)
                      (re-search-forward "[\"\n\\]" nil 'goto-eob))
            (cond
             ((equal (match-string 0) "\\")
              (if (looking-at "[ \t\n\r\v\f]+\\\\?")
                  (goto-char (match-end 0))
                (goto-char (1+ (point)))))

             ((equal (match-string 0) "\"")
              (set-match-data
               (list begin (match-end 0)
                     begin (1+ begin)
                     (1+ begin) (match-beginning 0)
                     (match-beginning 0) (match-end 0)))
              (setq finish t))

             ((equal (match-string 0) "\n")
              (set-match-data
               (list begin (match-beginning 0)
                     begin (1+ begin)
                     (1+ begin) (match-beginning 0)
                     nil nil))
              (setq finish t))))
          (unless finish
            ;; string closed by end of buffer
            (set-match-data
             (list begin (point)
                   begin (1+ begin)
                   (1+ begin) (point)
                   nil nil))))))
    ;; there was a match
    t))

(defun purescript-lexeme-looking-at-quasi-quote-literal ()
  "Non-nil when point is just in front of Template Purescript
quaisquote literal.

Quasi quotes start with '[xxx|' or '[$xxx|' sequence and end with
  '|]'. The 'xxx' is a quoter name. There is no escaping mechanism
provided for the ending sequence.

Regexp has subgroup expressions:
 (match-text 1) matches the quoter name (without $ sign if present).
 (match-text 2) matches the opening vertical bar.
 (match-text 3) matches the inside of the quoted string.
 (match-text 4) matches the closing vertical bar
                or nil if at the end of the buffer.

Note that this function excludes 'e', 't', 'd', 'p' as quoter
names according to Template Purescript specification."
  (let ((match-data-old (match-data)))
    (if (and
         (looking-at (rx-to-string `(: "[" (optional "$")
                                       (group (regexp ,purescript-lexeme-id))
                                       (group "|"))))
         (equal (purescript-lexeme-classify-by-first-char (char-after (match-beginning 1)))
                'varid)
         (not (member (match-string 1) '("e" "t" "d" "p"))))
      (save-excursion
        ;; note that quasi quote syntax does not have any escaping
        ;; mechanism and if not closed it will span til lthe end of buffer
        (goto-char (match-end 0))
        (let ((match-data (match-data))
              (match-data-2 (and (re-search-forward "|]" nil t)
                                 (match-data))))
          (if match-data-2
              (set-match-data
               (list
                (nth 0 match-data) (nth 1 match-data-2)          ;; whole match
                (nth 2 match-data) (nth 3 match-data)            ;; quoter name
                (nth 4 match-data) (nth 5 match-data)            ;; opening bar
                (nth 5 match-data) (nth 0 match-data-2)          ;; inner string
                (nth 0 match-data-2) (1+ (nth 0 match-data-2)))) ;; closing bar

            (set-match-data
             (list
              (nth 0 match-data) (point-max)                   ;; whole match
              (nth 2 match-data) (nth 3 match-data)            ;; quoter name
              (nth 4 match-data) (nth 5 match-data)            ;; opening bar
              (nth 5 match-data) (point-max)                   ;; inner string
              nil nil))                                        ;; closing bar
            ))
        t)
      ;; restore old match data if not matched
      (set-match-data match-data-old)
      nil)))

(defun purescript-lexeme-classify-by-first-char (char)
  "Classify token by CHAR.

CHAR is a chararacter that is assumed to be the first character
of a token."
  (let ((category (get-char-code-property (or char ?\ ) 'general-category)))

    (cond
     ((or (member char '(?! ?# ?$ ?% ?& ?* ?+ ?. ?/ ?< ?= ?> ?? ?@ ?^ ?| ?~ ?\\ ?-))
          (and (> char 127)
               (member category '(Pc Pd Po Sm Sc Sk So))))
      'varsym)
     ((equal char ?:)
      'consym)
     ((equal char ?\')
      'char)
     ((equal char ?\")
      'string)
     ((member category '(Lu Lt))
      'conid)
     ((or (equal char ?_)
          (member category '(Ll Lo)))
      'varid)
     ((and (>= char ?0) (<= char ?9))
      'number)
     ((member char '(?\] ?\[ ?\( ?\) ?\{ ?\} ?\` ?\, ?\;))
      'special))))

(defun purescript-lexeme-looking-at-token (&rest flags)
  "Like `looking-at' but understands Purescript lexemes.

Moves point forward over whitespace.  Returns a symbol describing
type of Purescript token recognized.  Use `match-string',
`match-beginning' and `match-end' with argument 0 to query match
result.

Possible results are:
- 'special: for chars [](){}`,;
- 'comment: for single line comments
- 'nested-comment: for multiline comments
- 'qsymid: for qualified identifiers or symbols
- 'string: for strings literals
- 'char: for char literals
- 'number: for decimal, float, hexadecimal and octal number literals
- 'template-purescript-quote: for a string of apostrophes for template purescript
- 'template-purescript-quasi-quote: for a string of apostrophes for template purescript

Note that for qualified symbols (match-string 1) returns the
unqualified identifier or symbol.  Further qualification for
symbol or identifier can be done with:

   (purescript-lexeme-classify-by-first-char (char-after (match-beginning 1)))

See `purescript-lexeme-classify-by-first-char' for details."
  (while
      ;; Due to how unterminated strings terminate at newline, some
      ;; newlines have syntax set to generic string delimeter. We want
      ;; those to be treated as whitespace anyway
      (or
       (> (skip-syntax-forward "-") 0)
       (and (not (member 'newline flags))
            (> (skip-chars-forward "\n") 0))))
  (let
      ((case-fold-search nil)
       (point (point-marker)))
    (or
     (and
      (equal (string-to-syntax "<")
             (get-char-property (point) 'syntax-table))
      (progn
        (set-match-data (list point (set-marker (make-marker) (line-end-position))))
        'literate-comment))
     (and (looking-at "\n")
          'newline)
     (and (looking-at "{-")
          (save-excursion
            (forward-comment 1)
            (set-match-data (list point (point-marker)))
            'nested-comment))
     (and (purescript-lexeme-looking-at-char-literal)
          'char)
     (and (purescript-lexeme-looking-at-string-literal)
          'string)
     (and (looking-at "[][(){}`,;]")
          (if (purescript-lexeme-looking-at-quasi-quote-literal)
              'template-purescript-quasi-quote
            'special))
     (and (purescript-lexeme-looking-at-qidsym)
          (if (save-match-data
                (string-match "\\`---*\\'" (match-string-no-properties 0)))
              (progn
                (set-match-data (list point (set-marker (make-marker) (line-end-position))))
                'comment)
            'qsymid))
     (and (looking-at purescript-lexeme-number)
          'number)
     (and (looking-at "'+")
          'template-purescript-quote)
     (and (looking-at ".")
          'illegal))))


;;; SECTION
;;; purescript-font-lock

;;;###autoload
(defgroup purescript-appearance nil
  "Purescript Appearance."
  :group 'purescript)


(defcustom purescript-font-lock-symbols nil
  "Display \\ and -> and such using symbols in fonts.

This may sound like a neat trick, but be extra careful: it changes the
alignment and can thus lead to nasty surprises with regards to layout."
  :group 'purescript-appearance
  :type 'boolean)

(defcustom purescript-font-lock-symbols-alist
  '(("\\" . "λ")
    ("not" . "¬")
    ("->" . "→")
    ("<-" . "←")
    ("=>" . "⇒")
    ("()" . "∅")
    ("==" . "≡")
    ("/=" . "≢")
    (">=" . "≥")
    ("<=" . "≤")
    ("!!" . "‼")
    ("&&" . "∧")
    ("||" . "∨")
    ("sqrt" . "√")
    ("undefined" . "⊥")
    ("pi" . "π")
    ("~>" . "⇝") ;; Omega language
    ;; ("~>" "↝") ;; less desirable
    ("-<" . "↢") ;; Paterson's arrow syntax
    ;; ("-<" "⤙") ;; nicer but uncommon
    ("::" . "∷")
    ("." "∘" ; "○"
     ;; Need a predicate here to distinguish the . used by
     ;; forall <foo> . <bar>.
     purescript-font-lock-dot-is-not-composition)
    ("forall" . "∀"))
  "Alist mapping Purescript symbols to chars.

Each element has the form (STRING . COMPONENTS) or (STRING
COMPONENTS PREDICATE).

STRING is the Purescript symbol.
COMPONENTS is a representation specification suitable as an argument to
`compose-region'.
PREDICATE if present is a function of one argument (the start position
of the symbol) which should return non-nil if this mapping should
be disabled at that position."
  :type '(alist string string)
  :group 'purescript-appearance)

(defcustom purescript-font-lock-keywords
  ;; `as', `hiding', and `qualified' are part of the import
  ;; spec syntax, but they are not reserved.
  ;; `_' can go in here since it has temporary word syntax.
  '("case" "class" "data" "default" "deriving" "do"
    "else" "if" "import" "in" "infix" "infixl"
    "infixr" "instance" "let" "module" "mdo" "newtype" "of"
    "rec" "pattern" "proc" "signature" "then" "type" "where" "_")
  "Identifiers treated as reserved keywords in Purescript."
  :group 'purescript-appearance
  :type '(repeat string))


(defun purescript-font-lock-dot-is-not-composition (start)
  "Return non-nil if the \".\" at START is not a composition operator.
This is the case if the \".\" is part of a \"forall <tvar> . <type>\"."
  (save-excursion
    (goto-char start)
    (or (re-search-backward "\\<forall\\>[^.\"]*\\="
                            (line-beginning-position) t)
        (not (or
              (string= " " (string (char-after start)))
              (null (char-before start))
              (string= " " (string (char-before start))))))))

(defvar purescript-yesod-parse-routes-mode-keywords
  '(("^\\([^ \t\n]+\\)\\(?:[ \t]+\\([^ \t\n]+\\)\\)?"
     (1 'font-lock-string-face)
     (2 'purescript-constructor-face nil lax))))

(define-derived-mode purescript-yesod-parse-routes-mode text-mode "Yesod parseRoutes mode"
  "Mode for parseRoutes from Yesod."
  (setq-local font-lock-defaults '(purescript-yesod-parse-routes-mode-keywords t t nil nil)))

(defcustom purescript-font-lock-quasi-quote-modes
  `(("hsx" . xml-mode)
    ("hamlet" . shakespeare-hamlet-mode)
    ("shamlet" . shakespeare-hamlet-mode)
    ("whamlet" . shakespeare-hamlet-mode)
    ("xmlQQ" . xml-mode)
    ("xml" . xml-mode)
    ("cmd" . shell-mode)
    ("sh_" . shell-mode)
    ("jmacro" . javascript-mode)
    ("jmacroE" . javascript-mode)
    ("r" . ess-mode)
    ("rChan" . ess-mode)
    ("sql" . sql-mode)
    ("json" . json-mode)
    ("aesonQQ" . json-mode)
    ("parseRoutes" . purescript-yesod-parse-routes-mode))
  "Mapping from quasi quoter token to fontification mode.

If a quasi quote is seen in Purescript code its contents will have
font faces assigned as if respective mode was enabled."
  :group 'purescript-appearance
  :type '(repeat (cons string symbol)))

;;;###autoload
(defface purescript-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face used to highlight Purescript keywords."
  :group 'purescript-appearance)

;;;###autoload
(defface purescript-type-face
  '((t :inherit font-lock-type-face))
  "Face used to highlight Purescript types"
  :group 'purescript-appearance)

;;;###autoload
(defface purescript-constructor-face
  '((t :inherit font-lock-type-face))
  "Face used to highlight Purescript constructors."
  :group 'purescript-appearance)

;; This used to be `font-lock-variable-name-face' but it doesn't result in
;; a highlighting that's consistent with other modes (it's mostly used
;; for function defintions).
(defface purescript-definition-face
  '((t :inherit font-lock-function-name-face))
  "Face used to highlight Purescript definitions."
  :group 'purescript-appearance)

;; This is probably just wrong, but it used to use
;; `font-lock-function-name-face' with a result that was not consistent with
;; other major modes, so I just exchanged with `purescript-definition-face'.
;;;###autoload
(defface purescript-operator-face
  '((t :inherit font-lock-variable-name-face))
  "Face used to highlight Purescript operators."
  :group 'purescript-appearance)

;;;###autoload
(defface purescript-pragma-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used to highlight Purescript pragmas ({-# ... #-})."
  :group 'purescript-appearance)

;;;###autoload
(defface purescript-liquid-purescript-annotation-face
  '((t :inherit purescript-pragma-face))
  "Face used to highlight LiquidPurescript annotations ({-@ ... @-})."
  :group 'purescript-appearance)

;;;###autoload
(defface purescript-literate-comment-face
  '((t :inherit font-lock-doc-face))
  "Face with which to fontify literate comments.
Inherit from `default' to avoid fontification of them."
  :group 'purescript-appearance)

(defface purescript-quasi-quote-face
  '((t :inherit font-lock-string-face))
  "Generic face for quasiquotes.

Some quote types are fontified according to other mode defined in
`purescript-font-lock-quasi-quote-modes'."
  :group 'purescript-appearance)

(defun purescript-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((start (match-beginning 0))
         (end (match-end 0))
         (syntaxes (cond
                    ((eq (char-syntax (char-after start)) ?w) '(?w))
                    ((eq (char-syntax (char-after start)) ?.) '(?.))
                    ;; Special case for the . used for qualified names.
                    ((and (eq (char-after start) ?\.) (= end (1+ start)))
                     '(?_ ?\\ ?w))
                    (t '(?_ ?\\))))
         sym-data)
    (if (or (memq (char-syntax (or (char-before start) ?\ )) syntaxes)
            (memq (char-syntax (or (char-after end) ?\ )) syntaxes)
            (or (elt (syntax-ppss) 3) (elt (syntax-ppss) 4))
            (and (consp (setq sym-data (cdr (assoc (match-string 0) alist))))
                 (let ((pred (cadr sym-data)))
                   (setq sym-data (car sym-data))
                   (funcall pred start))))
        ;; No composition for you.  Let's actually remove any composition
        ;; we may have added earlier and which is now incorrect.
        (remove-text-properties start end '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region start end sym-data)))
  ;; Return nil because we're not adding any face property.
  nil)

(defun purescript-font-lock-symbols-keywords ()
  (when (and purescript-font-lock-symbols
             purescript-font-lock-symbols-alist)
    `((,(regexp-opt (mapcar 'car purescript-font-lock-symbols-alist) t)
       (0 (purescript-font-lock-compose-symbol ',purescript-font-lock-symbols-alist)
          ;; In Emacs-21, if the `override' field is nil, the face
          ;; expressions is only evaluated if the text has currently
          ;; no face.  So force evaluation by using `keep'.
          keep)))))

(defun purescript-font-lock--forward-type (&optional ignore)
  "Find where does this type declaration end.

Moves the point to the end of type declaration. It should be
invoked with point just after one of type introducing keywords
like ::, class, instance, data, newtype, type."
  (interactive)
  (let ((cont t)
        (end (point))
        (token nil)
        ;; we are starting right after ::
        (last-token-was-operator t)
        (last-token-was-newline nil)
        (open-parens 0))
    (while cont
      (setq token (purescript-lexeme-looking-at-token 'newline))

      (cond
       ((null token)
        (setq cont nil))
       ((member token '(newline))
        (setq last-token-was-newline (not last-token-was-operator))
        (setq end (match-end 0))
        (goto-char (match-end 0)))
       ((member (match-string-no-properties 0)
                    '(")" "]" "}"))
        (setq open-parens (1- open-parens))
        (if (< open-parens 0)
            ;; unmatched closing parenthesis closes type declaration
            (setq cont nil)
          (setq end (match-end 0))
          (goto-char end))
        (setq last-token-was-newline nil))
       ((and (member (match-string-no-properties 0)
                     '("," ";" "|"))
             (not (member (match-string-no-properties 0) ignore)))
        (if (equal 0 open-parens)
            (setq cont nil)
          (setq last-token-was-operator t)
          (setq end (match-end 0))
          (goto-char end))
        (setq last-token-was-newline nil))
       ((and (or (member (match-string-no-properties 0)
                         '("<-" "=" "←"))
                 (member (match-string-no-properties 0) purescript-font-lock-keywords))
             (not (member (match-string-no-properties 0) ignore)))
        (setq cont nil)
        (setq last-token-was-newline nil))
       ((member (match-string-no-properties 0)
                '("(" "[" "{"))
        (if last-token-was-newline
            (setq cont nil)
          (setq open-parens (1+ open-parens))
          (setq end (match-end 0))
          (goto-char end)
          (setq last-token-was-newline nil)))
       ((member token '(qsymid char string number template-purescript-quote template-purescript-quasi-quote))
        (setq last-token-was-operator (member (purescript-lexeme-classify-by-first-char (char-after (match-beginning 1)))
                                              '(varsym consym)))
        (if (and (not last-token-was-operator) last-token-was-newline)
            (setq cont nil)

          (goto-char (match-end 0))
          (setq end (point)))
        (setq last-token-was-newline nil))
       ((member token '(comment nested-comment literate-comment))
        (goto-char (match-end 0))
        (setq end (point)))
       (t
        (goto-char (match-end 0))
        (setq end (point))
        (setq last-token-was-newline nil))))
    (goto-char end)))


(defun purescript-font-lock--select-face-on-type-or-constructor ()
  "Private function used to select either type or constructor face
on an uppercase identifier."
  (cl-case (purescript-lexeme-classify-by-first-char (char-after (match-beginning 1)))
    (varid (let ((word (match-string-no-properties 0)))
             (cond
              ((member word purescript-font-lock-keywords)
               ;; Note: keywords parse as keywords only when not qualified.
               ;; GHC parses Control.let as a single but illegal lexeme.
               (when (member word '("class" "instance" "type" "data" "newtype"))
                 (save-excursion
                   (goto-char (match-end 0))
                   (save-match-data
                     (purescript-font-lock--forward-type
                      (cond
                       ((member word '("class" "instance"))
                        '("|"))
                       ((member word '("type"))
                        ;; Need to support 'type instance'
                        '("=" "instance")))))
                   (add-text-properties (match-end 0) (point) '(font-lock-multiline t purescript-type t))))
               'purescript-keyword-face)
              ((member word '("forall"))
               (when (get-text-property (match-beginning 0) 'purescript-type)
                 'purescript-keyword-face)))))
    (conid (if (get-text-property (match-beginning 0) 'purescript-type)
               'purescript-type-face
             'purescript-constructor-face))
    (varsym (unless (and (member (match-string 0) '("-" "+" "."))
                         (equal (string-to-syntax "w") (syntax-after (match-beginning 0))))
              ;; We need to protect against the case of
              ;; plus, minus or dot inside a floating
              ;; point number.
              'purescript-operator-face))
    (consym (if (not (member (match-string 1) '("::" "∷")))
                (if (get-text-property (match-beginning 0) 'purescript-type)
                    'purescript-type-face
                  'purescript-constructor-face)
              (save-excursion
                (goto-char (match-end 0))
                (save-match-data
                  (purescript-font-lock--forward-type))
                (add-text-properties (match-end 0) (point) '(font-lock-multiline t purescript-type t)))
              'purescript-operator-face))))

(defun purescript-font-lock--put-face-on-type-or-constructor ()
  "Private function used to put either type or constructor face
on an uppercase identifier."
  (let ((face (purescript-font-lock--select-face-on-type-or-constructor)))
    (when (and face
               (not (text-property-not-all (match-beginning 0) (match-end 0) 'face nil)))
      (put-text-property (match-beginning 0) (match-end 0) 'face face))))


(defun purescript-font-lock-keywords ()
  ;; this has to be a function because it depends on global value of
  ;; `purescript-font-lock-symbols'
  "Generate font lock eywords."
  (let* (;; Bird-style literate scripts start a line of code with
         ;; "^>", otherwise a line of code starts with "^".
         (line-prefix "^\\(?:> ?\\)?")

         (varid "[[:lower:]_][[:alnum:]'_]*")
         ;; We allow ' preceding conids because of DataKinds/PolyKinds
         (conid "'?[[:upper:]][[:alnum:]'_]*")
         (sym "\\s.+")

         ;; Top-level declarations
         (topdecl-var
          (concat line-prefix "\\(" varid "\\(?:\\s-*,\\s-*" varid "\\)*" "\\)"
                  ;; optionally allow for a single newline after identifier
                  "\\(\\s-+\\|\\s-*[\n]\\s-+\\)"
                  ;; A toplevel declaration can be followed by a definition
                  ;; (=), a type (::) or (∷), a guard, or a pattern which can
                  ;; either be a variable, a constructor, a parenthesized
                  ;; thingy, or an integer or a string.
                  "\\(" varid "\\|" conid "\\|::\\|∷\\|=\\||\\|\\s(\\|[0-9\"']\\)"))
         (topdecl-var2
          (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*`\\(" varid "\\)`"))
         (topdecl-bangpat
          (concat line-prefix "\\(" varid "\\)\\s-*!"))
         (topdecl-sym
          (concat line-prefix "\\(" varid "\\|" conid "\\)\\s-*\\(" sym "\\)"))
         (topdecl-sym2 (concat line-prefix "(\\(" sym "\\))"))

         keywords)

    (setq keywords
          `(;; NOTICE the ordering below is significant
            ;;
            ("^#\\(?:[^\\\n]\\|\\\\\\(?:.\\|\n\\|\\'\\)\\)*\\(?:\n\\|\\'\\)" 0 'font-lock-preprocessor-face t)

            ,@(purescript-font-lock-symbols-keywords)

            ;; Special case for `as', `hiding', `safe' and `qualified', which are
            ;; keywords in import statements but are not otherwise reserved.
            ("\\<import[ \t]+\\(?:\\(safe\\>\\)[ \t]*\\)?\\(?:\\(qualified\\>\\)[ \t]*\\)?\\(?:\"[^\"]*\"[\t ]*\\)?[^ \t\n()]+[ \t]*\\(?:\\(\\<as\\>\\)[ \t]*[^ \t\n()]+[ \t]*\\)?\\(\\<hiding\\>\\)?"
             (1 'purescript-keyword-face nil lax)
             (2 'purescript-keyword-face nil lax)
             (3 'purescript-keyword-face nil lax)
             (4 'purescript-keyword-face nil lax))

            ;; Special case for `foreign import'
            ;; keywords in foreign import statements but are not otherwise reserved.
            ("\\<\\(foreign\\)[ \t]+\\(import\\)[ \t]+\\(?:\\(ccall\\|stdcall\\|cplusplus\\|jvm\\|dotnet\\)[ \t]+\\)?\\(?:\\(safe\\|unsafe\\|interruptible\\)[ \t]+\\)?"
             (1 'purescript-keyword-face nil lax)
             (2 'purescript-keyword-face nil lax)
             (3 'purescript-keyword-face nil lax)
             (4 'purescript-keyword-face nil lax))

            ;; Special case for `foreign export'
            ;; keywords in foreign export statements but are not otherwise reserved.
            ("\\<\\(foreign\\)[ \t]+\\(export\\)[ \t]+\\(?:\\(ccall\\|stdcall\\|cplusplus\\|jvm\\|dotnet\\)[ \t]+\\)?"
             (1 'purescript-keyword-face nil lax)
             (2 'purescript-keyword-face nil lax)
             (3 'purescript-keyword-face nil lax))

            ;; Special case for `type family' and `data family'.
            ;; `family' is only reserved in these contexts.
            ("\\<\\(type\\|data\\)[ \t]+\\(family\\>\\)"
             (1 'purescript-keyword-face nil lax)
             (2 'purescript-keyword-face nil lax))

            ;; Special case for `type role'
            ;; `role' is only reserved in this context.
            ("\\<\\(type\\)[ \t]+\\(role\\>\\)"
             (1 'purescript-keyword-face nil lax)
             (2 'purescript-keyword-face nil lax))

            ;; Toplevel Declarations.
            ;; Place them *before* generic id-and-op highlighting.
            (,topdecl-var  (1 (unless (member (match-string 1) purescript-font-lock-keywords)
                                'purescript-definition-face)))
            (,topdecl-var2 (2 (unless (member (match-string 2) purescript-font-lock-keywords)
                                'purescript-definition-face)))
            (,topdecl-bangpat  (1 (unless (member (match-string 1) purescript-font-lock-keywords)
                                'purescript-definition-face)))
            (,topdecl-sym  (2 (unless (member (match-string 2) '("\\" "=" "->" "→" "<-" "←" "::" "∷" "," ";" "`"))
                                'purescript-definition-face)))
            (,topdecl-sym2 (1 (unless (member (match-string 1) '("\\" "=" "->" "→" "<-" "←" "::" "∷" "," ";" "`"))
                                'purescript-definition-face)))

            ;; These four are debatable...
            ("(\\(,*\\|->\\))" 0 'purescript-constructor-face)
            ("\\[\\]" 0 'purescript-constructor-face)

            ("`"
             (0 (if (or (elt (syntax-ppss) 3) (elt (syntax-ppss) 4))
                    (parse-partial-sexp (point) (point-max) nil nil (syntax-ppss)
                                        'syntax-table)
                  (when (save-excursion
                          (goto-char (match-beginning 0))
                          (purescript-lexeme-looking-at-backtick))
                    (goto-char (match-end 0))
                    (unless (text-property-not-all (match-beginning 1) (match-end 1) 'face nil)
                      (put-text-property (match-beginning 1) (match-end 1) 'face 'purescript-operator-face))
                    (unless (text-property-not-all (match-beginning 2) (match-end 2) 'face nil)
                      (put-text-property (match-beginning 2) (match-end 2) 'face 'purescript-operator-face))
                    (unless (text-property-not-all (match-beginning 4) (match-end 4) 'face nil)
                      (put-text-property (match-beginning 4) (match-end 4) 'face 'purescript-operator-face))
                    (add-text-properties
                     (match-beginning 0) (match-end 0)
                     '(font-lock-fontified t fontified t font-lock-multiline t))))))

            (,purescript-lexeme-idsym-first-char
             (0 (if (or (elt (syntax-ppss) 3) (elt (syntax-ppss) 4))
                    (parse-partial-sexp (point) (point-max) nil nil (syntax-ppss)
                                        'syntax-table)
                  (when (save-excursion
                          (goto-char (match-beginning 0))
                          (purescript-lexeme-looking-at-qidsym))
                    (goto-char (match-end 0))
                    ;; note that we have to put face ourselves here because font-lock
                    ;; will use match data from the original matcher
                    (purescript-font-lock--put-face-on-type-or-constructor)))))))
    keywords))


(defun purescript-font-lock-fontify-block (lang-mode start end)
  "Fontify a block as LANG-MODE."
  (let ((string (buffer-substring-no-properties start end))
        (modified (buffer-modified-p))
        (org-buffer (current-buffer)) pos next)
    (remove-text-properties start end '(face nil))
    (with-current-buffer
        (get-buffer-create
         (concat " purescript-font-lock-fontify-block:" (symbol-name lang-mode)))
      (delete-region (point-min) (point-max))
      (insert string " ") ;; so there's a final property change
      (cl-letf (((symbol-function 'message)
                 (lambda (_fmt &rest _args))))
        ;; silence messages
        (unless (eq major-mode lang-mode) (funcall lang-mode))
        (font-lock-ensure))
      (setq pos (point-min))
      (while (setq next (next-single-property-change pos 'face))
        (put-text-property
         (+ start (1- pos)) (1- (+ start next)) 'face
         (or (get-text-property pos 'face) 'default) org-buffer)
        (setq pos next))
      (unless (equal pos (point-max))
        (put-text-property
         (+ start (1- pos)) (1- (+ start (point-max))) 'face
         'default org-buffer)))
    (add-text-properties
     start end
     '(font-lock-fontified t fontified t font-lock-multiline t))
    (set-buffer-modified-p modified)))

(defun purescript-syntactic-face-function (state)
  "`font-lock-syntactic-face-function' for Purescript."
  (cond
   ((nth 3 state)
    (if (equal ?| (nth 3 state))
        ;; find out what kind of QuasiQuote is this
        (let* ((qqname (save-excursion
                        (goto-char (nth 8 state))
                        (skip-syntax-backward "w._")
                        (buffer-substring-no-properties (point) (nth 8 state))))
               (lang-mode (cdr (assoc qqname purescript-font-lock-quasi-quote-modes))))

          (if (and lang-mode
                   (fboundp lang-mode))
              (save-excursion
                ;; find the end of the QuasiQuote
                (parse-partial-sexp (point) (point-max) nil nil state
                                    'syntax-table)
                (purescript-font-lock-fontify-block lang-mode (1+ (nth 8 state)) (1- (point)))
                ;; must return nil here so that it is not fontified again as string
                nil)
            ;; fontify normally as string because lang-mode is not present
            'purescript-quasi-quote-face))
      (save-excursion
        (let
            ((state2
              (parse-partial-sexp (point) (point-max) nil nil state
                                  'syntax-table))
             (end-of-string (point)))

          (put-text-property (nth 8 state) (point)
                             'face 'font-lock-string-face)


          (if (or (equal t (nth 3 state)) (nth 3 state2))
              ;; This is an unterminated string constant, use warning
              ;; face for the opening quote.
              (put-text-property (nth 8 state) (1+ (nth 8 state))
                                 'face 'font-lock-warning-face))

          (goto-char (1+ (nth 8 state)))
          (while (re-search-forward "\\\\" end-of-string t)

            (goto-char (1- (point)))

            (if (looking-at purescript-lexeme-string-literal-inside-item)
                (goto-char (match-end 0))

              ;; We are looking at an unacceptable escape
              ;; sequence. Use warning face to highlight that.
              (put-text-property (point) (1+ (point))
                                 'face 'font-lock-warning-face)
              (goto-char (1+ (point)))))))
      ;; must return nil here so that it is not fontified again as string
      nil))
   ;; Detect literate comment lines starting with syntax class '<'
   ((save-excursion
      (goto-char (nth 8 state))
      (equal (string-to-syntax "<") (syntax-after (point))))
    'purescript-literate-comment-face)
   ;; Detect pragmas. A pragma is enclosed in special comment
   ;; delimiters {-# .. #-}.
   ((save-excursion
      (goto-char (nth 8 state))
      (and (looking-at-p "{-#")
           (forward-comment 1)
           (goto-char (- (point) 3))
           (looking-at-p "#-}")))
    'purescript-pragma-face)
   ;; Detect Liquid Purescript annotations enclosed in special comment
   ;; delimiters {-@ .. @-}.
   ((save-excursion
      (goto-char (nth 8 state))
      (and (looking-at-p "{-@")
           (forward-comment 1)
           (goto-char (- (point) 3))
           (looking-at-p "@-}")))
    'purescript-liquid-purescript-annotation-face)
   ;; Haddock comment start with either "-- [|^*$]" or "{- ?[|^*$]"
   ;; (note space optional for nested comments and mandatory for
   ;; double dash comments).
   ;;
   ;; Haddock comment will also continue on next line, provided:
   ;; - current line is a double dash haddock comment
   ;; - next line is also double dash comment
   ;; - there is only whitespace between
   ;;
   ;; We recognize double dash haddock comments by property
   ;; 'font-lock-doc-face attached to newline. In case of {- -}
   ;; comments newline is outside of comment.
   ((save-excursion
      (goto-char (nth 8 state))
      (or (looking-at-p "\\(?:{- ?\\|-- \\)[|^*$]")
          (and (looking-at-p "--")            ; are we at double dash comment
               (forward-line -1)              ; this is nil on first line
               (eq (get-text-property (line-end-position) 'face)
                   'font-lock-doc-face)       ; is a doc face
               (forward-line)
               (skip-syntax-forward "-")      ; see if there is only whitespace
               (eq (point) (nth 8 state)))))  ; we are back in position
    ;; Here we look inside the comment to see if there are substrings
    ;; worth marking inside we try to emulate as much of haddock as
    ;; possible.  First we add comment face all over the comment, then
    ;; we add special features.
    (let ((beg (nth 8 state))
          (end (save-excursion
                 (parse-partial-sexp (point) (point-max) nil nil state
                                     'syntax-table)
                 (point)))
          (emphasis-open-point nil)
          (strong-open-point nil))
      (put-text-property beg end 'face 'font-lock-doc-face)

      (when (fboundp 'add-face-text-property)
        ;; `add-face-text-property' is not defined in Emacs 23

        ;; iterate over chars, take escaped chars unconditionally
        ;; mark when a construct is opened, close and face it when
        ;; it is closed

        (save-excursion
          (while (< (point) end)
            (if (looking-at "__\\|\\\\.\\|\\\n\\|[/]")
                (progn
                  (cond
                   ((equal (match-string 0) "/")
                    (if emphasis-open-point
                        (progn
                          (add-face-text-property emphasis-open-point (match-end 0)
                                                  '(:slant italic))
                          (setq emphasis-open-point nil))
                      (setq emphasis-open-point (point))))
                   ((equal (match-string 0) "__")
                    (if strong-open-point
                        (progn
                          (add-face-text-property strong-open-point (match-end 0)
                                                  '(:weight bold))
                          (setq strong-open-point nil))
                      (setq strong-open-point (point))))
                   (t
                    ;; this is a backslash escape sequence, skip over it
                    ))
                  (goto-char (match-end 0)))
              ;; skip chars that are not interesting
              (goto-char (1+ (point)))
              (skip-chars-forward "^_\\\\/" end))))))
    nil)
   (t 'font-lock-comment-face)))

(defun purescript-font-lock-defaults-create ()
  "Locally set `font-lock-defaults' for Purescript."
  (setq-local font-lock-defaults
              '((purescript-font-lock-keywords)
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . purescript-syntactic-face-function)
                ;; Get help from font-lock-syntactic-keywords.
                (parse-sexp-lookup-properties . t)
                (font-lock-extra-managed-props . (composition)))))

(defun purescript-fontify-as-mode (text mode)
  "Fontify TEXT as MODE, returning the fontified text."
  (with-temp-buffer
    (funcall mode)
    (insert text)
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings (font-lock-fontify-buffer)))
    (buffer-substring (point-min) (point-max))))

;; Provide ourselves:

(provide 'custom-purescript-font-lock)

;; Local Variables:
;; coding: utf-8-unix
;; tab-width: 8
;; End:

;;; purescript-font-lock.el ends here
