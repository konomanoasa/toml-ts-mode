;;; toml-ts-mode.el --- Tree-sitter mode for TOML 1.1.0  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 konomanoasa
;;
;; Author: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Maintainer: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: languages
;; URL: https://github.com/konomanoasa/toml-ts-mode
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; Tree-sitter major mode for TOML 1.1.0.

;;; Code:

(require 'paren)
(require 'treesit)

(defgroup toml-ts nil
  "Tree-sitter mode for TOML 1.1.0."
  :group 'languages)

(defconst toml-ts-mode--grammar-sources
  '((toml "https://github.com/konomanoasa/tree-sitter-toml"
          :revision "v0.3.0"))
  "Tree-sitter grammar sources for TOML 1.1.0.")

;;;; Syntax

(defvar toml-ts-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?# ?\" ?\' ?\\ ?\( ?\) ?\[ ?\] ?{ ?}))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `toml-ts-mode'.")

;;;;; Syntax Queries

(defconst toml-ts-mode-syntax--query
  (treesit-query-compile
   'toml
   '((comment) @comment
     (basic_string) @string
     (literal_string) @string
     (ml_basic_string) @string
     (ml_literal_string) @string
     (array "[" @open "]" @close)
     (inline_table "{" @open "}" @close)
     (std_table "[" @open "]" @close)
     (array_table "[[" @open "]]" @close)))
  "Compiled syntax query for TOML 1.1.0.")

;;;;; Propertization

(defun toml-ts-mode-syntax--propertize (start end)
  "Apply syntax properties between START and END."
  (let ((accessible-start (point-min)))
    (save-restriction
      (widen)
      (when (and (= start accessible-start)
                 (> accessible-start (point-min)))
        (remove-text-properties (point-min) start '(syntax-table nil))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (dolist (capture (treesit-query-capture
                        (treesit-parser-root-node treesit-primary-parser)
                        toml-ts-mode-syntax--query start end))
        (let* ((name (car capture))
               (node (cdr capture))
               (begin (treesit-node-start node))
               (finish (treesit-node-end node)))
          (unless (or (= begin finish) (treesit-node-check node 'missing))
            (pcase name
              ('comment
               (put-text-property begin (1+ begin) 'syntax-table
                                  (string-to-syntax "<")))
              ('string
               (unless (treesit-node-check node 'has-error)
                 (put-text-property begin (1+ begin) 'syntax-table
                                    (string-to-syntax "|"))
                 (put-text-property (1- finish) finish 'syntax-table
                                    (string-to-syntax "|"))))
              ((or 'open 'close)
               (let ((syntax (if (eq name 'open)
                                 (if (equal (treesit-node-type node) "{")
                                     "(}" "(]")
                               (if (equal (treesit-node-type node) "}")
                                   "){" ")["))))
                 (put-text-property begin finish 'syntax-table
                                    (string-to-syntax syntax))
                 (when (> (- finish begin) 1)
                   (put-text-property begin (1- finish) 'syntax-table
                                      (string-to-syntax
                                       (concat syntax "p")))))))))))))

;;;;; Setup

(defun toml-ts-mode-syntax-setup ()
  "Configure syntax handling for the current buffer."
  (setq-local syntax-propertize-function
              #'toml-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#[[:blank:]]*")
  (setq-local comment-use-syntax t))

;;;; Font Lock

;;;;; Features

(defconst toml-ts-mode-font-lock--feature-list
  '((comment)
    (key string)
    (number boolean date-time escape)
    (operator delimiter bracket))
  "Font-lock features by decoration level.")

;;;;; Settings

(defun toml-ts-mode-font-lock--settings ()
  "Return font-lock settings for the current buffer."
  (treesit-font-lock-rules
   :default-language 'toml

   :feature 'comment
   '((comment) @font-lock-comment-face)

   :feature 'key
   '((unquoted_key) @font-lock-property-name-face
     (quoted_key
      (basic_string (basic_char (basic_unescaped) @font-lock-property-name-face)))
     (quoted_key
      (literal_string (literal_char) @font-lock-property-name-face)))

   :feature 'string
   '((string
      (basic_string (basic_char (basic_unescaped) @font-lock-string-face)))
     (string (literal_string (literal_char) @font-lock-string-face))
     (mlb_content (basic_char (basic_unescaped) @font-lock-string-face))
     (mll_content (literal_char) @font-lock-string-face)
     (basic_string "\"" @font-lock-string-face)
     (literal_string "'" @font-lock-string-face)
     (ml_basic_string "\"\"\"" @font-lock-string-face)
     (ml_literal_string "'''" @font-lock-string-face)
     (mlb_quotes "\"" @font-lock-string-face)
     (mll_quotes "'" @font-lock-string-face))

   :feature 'number
   '((unsigned_dec_int [(digit) (digit1_9) "_"] @font-lock-number-face)
     (zero_prefixable_int [(digit) "_"] @font-lock-number-face)
     (dec_int ["+" "-"] @font-lock-number-face)
     (float_exp_part ["+" "-"] @font-lock-number-face)
     (special_float ["+" "-" "inf" "nan"] @font-lock-number-face)
     (hex_int ["0x" (hexdig) "_"] @font-lock-number-face)
     (oct_int ["0o" (digit0_7) "_"] @font-lock-number-face)
     (bin_int ["0b" (digit0_1) "_"] @font-lock-number-face)
     (frac "." @font-lock-number-face)
     (exp ["e" "E"] @font-lock-number-face))

   :feature 'boolean
   '((boolean ["true" "false"] @font-lock-constant-face))

   :feature 'date-time
   '((date_fullyear (digit) @font-lock-constant-face)
     (date_month (digit) @font-lock-constant-face)
     (date_mday (digit) @font-lock-constant-face)
     (time_hour (digit) @font-lock-constant-face)
     (time_minute (digit) @font-lock-constant-face)
     (time_second (digit) @font-lock-constant-face)
     (full_date "-" @font-lock-constant-face)
     (time_delim ["T" "t" " "] @font-lock-constant-face)
     (partial_time ":" @font-lock-constant-face)
     (time_secfrac ["." (digit)] @font-lock-constant-face)
     (time_numoffset ["+" "-" ":"] @font-lock-constant-face)
     (time_offset ["Z" "z"] @font-lock-constant-face))

   :feature 'escape
   '((escaped "\\" @font-lock-escape-face)
     (escape_seq_char
      ["\"" "\\" "b" "e" "f" "n" "r" "t" "x" "u" "U" (hexdig)]
      @font-lock-escape-face)
     (mlb_escaped_nl "\\" @font-lock-escape-face))

   :feature 'operator
   '((keyval "=" @font-lock-operator-face))

   :feature 'delimiter
   '((dotted_key "." @font-lock-punctuation-face)
     (array_values "," @font-lock-punctuation-face)
     (inline_table_keyvals "," @font-lock-punctuation-face))

   :feature 'bracket
   '((array ["[" "]"] @font-lock-bracket-face)
     (inline_table ["{" "}"] @font-lock-bracket-face)
     (std_table ["[" "]"] @font-lock-bracket-face)
     (array_table ["[[" "]]"] @font-lock-bracket-face))))

;;;;; Setup

(defun toml-ts-mode-font-lock-setup ()
  "Configure font lock for the current buffer."
  (setq-local treesit-font-lock-feature-list
              toml-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (toml-ts-mode-font-lock--settings)))

;;;; Navigation

(defconst toml-ts-mode-thing-settings
  `((toml (sexp "^val$")
          (list ,(rx string-start
                     (or "array" "inline_table" "std_table" "array_table")
                     string-end))))
  "Tree-sitter thing definitions for TOML 1.1.0.")

(defun toml-ts-mode-navigation-setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings
              toml-ts-mode-thing-settings))

;;;; Imenu

(defconst toml-ts-mode-imenu-settings
  `(("Table" ,(rx string-start (or "std_table" "array_table") string-end)
     nil nil))
  "Tree-sitter Imenu settings for TOML 1.1.0.")

(defun toml-ts-mode--defun-name (node)
  "Return the source name of NODE, or nil if it has no name."
  (when (member (treesit-node-type node) '("std_table" "array_table"))
    (treesit-node-text node t)))

(defun toml-ts-mode-imenu-setup ()
  "Configure Imenu for the current buffer."
  (setq-local treesit-defun-name-function
              #'toml-ts-mode--defun-name)
  (setq-local treesit-simple-imenu-settings
              toml-ts-mode-imenu-settings))

;;;; Indentation

(defcustom toml-ts-mode-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'natnum
  :group 'toml-ts)

(defconst toml-ts-mode-indent-rules
  '((toml
     ((parent-is "^ml_basic_string$") no-indent 0)
     ((parent-is "^ml_literal_string$") no-indent 0)
     ((parent-is "^ml_basic_body$") no-indent 0)
     ((parent-is "^ml_literal_body$") no-indent 0)
     ((parent-is "^mlb_content$") no-indent 0)
     ((parent-is "^mll_content$") no-indent 0)
     ((parent-is "^mlb_escaped_nl$") no-indent 0)
     ((node-is "^]$") parent-bol 0)
     ((node-is "^}$") parent-bol 0)
     ((parent-is "^array$") parent-bol toml-ts-mode-indent-offset)
     ((parent-is "^inline_table$") parent-bol toml-ts-mode-indent-offset)
     ((parent-is "^array_values$") parent-bol toml-ts-mode-indent-offset)
     ((parent-is "^inline_table_keyvals$") parent-bol toml-ts-mode-indent-offset)
     ((parent-is "^toml$") column-0 0)
     ((parent-is "^expression$") column-0 0)))
  "Tree-sitter indentation rules for TOML 1.1.0.")

(defun toml-ts-mode-indent-setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules
              toml-ts-mode-indent-rules))

;;;; Mode

(defun toml-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language toml-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (or (treesit-ensure-installed language)
        (user-error "Tree-sitter grammar `%s' is unavailable" language))))

(defun toml-ts-mode--setup ()
  "Configure `toml-ts-mode' in the current buffer."
  (toml-ts-mode--ensure-grammar 'toml)
  (setq-local treesit-primary-parser (treesit-parser-create 'toml))
  (toml-ts-mode-syntax-setup)
  (toml-ts-mode-font-lock-setup)
  (toml-ts-mode-navigation-setup)
  (toml-ts-mode-imenu-setup)
  (toml-ts-mode-indent-setup)
  (treesit-major-mode-setup)
  (setq-local show-paren-data-function #'treesit-show-paren-data))

;;;###autoload
(define-derived-mode toml-ts-mode prog-mode "TOML-TS"
  "Major mode for editing TOML 1.1.0."
  :syntax-table toml-ts-mode-syntax-table
  :group 'toml-ts
  (toml-ts-mode--setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("/Cargo\\.lock\\'" . toml-ts-mode))

(provide 'toml-ts-mode)

;;; toml-ts-mode.el ends here
