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
(require 'elec-pair)
(require 'treesit)

(defgroup toml-ts nil
  "Tree-sitter mode for TOML 1.1.0."
  :group 'languages)

;;;; Grammar

(defconst toml-ts-mode--grammar-sources
  '((toml "https://github.com/konomanoasa/tree-sitter-toml"
          :revision "v0.5.0"))
  "Tree-sitter grammar sources for TOML 1.1.0.")

(defun toml-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language toml-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (or (treesit-ensure-installed language)
        (user-error "Tree-sitter grammar `%s' is unavailable" language))))

;;;; Syntax

(defvar toml-ts-mode-syntax--text-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?# ?\" ?\' ?` ?\\ ?\( ?\) ?\[ ?\] ?{ ?}))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for text without a CST syntax classification.")

(defvar toml-ts-mode-syntax-table
  (let ((table (copy-syntax-table toml-ts-mode-syntax--text-table)))
    (dolist (entry '((?\[ . "(]") (?\] . ")[") (?{ . "(}") (?} . "){")))
      (modify-syntax-entry (car entry) (cdr entry) table))
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
     (array "[" @open) (array "]" @close)
     (inline_table "{" @open) (inline_table "}" @close)
     (std_table "[" @open) (std_table "]" @close)
     (array_table "[[" @open) (array_table "]]" @close)))
  "Compiled syntax query for TOML 1.1.0.")

;;;;; Propertization

(defun toml-ts-mode-syntax--propertize (start end)
  "Apply syntax properties between START and END."
  (let ((accessible-start (point-min)))
    (save-restriction
      (widen)
      (when (and (= start accessible-start)
                 (> accessible-start (point-min)))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (put-text-property start end 'syntax-table toml-ts-mode-syntax--text-table)
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
               (put-text-property begin finish 'syntax-table
                                  toml-ts-mode-syntax--text-table)
               (let ((opening (treesit-node-child node 0))
                     (closing (treesit-node-child node -1)))
                 (when (and (not (treesit-node-check node 'has-error))
                            (member (treesit-node-type opening)
                                    '("\"" "'" "\"\"\"" "'''"))
                            (equal (treesit-node-type opening)
                                   (treesit-node-type closing))
                            (< (treesit-node-start opening)
                               (treesit-node-start closing)))
                   (put-text-property (treesit-node-start opening)
                                      (1+ (treesit-node-start opening))
                                      'syntax-table (string-to-syntax "|"))
                   (put-text-property (1- (treesit-node-end closing))
                                      (treesit-node-end closing)
                                      'syntax-table (string-to-syntax "|")))))
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

;;;;; Matching Delimiters

(defun toml-ts-mode-syntax--show-paren-data ()
  "Return matching delimiter ranges from their CST owner."
  (let ((position (point)))
    (catch 'match
      (dolist (capture (treesit-query-capture
                        (treesit-parser-root-node treesit-primary-parser)
                        toml-ts-mode-syntax--query
                        (max (point-min) (1- position))
                        (min (point-max) (1+ position))))
        (let* ((name (car capture))
               (node (cdr capture))
               (owner (treesit-node-parent node))
               (opening (treesit-node-child owner 0))
               (closing (treesit-node-child owner -1)))
          (when (and (or (and (eq name 'open)
                              (= position (treesit-node-start node)))
                         (and (eq name 'close)
                              (= position (treesit-node-end node))))
                     (not (treesit-node-check opening 'missing))
                     (not (treesit-node-check closing 'missing))
                     (member (list (treesit-node-type opening)
                                   (treesit-node-type closing))
                             '(("[" "]") ("[[" "]]") ("{" "}"))))
            (let ((other (if (eq name 'open) closing opening)))
              (throw 'match (list (treesit-node-start node)
                                  (treesit-node-end node)
                                  (treesit-node-start other)
                                  (treesit-node-end other))))))))))

;;;;; Setup

(defun toml-ts-mode-syntax--setup ()
  "Configure syntax handling for the current buffer."
  (setq-local syntax-propertize-function
              #'toml-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#[[:blank:]]*")
  (setq-local comment-use-syntax t)
  (setq-local show-paren-data-function #'toml-ts-mode-syntax--show-paren-data))

;;;; Electric Pair

(defun toml-ts-mode-electric-pair--newline-context-p ()
  "Return non-nil between adjacent delimiters of an array or inline table."
  (when (and (eq (char-before) ?\n)
             (>= (- (point) 2) (point-min))
             (< (point) (point-max)))
    (let* ((opening (treesit-node-at (- (point) 2) treesit-primary-parser))
           (closing (treesit-node-at (point) treesit-primary-parser))
           (owner (treesit-node-parent opening)))
      (and (= (treesit-node-start opening) (- (point) 2))
           (= (treesit-node-end opening) (1- (point)))
           (= (treesit-node-start closing) (point))
           (= (treesit-node-end closing) (1+ (point)))
           (treesit-node-eq owner (treesit-node-parent closing))
           (member (list (treesit-node-type owner)
                         (treesit-node-type opening)
                         (treesit-node-type closing))
                   '(("array" "[" "]") ("inline_table" "{" "}")))))))

(defun toml-ts-mode-electric-pair--setup ()
  "Configure electric pairing for the current buffer."
  (let ((pairs '((?\[ . ?\]) (?{ . ?})))
        (table (copy-syntax-table (syntax-table))))
    (setq-local electric-pair-pairs (append electric-pair-pairs pairs))
    (dolist (pair pairs)
      (unless (eq (cdr (assq (car pair) electric-pair-pairs)) (cdr pair))
        (modify-syntax-entry (car pair) "." table)))
    (set-syntax-table table))
  (let ((setting electric-pair-open-newline-between-pairs))
    (setq-local electric-pair-open-newline-between-pairs
                (lambda ()
                  (and (if (functionp setting) (funcall setting) setting)
                       (toml-ts-mode-electric-pair--newline-context-p))))))

;;;; Font Lock

;;;;; Features

(defconst toml-ts-mode-font-lock--feature-list
  '((comment)
    (key string)
    (number boolean date-time escape)
    (operator delimiter punctuation bracket))
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
      @font-lock-escape-face))

   :feature 'punctuation
   '((mlb_escaped_nl "\\" @font-lock-punctuation-face))

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

(defun toml-ts-mode-font-lock--setup ()
  "Configure font lock for the current buffer."
  (setq-local treesit-font-lock-feature-list
              toml-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (toml-ts-mode-font-lock--settings)))

;;;; Navigation

(defun toml-ts-mode-navigation--sexp-p (node)
  "Return non-nil if NODE is a representative editing unit."
  (let ((type (treesit-node-type node))
        (parent (treesit-node-parent node)))
    (and (< (treesit-node-start node) (treesit-node-end node))
         (or (member type '("keyval" "key" "val"))
             (and (member type '("array" "inline_table"))
                  (not (and (equal (treesit-node-type parent) "val")
                            (= (treesit-node-start parent) (treesit-node-start node))
                            (= (treesit-node-end parent) (treesit-node-end node)))))))))

(defconst toml-ts-mode-navigation--settings
  `((toml (sexp toml-ts-mode-navigation--sexp-p)
          (defun ,(rx string-start (or "std_table" "array_table") string-end))))
  "Tree-sitter thing definitions for TOML 1.1.0.")

(defun toml-ts-mode-navigation--setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings
              toml-ts-mode-navigation--settings))

;;;; Imenu

(defun toml-ts-mode-imenu--name (node)
  "Return the source name of NODE, or nil if it has no name."
  (when (member (treesit-node-type node) '("std_table" "array_table"))
    (let ((key (treesit-node-child-by-field-name node "key")))
      (when (and key
                 (< (treesit-node-start key) (treesit-node-end key))
                 (not (treesit-node-check key 'missing)))
        (treesit-node-text node t)))))

(defconst toml-ts-mode-imenu--settings
  `(("Table" ,(rx string-start (or "std_table" "array_table") string-end)
     toml-ts-mode-imenu--name nil))
  "Tree-sitter Imenu settings for TOML 1.1.0.")

(defun toml-ts-mode-imenu--setup ()
  "Configure Imenu for the current buffer."
  (setq-local treesit-defun-name-function
              #'toml-ts-mode-imenu--name)
  (setq-local treesit-simple-imenu-settings
              toml-ts-mode-imenu--settings))

;;;; Indentation

(defcustom toml-ts-mode-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'natnum
  :group 'toml-ts)

(defconst toml-ts-mode-indent--rules
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

(defun toml-ts-mode-indent--setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules
              toml-ts-mode-indent--rules))

;;;; Mode

(defun toml-ts-mode--setup ()
  "Configure `toml-ts-mode' in the current buffer."
  (toml-ts-mode--ensure-grammar 'toml)
  (setq-local treesit-primary-parser (treesit-parser-create 'toml))
  (toml-ts-mode-syntax--setup)
  (toml-ts-mode-electric-pair--setup)
  (toml-ts-mode-font-lock--setup)
  (toml-ts-mode-navigation--setup)
  (toml-ts-mode-imenu--setup)
  (toml-ts-mode-indent--setup)
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode toml-ts-mode prog-mode "TOML-TS"
  "Major mode for editing TOML 1.1.0."
  :syntax-table toml-ts-mode-syntax-table
  :group 'toml-ts
  (toml-ts-mode--setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\(?:\\`\\|/\\)Cargo\\.lock\\'" . toml-ts-mode))

(provide 'toml-ts-mode)

;;; toml-ts-mode.el ends here
