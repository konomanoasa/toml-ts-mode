;;; toml-ts-mode-test.el --- Tests for toml-ts-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 konomanoasa
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

;;; Code:

(require 'ert)
(require 'imenu)
(require 'newcomment)
(require 'toml-ts-mode)

(dolist (language '(toml))
  (unless (treesit-ready-p language t)
    (error "The %s grammar is required to run the tests" language)))

;;;; Helpers

(defun toml-ts-mode-test--position (fragment &optional line)
  "Return FRAGMENT's start in LINE, or anywhere when LINE is nil."
  (save-excursion
    (goto-char (point-min))
    (when line
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (if (equal line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
              (setq found t)
            (forward-line 1)))
        (unless found (ert-fail (format "Missing fixture line: %S" line)))))
    (unless (search-forward fragment (and line (line-end-position)) t)
      (ert-fail (format "Missing fixture fragment: %S" fragment)))
    (- (point) (length fragment))))

(defun toml-ts-mode-test--face (fragment &optional offset line)
  "Return the face at OFFSET into FRAGMENT on LINE."
  (get-text-property (+ (toml-ts-mode-test--position fragment line)
                        (or offset 0)) 'face))

(defun toml-ts-mode-test--indent (source &optional offset)
  "Indent SOURCE twice using OFFSET and assert stable whitespace."
  (with-temp-buffer
    (insert source)
    (toml-ts-mode)
    (setq-local indent-tabs-mode nil)
    (when offset
      (setq-local toml-ts-mode-indent-offset offset))
    (indent-region (point-min) (point-max))
    (let ((indented (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) indented))
      indented)))

(defun toml-ts-mode-test--buffer-state ()
  "Return current face and syntax properties after refreshing the buffer."
  (font-lock-ensure)
  (syntax-propertize (point-max))
  (let (state)
    (dotimes (offset (- (point-max) (point-min)))
      (let ((position (+ (point-min) offset)))
        (push (list (get-text-property position 'face) (syntax-after position)) state)))
    (nreverse state)))

(defun toml-ts-mode-test--should-match-fresh-buffer (&optional level compare-tree)
  "Compare classifications at LEVEL and optionally COMPARE-TREE with fresh parsing."
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (toml-ts-mode-test--buffer-state))
        (tree (when compare-tree
                (treesit-node-string (treesit-parser-root-node treesit-primary-parser))))
        (file buffer-file-name)
        (level (or level treesit-font-lock-level)))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level level)) (toml-ts-mode))
      (should (equal state (toml-ts-mode-test--buffer-state)))
      (when compare-tree
        (should (equal tree (treesit-node-string
                             (treesit-parser-root-node treesit-primary-parser))))))))

;;;; Syntax

(ert-deftest toml-ts-mode-classifies-delimiters ()
  (with-temp-buffer
    (insert "[[items]]\nx = [1, {a = 2}]\ns = \"[()] # text\"\n")
    (toml-ts-mode)
    (syntax-propertize (point-max))
    (should (= 2 (car (syntax-ppss 3))))
    (should (= 0 (car (syntax-ppss 10))))
    (should (= 10 (scan-sexps 1 1)))
    (should (= 1 (scan-sexps 10 -1)))
    (should (= 9 (scan-sexps 2 1)))
    (should (= 2 (scan-sexps 9 -1)))
    (should (equal (mapcar (lambda (pos) (syntax-class (syntax-after pos)))
                           '(1 2 8 9))
                   '(4 4 5 5)))
    (pcase-dolist (`(,position ,expected)
                   '((1 (1 2 9 10))
                     (2 (2 3 8 9))
                     (9 (8 9 2 3))
                     (10 (9 10 1 2))))
      (goto-char position)
      (should (equal (funcall show-paren-data-function) expected)))
    (goto-char (point-min))
    (search-forward "text")
    (should (nth 3 (syntax-ppss (point))))
    (should-not (nth 4 (syntax-ppss (point))))))

(ert-deftest toml-ts-mode-classifies-only-owned-delimiters ()
  (with-temp-buffer
    (insert "s = \"\"\"[\\\"'`()]\n# text\n\"\"\"\na = [1]\n")
    (toml-ts-mode)
    (syntax-propertize (point-max))
    (dolist (text '("[\\" "\\\"" "'" "`" "(" ")" "# text"))
      (should (= (syntax-class (syntax-after (toml-ts-mode-test--position text))) 1)))
    (goto-char (toml-ts-mode-test--position "[1]"))
    (let ((start (point)))
      (should (equal (funcall show-paren-data-function)
                     (list start (1+ start) (+ start 2) (+ start 3)))))
    (goto-char (toml-ts-mode-test--position "[\\"))
    (should-not (funcall show-paren-data-function))))

(ert-deftest toml-ts-mode-missing-closings-do-not-create-matching-delimiters ()
  (dolist (source '("a=[1" "a={b=1" "[[a]"))
    (with-temp-buffer
      (insert source)
      (toml-ts-mode)
      (syntax-propertize (point-max))
      (goto-char (point-min))
      (search-forward (if (equal source "a={b=1") "{" "["))
      (backward-char)
      (should-not (funcall show-paren-data-function)))))

;;;; Comment Commands

(ert-deftest toml-ts-mode-uncomments-multiline-source ()
  (dolist (case
           '(("text = \"\"\"one\ntwo\"\"\" # keep\n" "# text = \"\"\"one\n# two\"\"\" # keep\n")
             ("text = '''one\n# two'''\n" "# text = '''one\n# # two'''\n")))
    (with-temp-buffer
      (insert (car case))
      (toml-ts-mode)
      (let ((state (toml-ts-mode-test--buffer-state)))
        (comment-or-uncomment-region (point-min) (point-max))
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       (cadr case)))
        (comment-or-uncomment-region (point-min) (point-max))
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       (car case)))
        (should (equal (toml-ts-mode-test--buffer-state) state))))))

;;;; Electric Pair

(ert-deftest toml-ts-mode-pairs-delimiters-and-indents-on-return ()
  (pcase-dolist (`(,prefix ,opening ,expected ,column)
                 '(("a = " ?\[ "a = [\n  \n]" 2)
                   ("a = " ?{ "a = {\n  \n}" 2)
                   ("" ?\[ "[\n]" 0)))
    (let ((electric-pair-pairs nil)
          (electric-pair-open-newline-between-pairs t))
      (with-temp-buffer
        (toml-ts-mode)
        (setq-local indent-tabs-mode nil)
        (electric-indent-local-mode 1)
        (electric-pair-local-mode 1)
        (insert prefix)
        (let ((last-command-event opening))
          (self-insert-command 1))
        (call-interactively (key-binding (kbd "RET")))
        (should (equal (buffer-string) expected))
        (should (= (current-column) column))))))

(ert-deftest toml-ts-mode-restricts-pair-newlines-to-cst-contexts ()
  (let ((electric-pair-open-newline-between-pairs t))
    (pcase-dolist (`(,before ,after ,expected)
                   '(("a = [" "]" "a = [\n\n]")
                     ("a = {" "}" "a = {\n\n}")
                     ("[" "]" "[\n]")
                     ("[[" "]]" "[[\n]]")
                     ("s = \"[" "]\"" "s = \"[\n]\"")
                     ("s = '''{" "}'''" "s = '''{\n}'''")
                     ("# [" "]" "# [\n]")))
      (ert-info ((format "%S / %S" before after))
        (with-temp-buffer
          (insert before after)
          (toml-ts-mode)
          (electric-indent-local-mode -1)
          (electric-pair-local-mode 1)
          (goto-char (1+ (length before)))
          (call-interactively (key-binding (kbd "RET")))
          (should (equal (buffer-string) expected)))))))

;;;; Navigation

(ert-deftest toml-ts-mode-navigates-inline-entries-and-dotted-keys ()
  (with-temp-buffer
    (insert "a = { nested.key = [12, 34], other = true }\n")
    (toml-ts-mode)
    (pcase-dolist (`(,fragment ,offset ,count ,target ,target-offset)
                   '(("nested.key" 0 1 "[12, 34]" 8)
                     ("nested.key" 1 1 "nested.key" 10)
                     ("nested.key" 10 -1 "nested.key" 0)
                     ("[12, 34]" 8 -1 "nested.key" 0)
                     ("12" 0 1 "12" 2)))
      (goto-char (+ (toml-ts-mode-test--position fragment) offset))
      (forward-sexp count)
      (should (= (point) (+ (toml-ts-mode-test--position target)
                            target-offset))))
    (goto-char (toml-ts-mode-test--position "nested.key"))
    (should-error (backward-sexp) :type 'scan-error)))

(ert-deftest toml-ts-mode-navigates-header-defuns ()
  (with-temp-buffer
    (insert "[server]\na = 1\n[[items]]\nx = 1\n")
    (toml-ts-mode)
    (goto-char (point-min))
    (end-of-defun)
    (should (= (point) 10))
    (beginning-of-defun)
    (should (= (point) 1))
    (goto-char (point-max))
    (beginning-of-defun)
    (should (= (point) 16))
    (end-of-defun)
    (should (= (point) 26))
    (goto-char 16)
    (insert "# ")
    (goto-char (point-max))
    (beginning-of-defun)
    (should (= (point) 1))))

;;;; Imenu

(ert-deftest toml-ts-mode-indexes-definitions ()
  (with-temp-buffer
    (insert "[server]\na = 1\n[[items]]\nx = 1\n[server.tls]\n[[items]]\n")
    (toml-ts-mode)
    (let* ((index (funcall imenu-create-index-function))
           (entries (cdar index)))
      (should (equal (mapcar #'car index) '("Table")))
      (should (equal (mapcar #'car entries)
                     '("[server]" "[[items]]" "[server.tls]" "[[items]]")))
      (should (equal (mapcar (lambda (entry) (marker-position (cdr entry)))
                             entries)
                     '(1 16 32 45))))))

;;;; Indentation

(ert-deftest toml-ts-mode-indents-structures ()
  (should (equal (toml-ts-mode-test--indent
                  "[server]\na = [\n1,\n[\n2,\n],\n]\nb = {\nx = 1,\ny = {\nz = 2,\n},\n}\n" 3)
                 "[server]\na = [\n   1,\n   [\n      2,\n   ],\n]\nb = {\n   x = 1,\n   y = {\n      z = 2,\n   },\n}\n")))

(ert-deftest toml-ts-mode-preserves-multiline-string-whitespace ()
  (dolist (source '("a = \"\"\"\n  body\n    text\n  \"\"\"\n"
                    "a = '''\n  body\n    text\n  '''\n"
                    "a = [\n  \"\"\"\n x\n\n   y\n  \"\"\",\n]\n"))
    (should (equal (toml-ts-mode-test--indent source 2) source))))

(ert-deftest toml-ts-mode-indents-comment-and-empty-container-lines ()
  (should (equal (toml-ts-mode-test--indent
                  "a = [\n# first\n1,\n# last\n]\nb = {\n# first\nx = 1,\n# last\n}\n" 4)
                 "a = [\n    # first\n    1,\n    # last\n]\nb = {\n    # first\n    x = 1,\n    # last\n}\n"))
  (with-temp-buffer
    (insert "a = [\n\n]\n")
    (toml-ts-mode)
    (goto-char 7)
    (indent-according-to-mode)
    (should (= (current-column) toml-ts-mode-indent-offset))))

(ert-deftest toml-ts-mode-indents-after-nested-closings ()
  (pcase-dolist (`(,source ,column)
                 '(("a = [\n  [\n    1,\n  ],|\n]\n" 2)
                   ("a = {\n  b = {\n    c = 1,\n  },|\n}\n" 2)
                   ("a = [\n  [\n    [1],|\n  ],\n]\n" 4)))
    (with-temp-buffer
      (insert source)
      (toml-ts-mode)
      (setq-local indent-tabs-mode nil)
      (electric-indent-local-mode 1)
      (goto-char (point-min))
      (search-forward "|")
      (delete-char -1)
      (call-interactively (key-binding (kbd "RET")))
      (should (= (current-column) column)))))

(ert-deftest toml-ts-mode-preserves-blank-string-content-on-indent ()
  (dolist (source '("a = \"\"\"\n   |\ntext\n\"\"\"\n"
                    "a = '''\n   |\ntext\n'''\n"
                    "a = \"\"\"text\\\n   |\ntext\n\"\"\"\n"))
    (with-temp-buffer
      (insert source)
      (toml-ts-mode)
      (goto-char (point-min))
      (search-forward "|")
      (delete-char -1)
      (indent-according-to-mode)
      (should (= (current-column) 3)))))

(ert-deftest toml-ts-mode-indents-incomplete-containers ()
  (should (equal (toml-ts-mode-test--indent "a=[\n1\n" 2)
                 "a=[\n  1\n"))
  (should (equal (toml-ts-mode-test--indent "a={\nb=\n" 2)
                 "a={\n  b=\n")))

;;;; Updates

(ert-deftest toml-ts-mode-refreshes-edited-owners-without-parser-notifications ()
  (dolist (narrow '(nil t))
    (ert-info ((format "Narrowed: %s" narrow))
      (with-temp-buffer
        (insert "key = \"value\"\n")
        (let ((treesit-font-lock-level 4)) (toml-ts-mode))
        (rename-buffer "toml-ts-mode-test" t)
        (let ((noninteractive nil)) (font-lock-mode 1))
        (toml-ts-mode-test--buffer-state)
        (should (get-text-property 1 'fontified))
        (let ((parser treesit-primary-parser))
          (should-not (treesit-node-check (treesit-parser-root-node parser) 'has-error))
          (dolist (notifier (treesit-parser-notifiers parser))
            (treesit-parser-remove-notifier parser notifier))
          (goto-char (toml-ts-mode-test--position "value"))
          (when narrow
            (narrow-to-region (line-beginning-position) (line-end-position)))
          (insert "x")
          (widen)
          (should (eq parser treesit-primary-parser))
          (should-not (get-text-property 1 'fontified))
          (toml-ts-mode-test--should-match-fresh-buffer 4 t))))))

(ert-deftest toml-ts-mode-reparses-a-bom-moved-to-buffer-start ()
  (dolist (narrow '(nil t))
    (ert-info ((format "Narrowed: %s" narrow))
      (with-temp-buffer
        (insert "key = 1\n\ufeff# note\n")
        (let ((treesit-font-lock-level 4)) (toml-ts-mode))
        (toml-ts-mode-test--buffer-state)
        (let ((parser treesit-primary-parser))
          (when narrow (narrow-to-region 1 10))
          (delete-region 1 9)
          (widen)
          (should-not (eq parser treesit-primary-parser))
          (should (equal (treesit-parser-list) (list treesit-primary-parser)))
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         "\ufeff# note\n"))
          (toml-ts-mode-test--buffer-state)
          (should-not (get-text-property 1 'face))
          (should (eq (toml-ts-mode-test--face "note") 'font-lock-comment-face))
          (toml-ts-mode-test--should-match-fresh-buffer 4 t))))))

(ert-deftest toml-ts-mode-refreshes-string-syntax-after-issue-repair ()
  (pcase-dolist (`(,delimiter ,invalid ,valid)
                 '(("\"" "\\q" "\\n")
                   ("'" "\0" "x")
                   ("\"\"\"" "\n\\q" "\n\\n")
                   ("'''" "\n\0" "\nx")))
    (ert-info ((format "%S / %S" delimiter invalid))
      (with-temp-buffer
        (insert "key = " delimiter "text" invalid delimiter "\n")
        (let ((treesit-font-lock-level 4)) (toml-ts-mode))
        (pcase-dolist (`(,old ,new ,syntax)
                       (list (list invalid invalid 1)
                             (list invalid valid 15)
                             (list valid invalid 1)))
          (toml-ts-mode-test--buffer-state)
          (goto-char (toml-ts-mode-test--position old))
          (delete-char (length old))
          (insert new)
          (syntax-propertize (point-max))
          (should (= (syntax-class (syntax-after 7)) syntax))
          (should (= (syntax-class (syntax-after (- (point-max) 2))) syntax))
          (toml-ts-mode-test--should-match-fresh-buffer 4))))))

(ert-deftest toml-ts-mode-updates-like-fresh-buffer ()
  (pcase-dolist (`(,source ,old ,new ,fragment ,face)
                 '(("a = [1, 2] # text\n" "[1, 2]" "\"[1, 2]\"" "1" font-lock-string-face)
                   ("[[items]]\n" "]]" "]" "items" font-lock-property-name-face)
                   ("[[items]]\n" "[[items]]" "[items]" "[" font-lock-bracket-face)
                   ("a = \"x\"\n" "\"x\"" "\"\"\"x\n# text\n\"\"\"" "text" font-lock-string-face)
                   ("a = \"\"\"x\n# text\n\"\"\"\n" "\"\"\"x\n# text\n\"\"\"" "\"x\"" "x" font-lock-string-face)
                   ("a = \"\"\"x\ntext\n\"\"\"\n" "text\n\"\"\"" "text" "a" font-lock-property-name-face)))
    (ert-info ((format "%S: %S -> %S" source old new))
      (with-temp-buffer
        (insert source)
        (let ((treesit-font-lock-level 4)) (toml-ts-mode))
        (toml-ts-mode-test--buffer-state)
        (goto-char (toml-ts-mode-test--position old))
        (delete-char (length old))
        (insert new)
        (font-lock-ensure)
        (should (eq (toml-ts-mode-test--face fragment) face))
        (toml-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest toml-ts-mode-preserves-syntax-when-narrowed ()
  (with-temp-buffer
    (insert "a = \"\"\"\n# string\n\"\"\"\n# comment\n")
    (toml-ts-mode)
    (goto-char (point-min))
    (forward-line 1)
    (let ((begin (point)))
      (forward-line 1)
      (save-restriction
        (narrow-to-region begin (point))
        (syntax-propertize (point-max))
        (should-not (nth 4 (syntax-ppss (+ begin 3))))))
    (syntax-propertize (point-max))
    (goto-char (point-min))
    (search-forward "comment")
    (should (nth 4 (syntax-ppss (point))))))

(provide 'toml-ts-mode-test)

;;; toml-ts-mode-test.el ends here
