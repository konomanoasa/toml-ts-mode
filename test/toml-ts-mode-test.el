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
(require 'loaddefs-gen)
(require 'newcomment)
(require 'toml-ts-mode)

(dolist (language '(toml))
  (unless (treesit-ready-p language t)
    (error "The %s grammar is required to run the tests" language)))

;;;; Helpers

(defun toml-ts-mode-test--position (fragment &optional line)
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
  (get-text-property (+ (toml-ts-mode-test--position fragment line)
                        (or offset 0)) 'face))

(defun toml-ts-mode-test--indent (source offset)
  (with-temp-buffer
    (insert source)
    (toml-ts-mode)
    (setq-local toml-ts-mode-indent-offset offset)
    (setq-local indent-tabs-mode nil)
    (indent-region (point-min) (point-max))
    (let ((result (buffer-substring-no-properties (point-min) (point-max))))
      (indent-region (point-min) (point-max))
      (should (equal result (buffer-substring-no-properties
                             (point-min) (point-max))))
      result)))

(defun toml-ts-mode-test--buffer-state ()
  (font-lock-ensure)
  (syntax-propertize (point-max))
  (let (state)
    (dotimes (offset (- (point-max) (point-min)))
      (let ((position (+ (point-min) offset)))
        (push (list (get-text-property position 'face) (syntax-after position)) state)))
    (nreverse state)))

(defun toml-ts-mode-test--should-match-fresh-buffer (level)
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (toml-ts-mode-test--buffer-state))
        (file buffer-file-name))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level level)) (toml-ts-mode))
      (should (equal state (toml-ts-mode-test--buffer-state))))))

;;;; Grammar

(ert-deftest toml-ts-mode-respects-grammar-sources ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)) received)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (language)
                  (setq received (assq language treesit-language-source-alist))
                  t))
          (dolist (source toml-ts-mode--grammar-sources)
            (let* ((language (car source))
                   (custom (list language "/local/grammar" :revision "custom")))
              (dolist (configured (list nil (list custom)))
                (let ((treesit-language-source-alist configured))
                  (should (toml-ts-mode--ensure-grammar language))
                  (should (equal received (if configured custom source)))
                  (should (eq treesit-language-source-alist configured)))))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest toml-ts-mode-reports-unavailable-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)))
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed (lambda (_language) nil))
          (with-temp-buffer
            (let ((buffer-file-name nil))
              (should-error (toml-ts-mode) :type 'user-error)
              (should-not (treesit-parser-list)))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest toml-ts-mode-starts-and-reuses-parser ()
  (with-temp-buffer
    (insert "a = 1\n")
    (toml-ts-mode)
    (should (eq major-mode 'toml-ts-mode))
    (should (eq (treesit-parser-language treesit-primary-parser) 'toml))
    (should (equal (treesit-node-type (treesit-parser-root-node treesit-primary-parser))
                   "toml"))
    (toml-ts-mode)
    (should (equal (treesit-parser-list) (list treesit-primary-parser)))))

;;;; Mode Selection

(ert-deftest toml-ts-mode-selects-files ()
  (dolist (filename '("/tmp/config.toml" "/tmp/Cargo.lock"))
    (with-temp-buffer
      (setq buffer-file-name filename)
      (set-auto-mode)
      (should (eq major-mode 'toml-ts-mode))))
  (dolist (filename '("/tmp/OtherCargo.lock" "/tmp/Cargo.locked"))
    (with-temp-buffer
      (setq buffer-file-name filename)
      (set-auto-mode)
      (should-not (eq major-mode 'toml-ts-mode)))))

(ert-deftest toml-ts-mode-generates-autoloads ()
  (let ((output (make-temp-file "toml-ts-mode-loaddefs-"))
        (directory (file-name-directory (locate-library "toml-ts-mode"))))
    (unwind-protect
        (progn
          (loaddefs-generate directory output nil nil nil t)
          (with-temp-buffer
            (insert-file-contents output)
            (dolist (form '("(autoload 'toml-ts-mode" "(add-to-list 'auto-mode-alist"))
              (goto-char (point-min))
              (should (search-forward form nil t)))))
      (delete-file output))))

;;;; Syntax

(ert-deftest toml-ts-mode-classifies-delimiters ()
  (with-temp-buffer
    (insert "[[items]]\nx = [1, {a = 2}]\ns = \"[()] # text\"\n")
    (toml-ts-mode)
    (syntax-propertize (point-max))
    (should (= 1 (car (syntax-ppss 3))))
    (should (= 0 (car (syntax-ppss 10))))
    (should (= 10 (scan-sexps 1 1)))
    (goto-char (scan-sexps 10 -1))
    (backward-prefix-chars)
    (should (= 1 (point)))
    (should (equal (mapcar (lambda (pos) (syntax-class (syntax-after pos)))
                           '(1 2 8 9))
                   '(4 4 5 5)))
    (goto-char 1)
    (should (equal (funcall show-paren-data-function) '(1 3 8 10)))
    (goto-char 10)
    (should (equal (funcall show-paren-data-function) '(8 10 1 3)))
    (goto-char (point-min))
    (search-forward "text")
    (should (nth 3 (syntax-ppss (point))))
    (should-not (nth 4 (syntax-ppss (point))))))

(ert-deftest toml-ts-mode-comments-and-uncomments ()
  (with-temp-buffer
    (insert "s = \"# string\" # comment\n")
    (toml-ts-mode)
    (syntax-propertize (point-max))
    (should-not (nth 4 (syntax-ppss 10)))
    (should (nth 4 (syntax-ppss 20)))
    (comment-region (point-min) (point-max))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "# s = \"# string\" # comment\n"))
    (uncomment-region (point-min) (point-max))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "s = \"# string\" # comment\n"))))

;;;; Font Lock

(ert-deftest toml-ts-mode-fontifies-by-level ()
  (dolist (level '(1 2 3 4))
    (with-temp-buffer
      (insert "key = [true, 1, \"s\\n\"] # note\n")
      (let ((treesit-font-lock-level level)) (toml-ts-mode))
      (font-lock-ensure)
      (pcase-dolist (`(,fragment ,minimum ,face)
                     '(("note" 1 font-lock-comment-face) ("key" 2 font-lock-property-name-face)
                       ("s\\n" 2 font-lock-string-face) ("true" 3 font-lock-constant-face)
                       ("1" 3 font-lock-number-face) ("\\n" 3 font-lock-escape-face)
                       ("[" 4 font-lock-bracket-face)))
        (ert-info ((format "Level %s: %S" level fragment))
          (should (eq (toml-ts-mode-test--face fragment) (and (>= level minimum) face))))))))

(ert-deftest toml-ts-mode-fontifies-language-syntax ()
  (with-temp-buffer
    (insert "# note\n[server]\n\"key\".name = [true, 12, -3.4e+5, 0xff, inf]\n"
            "s = \"a\\n\\u0041\"\nl = 'raw'\nm = \"\"\"body\"\"\"\n"
            "n = '''text'''\nd = 2026-09-21T12:34:56.123+09:00\n"
            "z = 2026-09-21t12:34z\nx = { f = false }\n")
    (let ((treesit-font-lock-level 4)) (toml-ts-mode))
    (font-lock-ensure)
    (dolist (case '(("# note" 0 font-lock-comment-face)
                    ("server" 0 font-lock-property-name-face)
                    ("key" 0 font-lock-property-name-face)
                    ("name" 0 font-lock-property-name-face)
                    (".name" 0 font-lock-punctuation-face)
                    ("=" 0 font-lock-operator-face)
                    ("[server]" 0 font-lock-bracket-face)
                    ("true" 0 font-lock-constant-face)
                    ("12" 0 font-lock-number-face)
                    ("-3.4e+5" 0 font-lock-number-face)
                    ("-3.4e+5" 1 font-lock-number-face)
                    ("-3.4e+5" 2 font-lock-number-face)
                    ("-3.4e+5" 4 font-lock-number-face)
                    ("0xff" 0 font-lock-number-face)
                    ("0xff" 2 font-lock-number-face)
                    ("inf" 0 font-lock-number-face)
                    ("a\\n" 0 font-lock-string-face)
                    ("\\n" 0 font-lock-escape-face)
                    ("\\n" 1 font-lock-escape-face)
                    ("\\u0041" 1 font-lock-escape-face)
                    ("\\u0041" 2 font-lock-escape-face)
                    ("'raw'" 0 font-lock-string-face)
                    ("raw" 0 font-lock-string-face)
                    ("body" 0 font-lock-string-face)
                    ("text" 0 font-lock-string-face)
                    ("2026" 0 font-lock-constant-face)
                    ("09-21T" 0 font-lock-constant-face)
                    ("T12" 0 font-lock-constant-face)
                    ("56.123" 2 font-lock-constant-face)
                    ("56.123" 3 font-lock-constant-face)
                    ("+09:00" 0 font-lock-constant-face)
                    ("34z" 2 font-lock-constant-face)
                    ("{ f" 0 font-lock-bracket-face)
                    ("false" 0 font-lock-constant-face)))
      (should (equal (list (car case) (cadr case)
                           (toml-ts-mode-test--face (car case) (cadr case)))
                     case)))))

(ert-deftest toml-ts-mode-fontifies-every-content-and-digit-leaf ()
  (with-temp-buffer
    (insert "a = 1_000\nb = 1.2_3\nc = 0o77\nd = 0b10\n'key' = 'hello'\n"
            "h = 0xff\ns = \"\\u0041\"\nt = 12:34:56.123\n")
    (let ((treesit-font-lock-level 4)) (toml-ts-mode))
    (font-lock-ensure)
    (dolist (case '(("1_000" font-lock-number-face)
                    ("1.2_3" font-lock-number-face)
                    ("0o77" font-lock-number-face)
                    ("0b10" font-lock-number-face)
                    ("key" font-lock-property-name-face)
                    ("hello" font-lock-string-face)
                    ("0xff" font-lock-number-face)
                    ("\\u0041" font-lock-escape-face)
                    ("12:34:56.123" font-lock-constant-face)))
      (dotimes (offset (length (car case)))
        (should (eq (toml-ts-mode-test--face (car case) offset) (cadr case)))))))

;;;; Navigation

(ert-deftest toml-ts-mode-navigates-structures ()
  (with-temp-buffer
    (insert "a = [1, 2]\nb = \"text\"\n")
    (toml-ts-mode)
    (goto-char 5)
    (forward-sexp)
    (should (= 11 (point)))
    (backward-sexp)
    (should (= 5 (point)))
    (should-not (assoc 'defun (cdr (assq 'toml treesit-thing-settings))))))

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

;;;; Updates

(ert-deftest toml-ts-mode-updates-like-fresh-buffer ()
  (pcase-dolist (`(,source ,old ,new ,fragment ,face)
                 '(("a = [1, 2]\n" "[1, 2]" "\"[1, 2]\"" "1" font-lock-string-face)
                   ("[[items]]\n" "[[items]]" "[items]" "[" font-lock-bracket-face)
                   ("a = \"unfinished\n" "unfinished" "finished\"" "finished" font-lock-string-face)))
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

(ert-deftest toml-ts-mode-edits-match-fresh-font-lock-and-syntax ()
  (dolist (case '(("a = [1, 2] # text\n" "[1, 2]" "\"[1, 2]\"")
                  ("[[items]]\n" "[[items]]" "[items]")
                  ("a = \"x\"\n" "\"x\"" "\"\"\"x\n# text\n\"\"\"")
                  ("a = \"\"\"x\n# text\n\"\"\"\n" "\"\"\"x\n# text\n\"\"\"" "\"x\"")
                  ("a = \"unfinished\n" "unfinished" "finished\"")))
    (with-temp-buffer
      (insert (car case))
      (let ((treesit-font-lock-level 4)) (toml-ts-mode))
      (toml-ts-mode-test--buffer-state)
      (goto-char (point-min))
      (search-forward (cadr case))
      (replace-match (nth 2 case) t t)
      (let ((state (toml-ts-mode-test--buffer-state))
            (source (buffer-substring-no-properties (point-min) (point-max))))
        (with-temp-buffer
          (insert source)
          (let ((treesit-font-lock-level 4)) (toml-ts-mode))
          (should (equal state (toml-ts-mode-test--buffer-state))))))))

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
