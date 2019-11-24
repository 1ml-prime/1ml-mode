;;; 1ml-mode.el --- Mode for 1ML -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup 1ml nil
  "Major mode for editing 1ML files."
  :group 'languages)

(defcustom 1ml-indentation-offset 2
  "Basic offset for indentation in 1ML."
  :type 'integer
  :group '1ml)

(defcustom 1ml-key-bindings
  '()
  "Key bindings for the 1ML mode.

The key specifications must be in a format accepted by `define-key'.  Hint: You
might want to type `M-x `describe-function' 1ml <TAB>' to see the available
commands."
  :type '(repeat (cons :tag "Key Binding"
                       (string :tag "Key")
                       (function :tag "Command")))
  :group '1ml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces

(defgroup 1ml-faces nil
  "Font lock faces for the 1ML mode."
  :group '1ml)

(defmacro deffacevar (name &rest body)
  "Define `NAME' as both a face with the given `BODY' and a var."
  `(progn
     (defface ,name ,@body)
     (defvar ,name ',name)))

(deffacevar 1ml-braced-face
  '((t (:foreground "SlateGray3")))
  "Font Lock mode face used to highlight 1ML braced expression."
  :group '1ml-faces)

(deffacevar 1ml-parenthesized-face
  '((t (:foreground "Gray")))
  "Font Lock mode face used to highlight 1ML parenthesized expressions."
  :group '1ml-faces)

(deffacevar 1ml-typing-face
  '((t (:foreground "YellowGreen")))
  "Font Lock mode face used to highlight 1ML typing constructs."
  :group '1ml-faces)

(deffacevar 1ml-conditional-face
  '((t (:foreground "GoldenRod1")))
  "Font Lock mode face used to highlight 1ML conditional constructs."
  :group '1ml-faces)

(deffacevar 1ml-functional-face
  '((t (:foreground "MediumSpringGreen")))
  "Font Lock mode face used to highlight 1ML functional constructs."
  :group '1ml-faces)

(deffacevar 1ml-pattern-face
  '((t (:foreground "DeepPink")))
  "Font Lock mode face used to highlight 1ML pattern constructs."
  :group '1ml-faces)

(deffacevar 1ml-definition-face
  '((t (:bold t :foreground "HotPink")))
  "Font Lock mode face used to highlight 1ML definition constructs."
  :group '1ml-faces)

(deffacevar 1ml-symbolic-face
  '((t (:bold t :foreground "Gold3")))
  "Font Lock mode face used to highlight 1ML symbolic constructs."
  :group '1ml-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax and highlighting

(defconst 1ml-scoping-kws '("do" "end" "in" "let" "local" "type"))
(defconst 1ml-definition-kws '("include"))
(defconst 1ml-functional-kws '("fun" "rec"))
(defconst 1ml-typing-kws '("unwrap" "with" "wrap"))
(defconst 1ml-primitive-kws '("primitive"))
(defconst 1ml-conditional-kws '("and" "else" "if" "or" "then"))
(defconst 1ml-pattern-kws '("_" "as"))

(defconst 1ml-definition-skws '("="))
(defconst 1ml-functional-skws '("=>" "@"))
(defconst 1ml-typing-skws '(":" ":>" "->"))

(defconst 1ml-symbolic-chars "~!#$%&*+/:<=>?@\\\\`|^-")
(defconst 1ml-symbolic-char-re (concat "[" 1ml-symbolic-chars "]"))
(defconst 1ml-not-symbolic-char-re (concat "[^" 1ml-symbolic-chars "]"))

(defconst 1ml-mode-syntax-table
  (let ((table (make-syntax-table)))
    (mapc (function (lambda (c) (modify-syntax-entry c "_" table)))
          1ml-symbolic-chars)
    ;; Note that above modifies the entry of `?\\', which we redefine below.
    (mapc (function
           (lambda (char-flags)
             (modify-syntax-entry (car char-flags) (cdr char-flags) table)))
          '((?\; . ". 12")
            (?\n . ">")
            (?\\ . "\\_")
            (?'  . "w")
            (?_  . "w")))
    table)
  "Syntax table for 1ML mode.")

(defun 1ml-kws-match (kws)
  "Return first match of `KWS'."
  (concat "\\<" (regexp-opt kws) "\\>"))

(defun 1ml-first-match (&rest regexps)
  "Return only first match group of `REGEXPS'."
  (lexical-let ((regexp (apply 'concat regexps)))
    (lambda (limit)
      (when (save-excursion (re-search-forward regexp limit t))
        (let ((data (match-data)))
          (set-match-data (list (nth 2 data) (nth 3 data)))
          (goto-char (nth 3 data))
          t)))))

(defun 1ml-skws-match (skws)
  "Return first match of `SKWS'."
  (1ml-first-match 1ml-not-symbolic-char-re
                   "\\(" (regexp-opt skws) "\\)"
                   1ml-not-symbolic-char-re))

(defvar 1ml-font-lock-table nil)

(defun 1ml-build-font-lock-table ()
  "Builds the font-lock table for the 1ML mode."
  (setq
   1ml-font-lock-table
   `(;; keywords
     (,(1ml-kws-match 1ml-scoping-kws) . font-lock-keyword-face)
     (,(1ml-kws-match 1ml-definition-kws) . 1ml-definition-face)
     (,(1ml-kws-match 1ml-functional-kws) . 1ml-functional-face)
     (,(1ml-kws-match 1ml-typing-kws) . 1ml-typing-face)
     (,(1ml-kws-match 1ml-primitive-kws) . font-lock-builtin-face)
     (,(1ml-kws-match 1ml-conditional-kws) . 1ml-conditional-face)
     (,(1ml-kws-match 1ml-pattern-kws) . 1ml-pattern-face)

     ;; symbolic keywords
     (,(1ml-skws-match 1ml-definition-skws) . 1ml-definition-face)
     (,(1ml-skws-match 1ml-functional-skws) . 1ml-functional-face)
     (,(1ml-skws-match 1ml-typing-skws) . 1ml-typing-face)

     ;; symbolic identifiers
     (,(concat 1ml-symbolic-char-re "+") . 1ml-symbolic-face)

     ;; constants
     ("\\<\\([1-9][0-9]*\\|0\\|true\\|false\\)\\>" . font-lock-constant-face)

     ;; character literal
     ("\\<'\\(?:[^\\\n]\\|[\\].\\)'" . font-lock-string-face)

     ;; tick
     (,(1ml-first-match "\\<\\([']\\)\\(?:\\w+[^']\\|(\\)") . 1ml-typing-face)

     ;; member access
     ("[.]" . 1ml-parenthesized-face)

     ;; parenthesized
     ("[(,)]" . 1ml-parenthesized-face)

     ;; braced
     ("[{;}]+" . 1ml-braced-face))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Map

(defvar 1ml-mode-map (make-sparse-keymap)
  "Keymap for 1ML mode.  This variable is updated by `1ml-update'.")

(defun 1ml-build-mode-map ()
  "Builds the key map for ML Basis mode."
  (let ((result (make-sparse-keymap)))
    (mapc (function
           (lambda (key-command)
             (define-key result (read (car key-command)) (cdr key-command))))
          1ml-key-bindings)
    (setq 1ml-mode-map result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define mode

(defvar 1ml-mode-hook nil
  "Hook run when entering 1ML mode.")

(define-derived-mode 1ml-mode fundamental-mode "1ML"
  "Major mode for editing 1ML files.

See the customization group `1ml'."
  :group '1ml
  (set (make-local-variable 'font-lock-defaults) '(1ml-font-lock-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finalization

(defun 1ml-update ()
  "Update data based on customization variables."
  (interactive)
  ;; Warning: order dependencies
  (1ml-build-font-lock-table)
  (1ml-build-mode-map))

;; We are finally ready to update everything the first time.
(1ml-update)

(provide '1ml-mode)
;;; 1ml-mode.el ends here
