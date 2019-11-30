;;; flycheck-1ml.el --- Flycheck for 1ML -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'flycheck)

(flycheck-define-checker 1ml
  "A 1ML checker using the 1ML compiler.

See URL `https://github.com/1ml-prime/1ml-mode'."
  :command ("1ml-check" source-original source)
  :error-patterns
  ((warning line-start (file-name) ":" line "." column "-" (+ num) "." (+ num) ": warning: " (message) line-end)
   (error   line-start (file-name) ":" line "." column "-" (+ num) "." (+ num) ": " (message) line-end))
  :modes 1ml-mode)

(provide 'flycheck-1ml)
;;; flycheck-1ml.el ends here
