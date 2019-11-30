# 1ML mode for Emacs

This is the beginnings of an [Emacs](https://www.gnu.org/software/emacs/) mode
for [1ML](https://people.mpi-sws.org/~rossberg/1ml/).

To take this into use, you could e.g. specify

```elisp
(use-package 1ml-mode
  :load-path "path-to-1ml-mode-src-directory"
  :mode ("\\.1ml\\'" . 1ml-mode)
  :init (autoload '1ml-mode "1ml-mode"))
```

using the wonderful [use-package](https://jwiegley.github.io/use-package/).
