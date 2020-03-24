# 1ML mode for Emacs [![Gitter](https://badges.gitter.im/1ml-prime/community.svg)](https://gitter.im/1ml-prime/community)

This is the beginnings of an [Emacs](https://www.gnu.org/software/emacs/) mode
for [1ML](https://people.mpi-sws.org/~rossberg/1ml/).

To take this into use, you could e.g. specify

```elisp
(use-package 1ml-mode
  :load-path "path-to-1ml-mode-src-directory"
  :mode ("\\.1mls?\\'" . 1ml-mode)
  :init
  (add-hook
   '1ml-mode-hook (function (lambda () (setq-local flycheck-checker '1ml)))))
```

using the wonderful [use-package](https://jwiegley.github.io/use-package/).

For the [Flycheck](https://www.flycheck.org/en/latest/) support you will also
need to have the `1ml` command in `PATH`.
