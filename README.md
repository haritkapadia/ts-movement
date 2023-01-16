# ts-movement-mode

This Emacs mode lets you traverse the Tree Sitter syntax tree.

This package uses the built-in `treesit` package available starting from Emacs 29.

This package supports [https://github.com/magnars/multiple-cursors.el](multiple-cursors).

## Installation

It is recommended to use `use-package`.

```lisp
(use-package ts-movement
  :hook
  (bash-ts-mode-hook . ts-movement-mode)
  (c++-ts-mode-hook . ts-movement-mode)
  (c-ts-mode-hook . ts-movement-mode)
  (cmake-ts-mode-hook . ts-movement-mode)
  (csharp-ts-mode-hook . ts-movement-mode)
  (css-ts-mode-hook . ts-movement-mode)
  (dockerfile-ts-mode-hook . ts-movement-mode)
  (go-mod-ts-mode-hook . ts-movement-mode)
  (go-ts-mode-hook . ts-movement-mode)
  (java-ts-mode-hook . ts-movement-mode)
  (js-ts-mode-hook . ts-movement-mode)
  (json-ts-mode-hook . ts-movement-mode)
  (python-ts-mode-hook . ts-movement-mode)
  (ruby-ts-mode-hook . ts-movement-mode)
  (rust-ts-mode-hook . ts-movement-mode)
  (toml-ts-mode-hook . ts-movement-mode)
  (tsx-ts-mode-hook . ts-movement-mode)
  (typescript-ts-mode-hook . ts-movement-mode)
  (yaml-ts-mode-hook . ts-movement-mode))
```

By default, the package uses the `C-c .` binding. If you have [https://github.com/abo-abo/hydra](hydra) installed, `C-c .` will be bound to `tsm/hydra/body`.
