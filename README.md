# ts-movement-mode

This Emacs mode lets you traverse the Tree Sitter syntax tree.

![demo.gif](demo.gif)

This package uses the built-in `treesit` package available starting from Emacs 29.

This package supports [multiple-cursors](https://github.com/magnars/multiple-cursors.el).

## Installation

This package is not on any ELPA yet. You must use this repository as a package. You can use the following steps.

1. Create a directory `~/.emacs.d/site-lisp` if it does not already exist.
2. Clone the repository in this directory.
3. Add the following to your configuration file.

```lisp
(use-package ts-movement
  :load-path "site-lisp/ts-movement"
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

By default, the package uses the `C-c .` binding. If you have [hydra](https://github.com/abo-abo/hydra) installed, `C-c .` will be bound to `tsm/hydra/body`.
