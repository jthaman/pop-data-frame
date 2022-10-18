# `tabulate-data-frame`

Move data.frames from your R session into new buffer for inspection.

## Status

It sort of works.

## Requirements

- A recent version of Emacs
- Emacs Speaks Statistics

## Installation

Install from Github via [straight.el](https://github.com/radian-software/straight.el) until the software is available on MELPA or ELPA.

```emacs-lisp
(use-package tabulate-data-frame
  :defer t
  :after ess-r-mode
  :straight (:host github :repo "jthaman/tabulate-data-frame"))
```

![](pic.png)
