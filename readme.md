# `pop-data-frame`

Move data.frames from your R session into Excel/Libreoffice for inspection.

## Requirements

- A recent version of Emacs
- Emacs Speaks Statistics
- Excel or something similar

## Installation

Install from Github via [straight.el](https://github.com/radian-software/straight.el) until the software is available on MELPA or ELPA.

```emacs-lisp
(use-package pop-data-frame
  :defer t
  :after ess-r-mode
  :straight (:host github :repo "jthaman/pop-data-frame"))
```
