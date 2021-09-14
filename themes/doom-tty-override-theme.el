;;; doom-tty-override-theme.el --- Doom TTY theme overrides. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(ec-def-theme
 doom-tty-override "Doom TTY theme overrides"

 (highlight :background (doom-blend (doom-color 'red) (doom-color 'bg) 0.07))

 (mode-line           :background 'unspecified)
 (mode-line-inactive  :background 'unspecified)
 (vertical-border     :background 'unspecified)

 (magit-diff-context  :background 'unspecified)

 (ediff-current-diff-A :background (doom-color 'base2 '256))
 (ediff-fine-diff-A    :background (doom-color 'base4 '256)))

;;; doom-tty-override-theme.el ends here
