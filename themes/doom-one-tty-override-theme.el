;;; doom-one-tty-override-theme.el --- Doom One tty theme overrides. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(ec-def-theme
 doom-one-tty-override "Doom One tty theme overrides"

 (default :background "black")

 (hl-line :background (doom-color 'base3 '256))
 (region  :background (doom-color 'base3 '256))

 (mode-line-inactive  :background 'unspecified)
 (vertical-border     :background 'unspecified)
 (bookmark-face       :background 'unspecified)

 (magit-diff-context  :background 'unspecified)

 (ediff-current-diff-A :background (doom-color 'base2 '256))
 (ediff-fine-diff-A    :background (doom-color 'base4 '256))

 (fill-column-indicator :foreground (doom-color 'base3 '256)))

;;; doom-one-tty-override-theme.el ends here
