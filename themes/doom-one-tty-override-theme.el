;;; doom-one-tty-override-theme.el --- Doom One TTY theme overrides. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(ec-def-theme
 doom-one-tty-override "Doom One TTY theme overrides"

 (default :background "black")

 (hl-line :background (doom-color 'base3 '256))
 (region  :background (doom-color 'base3 '256))

 (fill-column-indicator :foreground (doom-color 'base3 '256)))

;;; doom-one-tty-override-theme.el ends here
