;;; doom-override-theme.el --- Doom theme overrides. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(ec-def-theme
 doom-override "Doom theme overrides"

 (org-habit-ready-face        :background 'unspecified :foreground (doom-color 'blue)   :underline t)
 (org-habit-ready-future-face :background 'unspecified :foreground (doom-color 'blue)   :underline t)
 (org-habit-alert-face        :background 'unspecified :foreground (doom-color 'yellow) :underline t)
 (org-habit-alert-future-face :background 'unspecified :foreground (doom-color 'yellow) :underline t)
 (org-habit-overdue-face      :background 'unspecified :foreground (doom-color 'red)    :underline t)
 (org-habit-clear-face        :background 'unspecified :foreground (doom-color 'orange) :underline t)
 (org-habit-clear-future-face :background 'unspecified :foreground (doom-color 'orange) :underline t))

;;; doom-override-theme.el ends here
