;;; doom-override-theme.el --- Doom theme overrides. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(ec-def-theme
 doom-override "Doom theme overrides"

 (mouse :background (doom-color 'red))

 (region              :background (doom-blend (doom-color 'blue) (doom-color 'bg) 0.05))
 (highlight           :background (doom-blend (doom-color 'red) (doom-color 'bg) 0.2))

 (evil-normal-state   :foreground (doom-color 'yellow))
 (evil-insert-state   :foreground (doom-color 'green))
 (evil-emacs-state    :foreground (doom-color 'blue))
 (evil-replace-state  :foreground (doom-color 'red))
 (evil-visual-state   :foreground (doom-color 'base7))
 (evil-motion-state   :foreground (doom-color 'magenta))
 (evil-operator-state :foreground (doom-color 'violet))

 (org-habit-ready-face        :background 'unspecified :foreground (doom-color 'blue)   :underline t)
 (org-habit-ready-future-face :background 'unspecified :foreground (doom-color 'blue)   :underline t)
 (org-habit-alert-face        :background 'unspecified :foreground (doom-color 'yellow) :underline t)
 (org-habit-alert-future-face :background 'unspecified :foreground (doom-color 'yellow) :underline t)
 (org-habit-overdue-face      :background 'unspecified :foreground (doom-color 'red)    :underline t)
 (org-habit-clear-face        :background 'unspecified :foreground (doom-color 'orange) :underline t)
 (org-habit-clear-future-face :background 'unspecified :foreground (doom-color 'orange) :underline t))

;;; doom-override-theme.el ends here
