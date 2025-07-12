;;; keybindings.el --- Custom keybindings. -*- lexical-binding: t -*-

;;; Commentary:

;; Customization for keybindings.

;;; Code:

(nconc package-selected-packages '(evil
                                   evil-collection
                                   evil-snipe
                                   evil-surround
                                   evil-commentary
                                   evil-exchange
                                   evil-escape
                                   evil-org
                                   evil-args
                                   evil-string-inflection
                                   avy))

(setq evil-want-C-u-scroll t
      ;; Required for evil-collection.
      evil-want-keybinding nil)

(when (fboundp 'evil-mode)
  (add-hook 'emacs-startup-hook #'evil-mode))

;; Gate these behind Evil to ensure they don't load until Evil does.
(with-eval-after-load 'evil
  (evil-collection-init)

  (setq evil-snipe-scope 'whole-visible)
  (evil-snipe-mode)
  (evil-snipe-override-mode)

  (global-evil-surround-mode)

  (evil-commentary-mode)

  (evil-exchange-install)

  (keymap-set evil-inner-text-objects-map "a" 'evil-inner-arg)
  (keymap-set evil-outer-text-objects-map "a" 'evil-outer-arg)

  (setq evil-escape-key-sequence ";z")
  (evil-escape-mode)

  (keymap-set evil-normal-state-map "g s c" #'evil-avy-goto-char-2)
  (keymap-set evil-normal-state-map "g s t" #'evil-avy-goto-char-timer)
  (keymap-set evil-normal-state-map "g s w" #'evil-avy-goto-word-1)

  (keymap-set global-map "s-j" #'evil-window-down)
  (keymap-set global-map "s-k" #'evil-window-up)
  (keymap-set global-map "s-l" #'evil-window-right)
  (keymap-set global-map "s-h" #'evil-window-left)

  (keymap-set global-map "s-J" #'evil-window-decrease-height)
  (keymap-set global-map "s-K" #'evil-window-increase-height)
  (keymap-set global-map "s-L" #'evil-window-increase-width)
  (keymap-set global-map "s-H" #'evil-window-decrease-width))

;; These don't have any autoloads.
(with-eval-after-load 'org
  (require 'evil-org)
  (require 'evil-org-agenda)
  (add-hook 'org-mode-hook #'evil-org-mode)
  (evil-org-agenda-set-keys))

(keymap-set global-map "<XF86MonBrightnessUp>"   #'(lambda () (interactive) (ec-exec "light -A 5")))
(keymap-set global-map "<XF86MonBrightnessDown>" #'(lambda () (interactive) (ec-exec "light -U 5")))

;;; keybindings.el ends here
