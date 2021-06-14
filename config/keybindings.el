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
                                   avy))

(setq evil-want-C-u-scroll t
      evil-want-fine-undo t
      ;; Required for evil-collection.
      evil-want-keybinding nil
      evil-undo-system 'undo-tree)

(when (fboundp 'evil-mode)
  (add-hook 'emacs-startup-hook #'evil-mode))

;; Gate these behind Evil to ensure they don't load until Evil does.
(with-eval-after-load 'evil
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init)

  (setq evil-snipe-scope 'whole-visible)
  (evil-snipe-mode)
  (evil-snipe-override-mode)

  (global-evil-surround-mode)

  (evil-commentary-mode)

  (evil-exchange-install)

  (setq evil-escape-key-sequence ";z")
  (evil-escape-mode)

  (define-key evil-normal-state-map (kbd "gsc") #'evil-avy-goto-char-2)
  (define-key evil-normal-state-map (kbd "gst") #'evil-avy-goto-char-timer)
  (define-key evil-normal-state-map (kbd "gsw") #'evil-avy-goto-word-1))

;; These don't have any autoloads.
(with-eval-after-load 'org
  (require 'evil-org)
  (require 'evil-org-agenda)
  (add-hook 'org-mode-hook #'evil-org-mode)
  (evil-org-agenda-set-keys))

;;; keybindings.el ends here
