;;; windows.el --- Window management. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(winum))

;; Winner.
(add-hook 'emacs-startup-hook #'winner-mode)

;; Winum.
(setq winum-auto-setup-mode-line nil)

(when (fboundp 'winum-mode)
  (add-hook 'emacs-startup-hook #'winum-mode))

;;; windows.el ends here
