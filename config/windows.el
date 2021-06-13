;;; windows.el --- Window management. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(winum))

;; Winner.
(with-eval-after-load 'winner
  (define-key winner-mode-map (kbd "C-c wu") #'winner-undo)
  (define-key winner-mode-map (kbd "C-c wr") #'winner-redo))

(add-hook 'emacs-startup-hook #'winner-mode)

;; Winum.
(setq winum-auto-setup-mode-line nil)

(add-hook 'emacs-startup-hook #'winum-mode)

;;; windows.el ends here
