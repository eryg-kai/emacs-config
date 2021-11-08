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

;; Selected window.
(defvar ec-selected-window nil "Currently selected window.")

(defun ec--set-selected-window (&rest args)
  "Set `ec-selected-window' ignoring ARGS."
  (unless (minibuffer-window-active-p (selected-window))
    (setq ec-selected-window (selected-window))))

(add-hook 'buffer-list-update-hook #'ec--set-selected-window)

(defun ec-is-active-window ()
  "Return t if the selected window is active."
  (eq ec-selected-window (selected-window)))

;;; windows.el ends here
