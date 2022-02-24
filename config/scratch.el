;;; scratch.el --- Scratch buffers. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(persistent-scratch))

(define-key global-map (kbd "C-c sb") #'ec-scratch-buffer)
(define-key global-map (kbd "C-c sB") #'ec-global-scratch-buffer)

(defun ec-global-scratch-buffer ()
  "Switch to the `*scratch*' buffer."
  (interactive)
  (let* ((name "*scratch*")
         (buffer (get-buffer name)))
    (switch-to-buffer (get-buffer-create name))
    (funcall initial-major-mode)
    (when (and initial-scratch-message (not buffer))
      (insert initial-scratch-message))))

(defun ec-scratch-buffer ()
  "Switch to the scratch buffer for the current project and major mode."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (mode (cond ((eq major-mode 'exwm-mode) 'org-mode)
                     ((eq major-mode 'emacs-lisp-mode) 'lisp-interaction-mode)
                     ((not (or (derived-mode-p 'text-mode) (derived-mode-p 'prog-mode))) 'text-mode)
                     (t major-mode)))
         (mode-name (replace-regexp-in-string "-mode$" "" (symbol-name mode)))
         (name (project-prefixed-buffer-name (concat mode-name "-scratch"))))
    (switch-to-buffer (get-buffer-create name))
    (funcall mode)
    (when (fboundp 'evil-insert-state)
      (evil-insert-state))))

;; Persistent scratch buffers.
(setq persistent-scratch-scratch-buffer-p-function #'ec-scratch-buffer-p)

(defun ec-scratch-buffer-p (&optional buffer)
  "Return non-nil if the BUFFER (defaults to current buffer) is a scratch buffer."
  (string-match-p "^\\*.*scratch\\*$" (buffer-name buffer)))

(when (fboundp 'persistent-scratch-setup-default)
  (add-hook 'emacs-startup-hook #'persistent-scratch-setup-default))

;;; scratch.el ends here
