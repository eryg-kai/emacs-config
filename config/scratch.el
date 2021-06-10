;;; scratch.el --- Scratch buffers. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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

(defun ec-scratch-buffer (&optional project)
  "Switch to the scratch buffer for PROJECT and the major mode."
  (interactive)
  (let* ((project (or project (and (not (eq major-mode 'exwm-mode))
                                   (fboundp 'projectile-project-name)
                                   (projectile-project-name))))
         (mode (cond ((eq major-mode 'exwm-mode) 'org-mode)
                     ((eq major-mode 'emacs-lisp-mode) 'lisp-interaction-mode)
                     ((not (or (derived-mode-p 'text-mode) (derived-mode-p 'prog-mode))) 'text-mode)
                     (t major-mode)))
         (mode-name (replace-regexp-in-string "-mode$" "" (symbol-name mode)))
         (name (if (string= project mode-name)
                   (format "*scratch [%s]*" project)
                 (format
                  "*scratch %s%s*"
                  mode-name
                  (if (and project (not (string= "-" project)))
                      (concat " " project)
                    ""))))
         (buffer (get-buffer name)))
    (switch-to-buffer (get-buffer-create name))
    (funcall mode)
    (unless buffer
      (let* ((values `(("project" ,(or (unless (string= "-" project) project)
                                       "none"))
                       ("mode" ,mode)))
             (largest (seq-reduce (lambda (c v) (max c (length (car v)))) values 0)))
        (insert
         (concat
          (mapconcat
           (lambda (s)
             (format "%s%s:%s %s%s"
                     (or comment-start "")
                     (car s)
                     (make-string (- largest (length (car s))) ? )
                     (cadr s)
                     (or comment-end "")))
           values
           "\n")
          "\n\n"))))))

;; Persistent scratch buffers.
(setq persistent-scratch-scratch-buffer-p-function #'ec-scratch-buffer-p)

(defun ec-scratch-buffer-p (&optional buffer)
  "Return non-nil if the BUFFER (defaults to current buffer) is a scratch buffer."
  (string-match-p "^\\*scratch.*\\*$" (buffer-name buffer)))

(add-hook 'emacs-startup-hook #'persistent-scratch-setup-default)

;;; scratch.el ends here
