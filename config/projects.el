;;; projects.el --- Project management. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(ripgrep
                                   projectile))
;
(setq projectile-git-submodule-command nil)

;; Keymaps cannot be autoloaded so here is a workaround.  Loads projectile,
;; rebinds to the keymap, then triggers the keybinding again.
(defun ec-projectile ()
  "Load projectile."
  (interactive)
  (projectile-mode)
  (define-key global-map (kbd "C-c p") #'projectile-command-map)
  (setq unread-command-events (mapcar (lambda (ev) (cons t ev))
                                      (listify-key-sequence (kbd "C-c p")))))

(define-key global-map (kbd "C-c p") #'ec-projectile)

(defun ec--set-repositories ()
  "Set magit repository directories from known projectile projects."
  (let ((project-dirs (bound-and-true-p projectile-known-projects)))
    (setq magit-repository-directories
          (mapcar (lambda (path) `(,path . 0)) project-dirs))))

(advice-add 'projectile-discover-projects-in-search-path
            :after #'ec--set-repositories)

;;; projects.el ends here
