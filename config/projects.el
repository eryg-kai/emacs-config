;;; projects.el --- Project management. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq projectile-git-submodule-command nil)

;; Keymaps can't be autoloaded so here's a workaround. Loads projectile, rebinds
;; to the keymap, then triggers the keybinding again.
(defun ec-projectile ()
  "Load projectile."
  (interactive)
  (projectile-mode)
  (define-key global-map (kbd "C-c p") #'projectile-command-map)
  (setq unread-command-events (mapcar (lambda (ev) (cons t ev))
                                      (listify-key-sequence (kbd "C-c p")))))

(define-key global-map (kbd "C-c p") #'ec-projectile)

(defun ec--ignore-project (path)
  "Return non-nil if PATH should be ignored."
  ;; Any git repository not inside an archive or config is OK.
  (or (string-match-p "/\\(archive\\|\\.config\\)" path)
      (not (file-exists-p (expand-file-name ".git" path)))))

(defun ec--set-repositories ()
  "Set magit repository directories from known projectile projects."
  (let ((project-dirs (bound-and-true-p projectile-known-projects)))
    (setq magit-repository-directories
          (mapcar (lambda (path) `(,path . 0)) project-dirs))))

;; Do this every time to prevent issues with removing a directory then trying
;; to search again.
(defun ec--set-search-path ()
  "Set the projectile search path."
  (setq projectile-project-search-path
        (append (list (expand-file-name "~"))
                (cl-remove-if-not #'file-directory-p
                                  (directory-files "~" t "^[^.]")))
        projectile-ignored-project-function #'ec--ignore-project))

(advice-add 'projectile-discover-projects-in-search-path
            :before #'ec--set-search-path)
(advice-add 'projectile-discover-projects-in-search-path
            :after #'ec--set-repositories)

;;; projects.el ends here
