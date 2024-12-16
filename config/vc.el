;;; vc.el --- Version control configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(magit
                                   git-link
                                   forge))

;; Ediff.
(setq-default ediff-split-window-function #'split-window-horizontally
              ediff-merge-split-window-function #'split-window-horizontally
              ediff-window-setup-function #'ediff-setup-windows-plain)

;; Magit.
(keymap-set global-map "C-c g g" #'magit-file-dispatch)
(keymap-set global-map "C-c g d" #'magit-list-repositories)

(setq magit-delete-by-moving-to-trash nil
      magit-module-sections-nested nil
      magit-diff-refine-hunk t
      magit-bury-buffer-function #'magit-restore-window-configuration
      magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

(with-eval-after-load 'magit
  (add-to-list 'magit-status-sections-hook 'magit-insert-modules t))

(defun ec--set-repositories ()
  "Set magit repository directories from known projects."
  (setq magit-repository-directories
        (mapcar (lambda (path) `(,path . 0))
                (seq-filter (lambda (path) (not (tramp-tramp-file-p path)))
                            (project-known-project-roots)))))

(advice-add 'magit-list-repositories :before #'ec--set-repositories)

;; Magit repolist.
(setq magit-repolist-columns
      '(("Name"     15 magit-repolist-column-ident                    nil)
        ("Version"  25 magit-repolist-column-version                  nil)
        ("*"         1 magit-repolist-column-flag                     ((:help-echo "Uncommitted changes")))
        ("S"         1 magit-repolist-column-stashes                  ((:help-echo "Stash count")))
        ("Branch"   25 magit-repolist-column-branch                   nil)
        ("B"         1 magit-repolist-column-branches                 ((:help-echo "Branch count")))
        ("Remote"    6 ec--magit-repolist-column-push-remote          nil)
        ("↓"         1 magit-repolist-column-unpulled-from-pushremote ((:right-align t :help-echo "Push remote changes not in local")))
        ("↑"         1 magit-repolist-column-unpushed-to-pushremote   ((:right-align t :help-echo "Local changes not in push remote")))
        ("Upstream" 15 magit-repolist-column-upstream                 nil)
        ("↓"         1 magit-repolist-column-unpulled-from-upstream   ((:right-align t :help-echo "Upstream changes not in local")))
        ("↑"         1 magit-repolist-column-unpushed-to-upstream     ((:right-align t :help-echo "Local changes not in upstream")))
        ("Path"     10 magit-repolist-column-path                     nil)))

(defun ec--magit-repolist-column-push-remote (_id)
  "Insert the push remote of the current branch."
  (magit-get-push-remote))

(with-eval-after-load 'project
  ;; This duplicates code in magit-extras but is necessary since that file will
  ;; not load until something triggers it.
  (keymap-set project-prefix-map "m" #'magit-project-status)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

;; Git link.
(keymap-set global-map "C-c g l" #'git-link)

(setq git-link-default-remote "github")

;; Commit mode.
(setq git-commit-summary-max-length 50)

(defun ec--set-git-commit-width ()
  (setq fill-column 72))

(add-hook 'git-commit-mode-hook #'ec--set-git-commit-width)

(add-hook 'git-commit-mode-hook #'evil-insert-state)

;; Try speeding up Tramp.
(setq vc-handled-backends '(Git)) ;; Only look for git.

;;; vc.el ends here
