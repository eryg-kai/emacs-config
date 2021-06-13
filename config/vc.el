;;; vc.el --- Version control configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(ediff
                                   magit
                                   git-link
                                   forge
                                   git-commit))

;; Ediff.
(setq-default ediff-split-window-function #'split-window-horizontally
              ediff-merge-split-window-function #'split-window-horizontally
              ediff-window-setup-function #'ediff-setup-windows-plain)

;; Magit.
(define-key global-map (kbd "C-c gb") #'magit-blame)
(define-key global-map (kbd "C-c gg") #'magit-file-dispatch)
(define-key global-map (kbd "C-c gs") #'magit-status)
(define-key global-map (kbd "C-c gB") #'magit-log-buffer-file)
(define-key global-map (kbd "C-c gf") #'magit-find-file)
(define-key global-map (kbd "C-c gd") #'magit-list-repositories)

(setq magit-delete-by-moving-to-trash nil
      magit-module-sections-nested nil
      magit-diff-refine-hunk t
      magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

(with-eval-after-load 'magit
  (add-to-list 'magit-status-sections-hook 'magit-insert-modules t))

;; Magit repolist.
(setq magit-repolist-columns
      '(("Name"     15 magit-repolist-column-ident                    nil)
        ("Version"  25 magit-repolist-column-version                  nil)
        ("*"         1 magit-repolist-column-dirty                    ((:help-echo "Dirty state")))
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

;; Git lin.
(define-key global-map (kbd "C-c gl") #'git-link)

;; Commit mode.
(setq git-commit-summary-max-length 50)

(defun ec--set-git-commit-width ()
  (setq fill-column 72))

(add-hook 'git-commit-mode-hook #'ec--set-git-commit-width)

(add-hook 'git-commit-mode-hook #'evil-insert-state)

;;; vc.el ends here
