;;; init.el --- Emacs init.-*- lexical-binding: t -*-

;;; Commentary:

;; Core initialization.

;;; Code:

(setq lexical-binding t ; Only affects evaluating code.
      delete-by-moving-to-trash nil)

;; Manage with `package-install-selected-packages' and `package-autoremove'.
(setq package-selected-packages '(quelpa
                                  esup

                                  doom-themes
                                  evil-terminal-cursor-changer

                                  ediff
                                  magit
                                  git-link
                                  forge
                                  git-commit

                                  ripgrep
                                  projectile

                                  persistent-scratch

                                  smartparens
                                  evil-string-inflection
                                  hl-todo
                                  undo-tree

                                  docker
                                  magit-annex
                                  rfc-mode
                                  pinentry
                                  speed-type
                                  pdf-tools

                                  markdown-mode
                                  nix-mode
                                  web-mode
                                  yaml-mode
                                  yarn-mode
                                  lsp-mode
                                  go-mode
                                  css-mode
                                  tide
                                  typescript-mode
                                  flycheck
                                  dash-docs
                                  editorconfig

                                  fancy-battery
                                  anzu

                                  eshell-prompt-extras
                                  eshell-z

                                  evil
                                  evil-collection
                                  evil-snipe
                                  evil-surround
                                  evil-commentary
                                  evil-exchange
                                  evil-escape
                                  evil-org
                                  avy

                                  which-key
                                  yasnippet
                                  yasnippet-snippets
                                  company
                                  ispell
                                  flimenu

                                  typo
                                  org-bullets
                                  ob-async
                                  ob-go
                                  ob-http
                                  ob-restclient
                                  ob-typescript
                                  org-edna)
      package-quickstart t)
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Directories.
(defconst ec-dir (file-name-directory load-file-name))
(defconst ec-test-dir (expand-file-name "test" ec-dir))
(defconst ec-lib-dir (expand-file-name "lib" ec-dir))
(defconst ec-config-dir (expand-file-name "config" ec-dir))
(defconst ec-cache-dir (or (getenv "XDG_CACHE_HOME") (expand-file-name "~/.cache")))
(defconst ec-mail-dir (or (getenv "MAIL_HOME") (expand-file-name "~/com")))
(defconst ec-tmpl-dir (expand-file-name "templates" ec-dir))
(defconst ec-org-dir (or (getenv "ORG_HOME") (expand-file-name "~/org")))
(defconst ec-log-dir (expand-file-name user-login-name ec-org-dir))
(defconst ec-theme-dir (expand-file-name "themes" ec-dir))

;; Load configuration.
(push ec-theme-dir custom-theme-load-path)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load (file-name-sans-extension custom-file) t t)

(require 'find-lisp)
(defun ec--load-directory (directory)
  "Recursively load all `.el' files in DIRECTORY."
  (mapcar (lambda (fn)
            (load (file-name-sans-extension fn) nil t))
          (find-lisp-find-files directory "\\.el\\'")))

(ec--load-directory ec-lib-dir)
(ec--load-directory ec-config-dir)

(defun ec-test()
  "Test configuration."
  (interactive)
  (ec--load-directory ec-test-dir)
  (ert t))

;;; init.el ends here
