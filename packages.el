;;; packages.el --- Package list.-*- lexical-binding: t -*-

;;; Commentary:

;; This file only configures package installation. To actually install use
;; `package-install-selected-packages'. To remove packages no longer in the list
;; use `package-autoremove'.

;; Packages not in MELPA can be installed with Quelpa. (Currently just `osd').

;;; Code:

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

                                  winum

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

(when (display-graphic-p)
  (push 'exwm package-selected-packages))

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;;; packages.el ends here
