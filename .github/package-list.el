;;; package-list.el --- Emacs package list. -*- lexical-binding: t -*-

;; Package-Requires: (anzu avy beancount cape consult csv-mode dash-docs docker doom-themes eshell-prompt-extras eshell-z esup evil evil-args evil-collection evil-commentary evil-escape evil-exchange evil-org evil-snipe evil-string-inflection evil-surround evil-terminal-cursor-changer forge gnuplot groovy-mode hl-todo kotlin-ts-mode magit magit-annex markdown-mode minions nix-mode ob-async ob-go ob-http ob-kotlin ob-restclient ob-typescript org-bullets org-drill org-edna pdf-tools persistent-scratch pinentry qml-mode racket-mode rainbow-mode reformatter rfc-mode speed-type tempel undo-tree web-mode winum with-simulated-input yaml-mode yarn-mode)

;;; Commentary:

;; Externally installed packages.  Meant to be used for cache busting in CI and
;; for NixOS to determine what packages to install.

;;; Code:

(save-excursion
  (goto-char (point-min))
  (re-search-forward ";; Package-Requires:")
  (kill-line)
  (insert (format " %s"
                  (sort
                   package-selected-packages
                   #'string<))))

;;; package-list.el ends here
