;;; prog.el --- Programming configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(markdown-mode
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
                                   editorconfig))

;; LSP
(setq lsp-enable-file-watchers nil ; Call `lsp-workspace-restart' if necessary
      lsp-keymap-prefix "C-c l")

;; Elisp.
(setq flycheck-emacs-lisp-load-path 'inherit)

;; Typescript and Tide.
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(defun ec--setup-tide ()
  "Set up tide."
  (when (or (eq major-mode 'typescript-mode)
            (and (eq major-mode 'web-mode)
                 (string-equal "tsx" (file-name-extension buffer-file-name))))
    (tide-setup)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
    (eldoc-mode)
    (tide-hl-identifier-mode)))

(add-hook 'typescript-mode-hook #'ec--setup-tide)
(add-hook 'web-mode-hook #'ec--setup-tide)

;; Flycheck.
(setq flycheck-indication-mode nil)

(defun ec--find-node-module-binary (name &optional directory)
  "Travel upward from DIRECTORY looking for NAME in node_modules."
  (let* ((root (or directory default-directory))
         (binary (and root (expand-file-name (concat "node_modules/.bin/" name) root))))
    (cond ((and binary (file-executable-p binary)) binary)
          ((string-equal root "/") nil)
          (t (ec--find-node-module-binary name (directory-file-name (file-name-directory root)))))))

(defun ec--find-eslint ()
  "Travel upward to find eslint in node_modules."
  (when (or (eq major-mode 'js-mode) (eq major-mode 'typescript-mode))
    (let ((eslint (ec--find-node-module-binary "eslint")))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'ec--find-eslint)

(when (fboundp 'global-flycheck-mode)
  (add-hook 'emacs-startup-hook #'global-flycheck-mode))

;; Dash.
(setq dash-docs-docsets-path (expand-file-name "docsets" ec-cache-dir))

(defun ec-docs-at-point ()
  "Search docs for thing at point."
  (interactive)
  (message "TODO: implement"))

(defun ec--typescript-doc ()
  "Enable docs for `typescript-mode'."
  (setq-local dash-docs-docsets '("JavaScript")))

(add-hook 'typescript-mode-hook #'ec--typescript-doc)

;; Editorconfig.
(setq editorconfig-exclude-modes (list 'emacs-lisp-mode 'lisp-interaction-mode))

(when (fboundp 'editorconfig-mode)
  (add-hook 'emacs-startup-hook #'editorconfig-mode))

;;; prog.el ends here
