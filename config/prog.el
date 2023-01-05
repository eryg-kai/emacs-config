;;; prog.el --- Programming configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(markdown-mode
                                   nix-mode
                                   web-mode
                                   yaml-mode
                                   yarn-mode
                                   go-mode
                                   go-dlv
                                   css-mode
                                   csv-mode
                                   typescript-mode
                                   elixir-mode
                                   eglot
                                   dash-docs
                                   editorconfig
                                   rainbow-mode))

;; Eglot.
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c ar") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c ao") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c ad") 'xref-find-definitions))

;; Go.
(defun ec--hook-go-fmt ()
  "Run gofmt when saving the current file."
  (add-hook 'before-save-hook #'gofmt nil t))

(add-hook 'go-mode-hook #'ec--hook-go-fmt)

(defun ec-gud-localize (fn fmt &rest args)
  "Run FN with FMT and ARGS after ensuring FMT does not contain Tramp prefixes."
  (let* ((remote (file-remote-p default-directory))
         (fmt (if remote (string-replace remote "" fmt) fmt)))
    (apply fn fmt args)))

(advice-add 'gud-call :around #'ec-gud-localize)

;; Elisp.
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

;; JavaScript and TypeScript.
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; Dash.
(setq dash-docs-docsets-path (expand-file-name "docsets" (xdg-cache-home)))

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

;; Elixir.
(defun ec--hook-elixir-fmt ()
  "Run `elixir-format' when saving the current file."
  (when-let (dir (locate-dominating-file buffer-file-name ".formatter.exs"))
    (setq-local elixir-format-arguments
                `("--dot-formatter"
                  ,(expand-file-name ".formatter.exs" dir))))
  (add-hook 'before-save-hook #'elixir-format nil t))

(add-hook 'elixir-mode-hook #'ec--hook-elixir-fmt)

(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))

(with-eval-after-load 'eglot
 (add-to-list 'eglot-server-programs '(elixir-mode "elixir-ls")))

;;; prog.el ends here
