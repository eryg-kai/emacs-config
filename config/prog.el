;;; prog.el --- Programming configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(csv-mode
                                   dash-docs
                                   groovy-mode
                                   kotlin-ts-mode
                                   markdown-mode
                                   nix-mode
                                   racket-mode
                                   rainbow-mode
                                   reformatter
                                   web-mode
                                   yaml-mode
                                   yarn-mode))

;; Eglot.
(with-eval-after-load 'eglot
  (keymap-set eglot-mode-map "C-c a r" 'eglot-rename)
  (keymap-set eglot-mode-map "C-c a o" 'eglot-code-action-organize-imports)
  (keymap-set eglot-mode-map "C-c a d" 'xref-find-definitions))

;; Go.
(when (fboundp 'reformatter-define)
  (reformatter-define go-format
    :program "gofmt"
    :args '("-s"))

  (add-hook 'go-ts-mode-hook #'go-format-on-save-mode))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(defun ec-gud-localize (fn fmt &rest args)
  "Run FN with FMT and ARGS after ensuring FMT does not contain Tramp prefixes."
  (let* ((remote (file-remote-p default-directory))
         (fmt (if remote (string-replace remote "" fmt) fmt)))
    (apply fn fmt args)))

(advice-add 'gud-call :around #'ec-gud-localize)

;; Elisp.
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

;; Dash.
(setq dash-docs-docsets-path (expand-file-name "docsets" (xdg-cache-home)))

(defun ec-docs-at-point ()
  "Search docs for thing at point."
  (interactive)
  (message "TODO: implement"))

;; TypeScript.
(defun ec--typescript-doc ()
  "Enable docs for `typescript-mode'."
  (setq-local dash-docs-docsets '("JavaScript")))

(add-hook 'typescript-ts-mode-hook #'ec--typescript-doc)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(when (fboundp 'reformatter-define)
  (reformatter-define biome-format
    :program "biome"
    :args `("format" "--stdin-file-path" ,(buffer-file-name)))

  (defun ec--maybe-enable-biome ()
    "Enable Biome formatter if there is a biome.json file."
    (when (locate-dominating-file
           (or (buffer-file-name) (project-root (project-current t)))
           "biome.json")
      (biome-format-on-save-mode)))

  (add-hook 'typescript-ts-mode-hook #'ec--maybe-enable-biome)
  (add-hook 'tsx-ts-mode-hook #'ec--maybe-enable-biome))

;; Editorconfig.
(add-hook 'emacs-startup-hook #'editorconfig-mode)

;; Elixir.
(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))

;; `elixir-ts-mode' does specify these but they are not auto-loaded.
(add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-ts-mode))
(add-to-list 'auto-mode-alist '("mix\\.lock" . elixir-ts-mode))

;; Kotlin.
(when (fboundp 'reformatter-define)
  (reformatter-define kotlin-format
    :program "ktlint"
    :args '("-F" "--stdin" "--log-level=error"))

  (add-hook 'kotlin-ts-mode-hook #'kotlin-format-on-save-mode))

(add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-ts-mode))

;; Rust.
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

;;; prog.el ends here
