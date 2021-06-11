;;; edit.el --- General editing configuration. -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration that applies to the entire editing experience.

;;; Code:

;; General settings.
(setq-default fill-column 80
              indent-tabs-mode nil
              tab-width 2
              tab-stop-list (list 2)
              c-basic-offset 2
              standard-indent 2)

(setq sentence-end-double-space nil

      select-enable-primary t

      mode-require-final-newline t
      require-final-newline t)

;; Smart parenthesis.
(defun ec--indent-between-pair (&rest _args)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(with-eval-after-load 'smartparens
  (require 'smartparens-config)
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((ec--indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((ec--indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((ec--indent-between-pair "RET"))))

(add-hook 'emacs-startup-hook #'smartparens-global-mode)

;; Undo tree.
(setq undo-tree-history-directory-alist
      `((".*" . ,(expand-file-name "undo-tree/" user-emacs-directory)))
      undo-tree-auto-save-history t
      undo-limit        400000  undo-tree-limit        400000
      undo-strong-limit 3000000 undo-tree-strong-limit 3000000
      undo-outer-limit  3000000 undo-tree-outer-limit  3000000)

(add-hook 'text-mode-hook #'undo-tree-mode)
(add-hook 'prog-mode-hook #'undo-tree-mode)

;; Filling and unfilling.
(defun ec--prog-auto-fill-mode ()
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode))

(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'ec--prog-auto-fill-mode)

;; Delete whitespace.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; A way to use C-u when it's bound in Evil.
(define-key global-map (kbd "C-c u") #'universal-argument)

(defun ec-unfill (beg end)
  "Unfill the text from BEG to END or the paragraph if nil."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (if (use-region-p)
        (fill-region beg end)
      (fill-paragraph nil))))

(define-key global-map (kbd "M-Q") #'ec-unfill)

;; Todo keywords in comments.
(defface hl-temp     '((t (:inherit 'hl-todo :weight bold))) "Face for the TEMP keyword.")
(defface hl-note     '((t (:inherit 'hl-todo :weight bold))) "Face for the NOTE keyword.")
(defface hl-review   '((t (:inherit 'hl-todo :weight bold))) "Face for the REVIEW keyword.")
(defface hl-optimize '((t (:inherit 'hl-todo :weight bold))) "Face for the OPTIMIZE keyword.")
(defface hl-fixme    '((t (:inherit 'hl-todo :weight bold))) "Face for the FIXME keyword.")
(defface hl-hack     '((t (:inherit 'hl-todo :weight bold))) "Face for the HACK keyword.")

(setq hl-todo-keyword-faces
      '(("TEMP"     . hl-temp)
        ("NOTE"     . hl-note)
        ("REVIEW"   . hl-review)
        ("TODO"     . hl-todo)
        ("OPTIMIZE" . hl-optimize)
        ("FIXME"    . hl-fixme)
        ("HACK"     . hl-hack)))

(add-hook 'emacs-startup-hook #'global-hl-todo-mode)

;; Line numbers.
(defun ec--enable-line-numbers ()
  (setq display-line-numbers 'relative))

(add-hook 'csv-mode-hook  #'ec--enable-line-numbers) ; Parent is text mode.
(add-hook 'conf-mode-hook #'ec--enable-line-numbers) ; No parent?
(add-hook 'yaml-mode-hook #'ec--enable-line-numbers) ; Parent is text-mode.
(add-hook 'prog-mode-hook #'ec--enable-line-numbers)

;; Subword and superword.
(add-hook 'go-mode-hook #'subword-mode)
(add-hook 'typescript-mode-hook #'subword-mode)

(add-hook 'text-mode #'superword-mode)
(add-hook 'emacs-lisp-mode #'superword-mode)

;;; edit.el ends here