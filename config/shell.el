;;; shell.el --- Shell configuration. -*- lexical-binding: t -*-

;;; Commentary:

;; Customizations for eshell, shell, term, etc.

;;; Code:

(nconc package-selected-packages '(eshell-prompt-extras
                                   eshell-z))

;; Comint.
(setq comint-input-ignoredups t
      comint-input-history-ignore "^ ")

(defun node-repl ()
  "Node REPL."
  (interactive)
  (pop-to-buffer
   (make-comint "*node-repl*" "node" nil "--interactive")
   '((display-buffer-reuse-window display-buffer-same-window))))

;; Eshell.
(setq eshell-history-size 10000
      eshell-visual-commands '("ssh" "htop" "watch" "mtr")
      eshell-hist-ignoredups t
      eshell-prefer-lisp-variables t
      eshell-prefer-lisp-functions t
      eshell-highlight-prompt nil
      eshell-prompt-function 'ec--eshell-prompt
      eshell-banner-message ""
      eshell-directory-name (expand-file-name "eshell" (xdg-data-home))
      eshell-z-freq-dir-hash-table-file-name (expand-file-name "eshell/z" (xdg-data-home))
      eshell-prompt-regexp "λ")

;; Unique history per eshell buffer.
(defun ec--eshell-set-history-file ()
  "Set history file based on the buffer name."
  (setq-local eshell-history-file-name
              (expand-file-name
               (replace-regexp-in-string "[ /]" "-" (buffer-name))
               eshell-directory-name)))

(add-hook 'eshell-hist-load-hook #'ec--eshell-set-history-file)

;; Write history after a command is executed.
(defun ec--eshell-write-history-soon ()
  (timer-idle-debounce #'eshell-write-history nil t))

(add-hook 'eshell-pre-command-hook  #'ec--eshell-write-history-soon)

;; Make !! and others work.
(add-hook 'eshell-expand-input-functions #'eshell-expand-history-references)

;; Prompt.
(defun ec--eshell-prompt()
  "Eshell prompt."
  (let ((status (or (bound-and-true-p eshell-last-command-status) 0)))
    (concat
     (if (not (zerop status)) (propertize (format "[%d] " status) 'face 'error) "")
     (if (fboundp 'epe-theme-lambda) (epe-theme-lambda) "$ "))))

;; It doesn't have its own autoloads.
(autoload 'epe-theme-lambda "eshell-prompt-extras")

;; Tramp support.
(with-eval-after-load "eshell"
  (add-to-list 'eshell-modules-list 'eshell-tramp))

;; Z for navigating previously visited directories.
(defun ec--load-eshell-z ()
  (require 'eshell-z nil t))

(add-hook 'eshell-mode-hook #'ec--load-eshell-z)

;; Compilation.
(setq compilation-scroll-output 'first-error)

(with-eval-after-load "compile"
  ;; This seems to result in false positives especially around timestamps in
  ;; brackets like [HH:MM:DD].
  (delete 'ant compilation-error-regexp-alist))

(defun ec--colorize-compile-buffer ()
  "Colorize ANSI codes in a compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))

(add-hook 'compilation-filter-hook #'ec--colorize-compile-buffer)

(defun ec-compile (&rest args)
  "Execute ARGS asynchronously in a compilation buffer.

ARGS are simply concatenated with spaces.

If no ARGS are provided, prompt for the command."
  (interactive (list (read-shell-command "$ ")))
  (let* ((command (mapconcat
                   #'(lambda (a)
                       (if (numberp a)
                           (number-to-string a)
                         a))
                   args " " ))
         (compilation-buffer-name-function
          #'(lambda (_) (format "*%s*" command))))
    (compile command)))

(defalias 'eshell/compile #'ec-compile)

;;; shell.el ends here
