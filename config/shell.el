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

(with-eval-after-load 'evil
  (evil-define-key 'normal comint-mode-map
    (kbd "q") 'quit-window))

;; Eshell.
(setq eshell-history-size 10000
      eshell-visual-commands '("ssh" "htop" "watch" "mtr" "iotop" "bmon" "nethogs")
      eshell-destroy-buffer-when-process-dies t
      eshell-hist-ignoredups t
      eshell-prefer-lisp-variables t
      eshell-prefer-lisp-functions nil
      eshell-highlight-prompt nil
      eshell-prompt-function 'ec--eshell-prompt
      eshell-banner-message ""
      eshell-directory-name (expand-file-name "eshell" (xdg-data-home))
      eshell-z-freq-dir-hash-table-file-name (expand-file-name "eshell/z" (xdg-data-home)))

;; The prefer variable seems to have no effect (lisp functions are always
;; preferred), so fix that here.
(defun ec--find-plain-lisp-command (fn command)
  "Call FN if the elisp version of COMMAND should be used."
  (when (member command '("cd" "exit" "export"))
    (funcall fn command)))

(advice-add #'eshell--find-plain-lisp-command :around #'ec--find-plain-lisp-command)

;; Unique history per eshell buffer.
(defun ec--eshell-set-history-file ()
  "Set history file based on the buffer name."
  (setq-local eshell-history-file-name
              (expand-file-name
               (replace-regexp-in-string "[ /]" "-" (buffer-name))
               eshell-directory-name)))

(add-hook 'eshell-hist-load-hook #'ec--eshell-set-history-file)

(defvar-local ec--eshell-history-timer nil "Eshell history write timer.")

;; Write history after a command is executed.
(defun ec--eshell-write-history-soon ()
  "Write the current shell's history after a timer."
  (when ec--eshell-history-timer (cancel-timer ec--eshell-history-timer))
  (setq ec--eshell-history-timer
        (run-with-idle-timer
         30
         nil
         `(lambda ()
            (when (buffer-live-p ,(current-buffer))
              (with-current-buffer ,(current-buffer)
                (eshell-write-history)))))))

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

(defun ec--eshell-prompt-regex (&rest _ignore)
  "Set `eshell-prompt-regex'."
  (setq eshell-prompt-regexp "^[^\nλ]* λ "))

;; `epe-theme-lambda' adds # to the prompt which gives false positives.
(advice-add 'epe-theme-lambda :after #'ec--eshell-prompt-regex)

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

(defun ec--compile (fn command &rest args)
  "Run FN with COMMAND and ARGS after setting `compilation-buffer-name-function'."
  (let ((compilation-buffer-name-function #'(lambda (_) (format "*%s*" command))))
    (apply fn command args)))

(advice-add 'compile :around #'ec--compile)

;;; shell.el ends here
