;;; init.el --- Emacs init. -*- lexical-binding: t -*-

;;; Commentary:

;; Core initialization.

;;; Code:

(setq lexical-binding t ; Only affects evaluating code.
      delete-by-moving-to-trash nil)

;; Load paths for lisp code.
(defconst ec-dir (file-name-directory load-file-name))
(defconst ec-test-dir (file-name-as-directory
                       (expand-file-name "test" ec-dir)))
(defconst ec-lib-dir (file-name-as-directory
                      (expand-file-name "lib" ec-dir)))
(defconst ec-config-dir (file-name-as-directory
                         (expand-file-name "config" ec-dir)))
(defconst ec-theme-dir (file-name-as-directory
                        (expand-file-name "themes" ec-dir)))
(push ec-theme-dir custom-theme-load-path)

;; Installed via the system.  This exists in mu4e but it does not load.
(autoload 'mu4e~compose-mail "mu4e" "mu for Emacs.")
(autoload 'mu4e "mu4e" "mu for Emacs.")
(define-key global-map (kbd "C-x M") #'mu4e)
(define-mail-user-agent 'mu4e-user-agent
  'mu4e~compose-mail
  'message-send-and-exit
  'message-kill-buffer
  'message-send-hook)

;; Autoload xdg utils.
(autoload 'xdg-cache-home "xdg" "XDG cache home.")
(autoload 'xdg-data-home "xdg" "XDG data home.")
(autoload 'xdg-config-home "xdg" "XDG config home.")

;; Load custom.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load (file-name-sans-extension custom-file) t t)

;; These files only configure packages, they do not install or remove them. This
;; can be done individually or all at once:
;;   - To install run `package-install-selected-packages'.
;;   - To remove packages no longer in the list run `package-autoremove'.
;;   - To update run `package-refresh-contents' then install packages again.
;; Packages not in MELPA can be installed with Quelpa.
;;   - For example: (quelpa '(osd :repo "0x0049/osd" :fetcher github))
;;                  (quelpa '(beancount :repo "beancount/beancount-mode" :fetcher github))
(setq package-selected-packages '(quelpa esup with-simulated-input)
      package-quickstart t)

(with-eval-after-load 'package ; Won't load immediately when using quickstart.
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Load configuration.
(require 'find-lisp)
(defun ec--load-directory (directory)
  "Recursively load all `.el' files in DIRECTORY."
  (mapcar (lambda (fn)
            (load (file-name-sans-extension fn) nil t))
          (find-lisp-find-files directory "\\.el\\'")))

(ec--load-directory ec-lib-dir)
(ec--load-directory ec-config-dir)

;; Emacs might write the package list to the custom file and somehow customized
;; variables seem to get reset back to their customized values. I want the code
;; here to be the source of truth for the package list so overwrite it.
(customize-set-variable 'package-selected-packages package-selected-packages)

;; Tests.
(defun ec-test()
  "Test configuration."
  (interactive)
  (ec--load-directory ec-test-dir)
  (if noninteractive
      (ert-run-tests-batch-and-exit)
    (ert t)))

;;; init.el ends here
