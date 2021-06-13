;;; init.el --- Emacs init.-*- lexical-binding: t -*-

;;; Commentary:

;; Core initialization.

;;; Code:

(setq lexical-binding t ; Only affects evaluating code.
      delete-by-moving-to-trash nil)

;; Directories and load paths.
(defconst ec-dir (file-name-directory load-file-name))
(defconst ec-test-dir (expand-file-name "test" ec-dir))
(defconst ec-lib-dir (expand-file-name "lib" ec-dir))
(defconst ec-config-dir (expand-file-name "config" ec-dir))
(defconst ec-cache-dir (or (getenv "XDG_CACHE_HOME")
                           (expand-file-name "~/.cache")))
(defconst ec-mail-dir (or (getenv "MAIL_HOME") (expand-file-name "~/com")))
(defconst ec-tmpl-dir (expand-file-name "templates" ec-dir))
(defconst ec-org-dir (or (getenv "ORG_HOME") (expand-file-name "~/org")))
(defconst ec-theme-dir (expand-file-name "themes" ec-dir))
(push ec-theme-dir custom-theme-load-path)
(autoload 'mu4e "mu4e" "mu for Emacs." t) ; Installed via the system.

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
;;                  (quelpa '(org-fc :repo "l3kn/org-fc" :fetcher github :files (:defaults "awk")))
(setq package-selected-packages '(quelpa esup)
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

;; Tests.
(defun ec-test()
  "Test configuration."
  (interactive)
  (ec--load-directory ec-test-dir)
  (if noninteractive
      (ert-run-tests-batch-and-exit)
    (ert t)))

;;; init.el ends here
