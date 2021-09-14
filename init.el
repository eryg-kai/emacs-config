;;; init.el --- Emacs init.-*- lexical-binding: t -*-

;;; Commentary:

;; Core initialization.

;;; Code:

(setq lexical-binding t ; Only affects evaluating code.
      delete-by-moving-to-trash nil)

;; Directories and load paths.
(defconst ec-dir (file-name-directory load-file-name))
(defconst ec-test-dir (file-name-as-directory
                       (expand-file-name "test" ec-dir)))
(defconst ec-lib-dir (file-name-as-directory
                      (expand-file-name "lib" ec-dir)))
(defconst ec-config-dir (file-name-as-directory
                         (expand-file-name "config" ec-dir)))
(defconst ec-cache-dir (file-name-as-directory
                        (or (getenv "XDG_CACHE_HOME")
                            (expand-file-name "~/.cache"))))
(defconst ec-mail-dir (file-name-as-directory
                       (or (getenv "MAIL_HOME")
                           (expand-file-name "~/com"))))
(defconst ec-tmpl-dir (file-name-as-directory
                       (expand-file-name "templates" ec-dir)))
(defconst ec-org-dir (file-name-as-directory
                      (or (getenv "ORG_HOME")
                          (expand-file-name "~/org"))))
(defconst ec-theme-dir (file-name-as-directory
                        (expand-file-name "themes" ec-dir)))
(defconst ec-research-dir (file-name-as-directory
                           (or (getenv "ORG_RESEARCH_HOME")
                               (expand-file-name "research" ec-org-dir))))
(defconst ec-lang-dir (file-name-as-directory
                       (or (getenv "ORG_LANG_HOME")
                           (expand-file-name "languages" ec-research-dir))))
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
