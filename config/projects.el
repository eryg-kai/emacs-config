;;; projects.el --- Project management. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(with-eval-after-load 'grep
  (grep-apply-setting
   'grep-find-command
   `(,(concat "rg --regexp '' --line-number --with-filename --null"
              " --no-heading --no-messages --max-columns 80 --max-columns-preview"
              " $(git rev-parse --show-superproject-working-tree --show-toplevel || pwd)")
     . 14)))

(setq xref-search-program 'ripgrep
      xref-search-program-alist
      `((grep . ,(concat "xargs -0 grep <C> --null --no-messages --extended-regexp"
                         " --line-number --with-filename --only-matching"
                         " --regexp '.{0,60}'<R>'.{0,20}'"))
        (ripgrep . ,(concat "xargs -0 rg <C> --null --line-number --with-filename"
                            " --no-heading --no-messages --glob '!*/' --only-matching"
                            " --regexp '.{0,60}'<R>'.{0,20}'"))))

(defun ec-project-find-regexp (regexp)
  "Find all matches for REGEXP in the current project's roots.

This is like `project-find-regexp' except uses ripgrep on the
project root instead of passing individual files and thus can
make use of ignore files."
  (interactive (list (project--read-regexp)))
  (require 'xref)
  (let* ((pr (project-current t))
         (default-directory (expand-file-name (project-root pr))))
    (xref--show-xrefs
     (apply-partially #'ec-project--find-regexp regexp default-directory)
     nil)))

(defun ec-project--find-regexp (regexp dir)
  (let* ((xref-search-program 'ripgrep)
         ;; Remove the glob exception so ripgrep traverses directories.
         (xref-search-program-alist
          `((ripgrep . ,(concat "xargs -0 rg <C> --null --line-number --with-filename"
                                " --no-heading --no-messages --only-matching"
                                " --regexp '.{0,60}'<R>'.{0,20}'"))))
         (xrefs (xref-matches-in-files regexp (list dir))))
    (unless xrefs
      (user-error "No matches for: %s" regexp))
    xrefs))


(define-key global-map (kbd "C-x g") #'ec-project-find-regexp)

;;; projects.el ends here
