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

;; If the path to your shell is different locally from the remote then
;; `xref-matches-in-files' will fail (in NixOS I end up getting
;; /run/current-system/sw/bin/bash).  Rely on /bin/sh instead.
;; TODO: What is the proper fix?  Also, no error is reported (instead it only
;; reports there are no matches).
(setq shell-file-name "/bin/sh")

(setq project-buffers-viewer #'project-list-buffers-ibuffer)

;;; projects.el ends here
