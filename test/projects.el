;;; projects.el --- Test project management. -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(ert-deftest ec-test-ignore-project ()
  "Tests `ec--ignore-project'."
  (should (ec--ignore-project (expand-file-name  "~/.config/emacs")))
  (should-not (ec--ignore-project (expand-file-name "~/ops")))
  (should-not (ec--ignore-project (expand-file-name "~/ops/emacs")))
  (should (ec--ignore-project (expand-file-name "~/ops/emacs/config"))))

;;; projects.el ends here
