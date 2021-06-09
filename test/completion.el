;;; completion.el --- Test completions. -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(defvar ec-test--spy-count 0)
(defvar ec-test--spy-value nil)

(defun ec-test--spy-reset()
  "Reset spy count and value."
  (setq ec-test--spy-value nil)
  (setq ec-test--spy-count 0))

(defun ec-test--spy(&rest args)
  "Increment spy count and set ARGS."
  (setq ec-test--spy-value args)
  (setq ec-test--spy-count (+ 1 ec-test--spy-count)))

(ert-deftest ec-test-split-name ()
  (should (equal '("prefix" "-" "test") (ec--split-name "prefix-test")))
  (should (equal '("doubleprefix" "-" "test") (ec--split-name "doubleprefix--test")))
  (should (equal '("prefix" "/" "test") (ec--split-name "prefix/test")))
  (should (equal '("doubleprefix" "/" "test") (ec--split-name "doubleprefix//test")))
  (should (equal '("prefix-prefix" "/" "test") (ec--split-name "prefix-prefix/test")))
  (should (equal '("prefix" "-" "test-command") (ec--split-name "prefix-test-command")))
  (should (equal '("prefix" "/" "test-command") (ec--split-name "prefix/test-command")))
  (should (equal '("doubleprefix-prefix" "/" "test") (ec--split-name "doubleprefix-prefix//test")))
  (should-not (ec--split-name nil))
  (should-not (ec--split-name "command")))

(ert-deftest ec-test-replace-then-run ()
  (ec-test--spy-reset)
  (should-not ec-test--spy-value)
  (ec--replace-then-run 'ec-test--spy '("SPC" . "execute-extended-command"))
  (should (equal '(("SPC" . "execute-extended-command") nil) ec-test--spy-value))
  (ec--replace-then-run 'ec-test--spy '("SPC 0" . "ec-test-replace-then-run"))
  (should (equal '(("SPC 0" . "test-replace-then-run") nil) ec-test--spy-value))
  (ec--replace-then-run 'ec-test--spy '("SPC 1" . "ec-spy"))
  (should (equal '(("SPC 1" . "spy") nil) ec-test--spy-value)))

;;; completion.el ends here
