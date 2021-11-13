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
  (should (equal '(("SPC" . "execute-extended-command")) ec-test--spy-value))
  (ec--replace-then-run 'ec-test--spy '("SPC 0" . "ec-test-replace-then-run"))
  (should (equal '(("SPC 0" . "test-replace-then-run")) ec-test--spy-value))
  (ec--replace-then-run 'ec-test--spy '("SPC 1" . "ec-spy"))
  (should (equal '(("SPC 1" . "spy")) ec-test--spy-value)))

(ert-deftest ec-test-ffap ()
  (defun ec-test--ffap-run (file line column)
    "Run `ec-ffap' and ensure FILE is opened to LINE then kill the buffer."
    (with-simulated-input "RET" (ec-ffap))
    (should (string= file (buffer-name)))
    (should (equal line (line-number-at-pos)))
    (should (equal (1- (or column 1)) (current-column)))
    (kill-this-buffer))

  (switch-to-buffer (get-buffer-create "ffap test"))

  (dolist (dir `(,(concat ec-dir "test/fixtures/")
                 "./config/../test/fixtures/"
                 ,(concat "../" (file-name-base (directory-file-name ec-dir)) "/test/fixtures/")
                 "./test/fixtures/"))
    (dolist (file '("file" ".file"))
      (dolist (line '(10))
        (dolist (column '(nil 15))
          (insert (concat dir file) (format ":%s" line))
          (when column (insert (format ":%s" column)))

          ;; Beginning of the line always works.
          (beginning-of-line)
          (ec-test--ffap-run file line column)

          ;; For relative paths having the point on the first slash always works.
          (forward-char 1)
          (ec-test--ffap-run file line column)

          ;; At this point relative paths only fail if there is one slash and the
          ;; file does not start with a dot.
          (forward-char 1)
          (ec-test--ffap-run file line column)

          ;; Now all one-slash relative paths will fail including those that start
          ;; with a dot.
          (end-of-line)
          (ec-test--ffap-run file line column)

          (insert "\n")))))

  ;; Should be disabled in dired.
  (dired-jump)
  (with-simulated-input "new-file RET" (ec-ffap))
  (should (string= "new-file" (buffer-name))))

;;; completion.el ends here
