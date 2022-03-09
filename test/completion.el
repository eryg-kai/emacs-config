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
  (defun ec-test--ffap-run (file line column exists)
    "Run `ec-ffap' and ensure FILE is opened to LINE then kill the buffer.

If EXISTS is nil `dired' should instead be opened to the current
directory."
    (with-simulated-input "RET" (ec-ffap))
    (if exists
        (progn
          (should (string= file (buffer-name)))
          (should (equal line (line-number-at-pos)))
          (should (equal (1- (or column 1)) (current-column))))
      (should (eq major-mode 'dired-mode)))
    (kill-this-buffer))

  (let ((default-directory ec-dir))
    (switch-to-buffer (get-buffer-create "ffap test")))

  ;; Examples:
  ;; ../test/fixtures/file:10:30
  ;; ../test/fixtures/file#L10 ../test/fixtures/file#L10-L30
  ;; completion.el:10:10 ./completion.el:10:10 ~/ops/emacs/test/completion.el:10
  ;; nope.el:10 ./nope.el:10 ~/ops/emacs/test/nope.el:10 < inconsistent
  ;; nope.el:10:10 ./nope.el:10:10 ~/ops/emacs/test/nope.el:10:10 < inconsistent
  ;; /home/foo/bar/nope.el /home/foo/bar/nope.el:10 < inconsistent
  ;; /sudo::/etc/resolv.conf:2:5
  ;; ~ < this one is really weird
  ;; https://emacs.org
  ;; emacs.org < seems to ping but just hangs
  ;; mailto:test@test.com < ?
  ;; file:///etc/resolv.conf
  ;; ftp://localhost.home/etc/resolv.conf
  ;; telnet://gnu.org:80/
  ;; ~/~/.config/emacs/
  ;; *.el*
  ;; /bin:/bin2:/bin3 /bin:/bin2:/bin3:10
  ;; /
  ;; $~/ops/emacs/test/completion.el $~/ops/emacs/test/completion.el:10
  ;; ~/ops/emacs/test/completion.el$

  (dolist (dir `(,(concat ec-dir "test/fixtures/")
                 "./config/../test/fixtures/"
                 ,(concat "../" (file-name-nondirectory (directory-file-name ec-dir)) "/test/fixtures/")
                 "./test/fixtures/"
                 "test/fixtures/"))
    (dolist (test '(("file" . t) (".file" . t) ("foo" . nil)))
      (dolist (line '(10 11))
        (dolist (column '(nil 15))
          (let ((file (car test))
                (exists (cdr test)))
            (insert (concat dir file) (format ":%s" line))
            (when column (insert (format ":%s" column)))

            ;; Beginning of the line always works.
            (beginning-of-line)
            (ec-test--ffap-run file line column exists)

            ;; For relative paths having the point on the first slash always works.
            (forward-char 1)
            (ec-test--ffap-run file line column exists)

            ;; At this point relative paths only fail if there is one slash and the
            ;; file does not start with a dot.
            (forward-char 1)
            (ec-test--ffap-run file line column exists)

            ;; Now all one-slash relative paths will fail including those that start
            ;; with a dot.
            (end-of-line)
            (ec-test--ffap-run file line column exists)

            (insert "\n"))))))

  ;; Should be disabled in dired.
  (dired-jump)
  (with-simulated-input "new-file RET" (ec-ffap))
  (should (string= "new-file" (buffer-name))))

;;; completion.el ends here
