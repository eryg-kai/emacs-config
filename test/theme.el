;;; theme.el --- Test theme. -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(ert-deftest ec-override-test ()
  "Test that theme overrides are loaded."
  (load-theme 'doom-one t)

  (should (equal '(doom-one-tty-override
                   doom-one-override
                   doom-override
                   override
                   doom-one)
                 custom-enabled-themes)))

(ert-deftest ec-first-cycle-test ()
  "Test that the first theme is selected if there is no current theme."
  (setq ec--current-theme nil)

  (ec-cycle-theme)

  (should (eq 'doom-one ec--current-theme))
  (should (equal '(doom-one-tty-override
                   doom-one-override
                   doom-override
                   override
                   doom-one)
                 custom-enabled-themes)))

(ert-deftest ec-cycle-test ()
  "Test that themes are cycled."
  (setq ec--current-theme 'doom-one)

  (ec-cycle-theme)
  (should (eq 'doom-solarized-light ec--current-theme))
  (should (equal '(doom-solarized-light-override
                   doom-override
                   override
                   doom-solarized-light)
                 custom-enabled-themes))

  (ec-cycle-theme)

  (should (eq 'doom-one ec--current-theme))
  (should (equal '(doom-one-tty-override
                   doom-one-override
                   doom-override
                   override
                   doom-one)
                 custom-enabled-themes)))

;;; theme.el ends here
