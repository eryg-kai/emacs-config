;;; mode-line.el --- Test modeline. -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(ert-deftest ectest--appt-mode-line-test ()
  "Tests `ec--appt-mode-line'."
  (should (equal (ec--appt-mode-line '("0")) "now"))
  (should (equal (ec--appt-mode-line '("0" "1" "2")) "0,1,2 min"))
  (should (equal (ec--appt-mode-line '("1")) "1 min")))

;;; mode-line.el ends here
