;;; funcs.el --- Test utility functions. -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(ert-deftest ec-test-center-truncate ()
  "Test truncating from the center."
  (should (string= "tes…test" (ec-center-truncate "test--------test" 8)))
  (should (string= "test…test" (ec-center-truncate "test--------test" 9)))
  (should (string= "time…ncate" (ec-center-truncate 'timer--center-truncate 10)))
  (should (string= "(lam…il t)" (ec-center-truncate '(lambda () t) 10))))

;;; funcs.el ends here
