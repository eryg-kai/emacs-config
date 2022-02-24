;;; scratch.el --- Scratch buffer tests. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'project)

(defun ec-test--get-scratch-buffers ()
  "Get a list of scratch buffers."
  (cl-remove-if-not #'ec-scratch-buffer-p (buffer-list)))

(ert-deftest ec-test-scratch-global-buffer ()
  "Test the global scratch buffer."
  (ec-global-scratch-buffer)
  (should (string= "*scratch*" (buffer-name)))
  (should (eq initial-major-mode major-mode))
  (should (string= "" (buffer-string)))

  ;; The global scratch buffer should only use the initial major mode.
  (let ((initial-major-mode 'fundamental-mode))
    (ec-global-scratch-buffer))
  (should (eq 'fundamental-mode major-mode)))

(ert-deftest ec-test-scratch-buffer ()
  "Test scratch buffer."
  (find-file ec-config-dir) ; So we are in a project.
  (ec-scratch-buffer)
  (should (string= (project-prefixed-buffer-name "text-scratch")
                   (buffer-name)))
  (should (eq 'text-mode major-mode))
  (should (string= "" (buffer-string))))

(ert-deftest ec-test-scratch-buffer-p ()
  "Test that scratch buffers are detected as scratch."
  ;; Create some scratch buffers.
  (ec-global-scratch-buffer)
  (find-file ec-config-dir) ; So we are in a project.
  (ec-scratch-buffer)

  ;; Both switching to the buffer and passing it in should work.
  (dolist (name `("*scratch*"
                  ,(project-prefixed-buffer-name "text-scratch")))
    (switch-to-buffer (should (get-buffer name)))
    (should (ec-scratch-buffer-p))
    (switch-to-buffer "*Messages*")
    (should (ec-scratch-buffer-p (should (get-buffer name)))))

  (dolist (name '("*Messages*"))
    (switch-to-buffer (should (get-buffer name)))
    (should-not (ec-scratch-buffer-p))
    (switch-to-buffer "*scratch*")
    (should-not (ec-scratch-buffer-p (should (get-buffer name))))))

;; scratch.el ends here
