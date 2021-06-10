;;; scratch.el --- Scratch buffer tests. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'projectile)

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
  ;; Text mode without a project.
  (find-file "/")
  (ec-scratch-buffer)
  (should (string= "*scratch text*" (buffer-name)))
  (should (eq 'text-mode major-mode))
  ;; The content uses the mode's comment style if there is one.
  (should (string= "project: none\nmode:    text-mode\n\n"
                   (buffer-substring-no-properties (point-min) (point-max))))

  ;; Same but in lisp-interaction mode.
  (switch-to-buffer (get-buffer-create "scratch test lisp"))
  (emacs-lisp-mode)
  (ec-scratch-buffer)
  (should (string= "*scratch lisp-interaction*" (buffer-name)))
  (should (eq 'lisp-interaction-mode major-mode))
  (should (string= ";project: none\n;mode:    lisp-interaction-mode\n\n"
                   (buffer-substring-no-properties (point-min) (point-max))))

  ;; Project.
  (find-file ec-config-dir) ; So we are in a project.
  (ec-scratch-buffer)
  (should (string= (format "*scratch text %s*" (projectile-project-name))
                   (buffer-name)))
  (should (eq 'text-mode major-mode))
  (should (string= (format "project: %s\nmode:    text-mode\n\n"
                           (projectile-project-name))
                   (buffer-substring-no-properties (point-min) (point-max))))

  ;; Manual project.
  (find-file "/")
  (ec-scratch-buffer "test-project")
  (should (string= "*scratch text test-project*" (buffer-name)))
  (should (eq 'text-mode major-mode))
  (should (string= "project: test-project\nmode:    text-mode\n\n"
                   (buffer-substring-no-properties (point-min) (point-max)))))

(ert-deftest ec-test-scratch-buffer-p ()
  "Test that scratch buffers are detected as scratch."
  ;; Create some scratch buffers.
  (find-file "/") ; So we aren't in a project.
  (ec-global-scratch-buffer)
  (ec-scratch-buffer)
  (ec-scratch-buffer "test-project")
  (find-file ec-config-dir) ; So we are in a project.
  (ec-scratch-buffer)

  ;; Both switching to the buffer and passing it in should work.
  (dolist (name `("*scratch*"
                  "*scratch lisp-interaction*"
                  "*scratch lisp-interaction test-project*"
                  ,(format "*scratch text %s*" (projectile-project-name))))
    (switch-to-buffer (should (get-buffer name)))
    (should (ec-scratch-buffer-p))
    (switch-to-buffer "*Messages*")
    (should (ec-scratch-buffer-p (should (get-buffer name)))))

  (dolist (name '("*Messages*"))
    (switch-to-buffer (should (get-buffer name)))
    (should-not (ec-scratch-buffer-p))
    (switch-to-buffer "*scratch*")
    (should-not (ec-scratch-buffer-p (should (get-buffer name))))))

(ert-deftest ec-test-scratch-combine ()
  "Test that the project and mode are combined if they are the same."
  (switch-to-buffer (get-buffer-create "scratch test lisp"))
  (lisp-interaction-mode)
  (ec-scratch-buffer "lisp-interaction")
  (should (string= "*scratch [lisp-interaction]*" (buffer-name))))

;; scratch.el ends here
