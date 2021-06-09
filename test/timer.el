;;; timer.el --- Timer tests. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'ert-x)

(defvar timer-test--idle-debounce-test-var)

(ert-deftest timer-test-idle-debounce-test ()
  "Test idle debounce."
  (setq timer-test--count 0)
  (defun timer-test--run()
    (setq timer-test--count (+ 1 timer-test--count)))

  ;; Run three timers.
  (dotimes (i 3) (timer-idle-debounce #'timer-test--run))
  (should (eq 0 timer-test--count))

  ;; Only the latest should have ran.
  (ert-run-idle-timers)
  (should (eq 1 timer-test--count))

  ;; Run three more timers.
  (dotimes (i 3) (timer-idle-debounce #'timer-test--run))

  ;; Should again run only once.
  (ert-run-idle-timers)
  (should (eq 2 timer-test--count)))

(defvar timer-test--count)

(ert-deftest timer-test-per-buffer-idle-debounce-test ()
  "Test per-buffer idle debounce."
  (defvar-local timer-test--per-buffer-name nil)
  (defvar-local timer-test--per-buffer-count 0)

  (defun timer-test--per-buffer-run()
    "Set a local variable to the name of the buffer it is in and increment the local count."
    (setq-local timer-test--per-buffer-count (+ 1 timer-test--per-buffer-count))
    (setq-local timer-test--per-buffer-name (buffer-name)))

  ;; Create three buffers and start debounced timers in each.
  (dotimes (i 3)
    (let ((name (format "test buffer %d" i)))
      (get-buffer-create name)
      (switch-to-buffer name)
      (dotimes (_ 3) (timer-idle-debounce #'timer-test--per-buffer-run nil t))))

  ;; There are no timers here so there should have no changes.
  (switch-to-buffer "*Messages*")
  (ert-run-idle-timers)
  (should (eq 0 timer-test--per-buffer-count))
  (should (eq nil timer-test--per-buffer-name))

  ;; In each buffer their timers should have ran once.
  (dotimes (i 3)
    (let ((name (format "test buffer %d" i)))
      (switch-to-buffer name)
      (should (eq 1 timer-test--per-buffer-count))
      (should (string= name timer-test--per-buffer-name)))))

(defvar timer-test--flush-test-var)

(ert-deftest timer-test-flush-test ()
  "Test flushing actions."
  (setq timer-test--flush-test-var 0)
  (defun timer-test--flush-test()
    (setq timer-test--flush-test-var (+ 1 timer-test--flush-test-var)))

  (timer-idle-debounce #'timer-test--flush-test nil t)
  (should (eq 0 timer-test--flush-test-var))
  (timer-flush)
  (should (eq 1 timer-test--flush-test-var)))

;;; timer.el ends here
