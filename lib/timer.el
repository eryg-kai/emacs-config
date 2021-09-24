;;; timer.el --- Timer functions. -*- lexical-binding: t -*-

;;; Commentary:

;; Provides additional functions for running code on a timer.

;;; Code:

(defvar timer-default-timeout 30 "Default timeout.")

(defvar timer--scheduled-actions nil "Scheduled actions and their timers.")

(defun timer--unschedule (action)
  "Unschedule ACTION (a list with a function and maybe a buffer)."
  (setq timer--scheduled-actions
        (seq-filter
         (lambda (item)
           (if (equal (car item) action)
               (cancel-timer (cdr item))
             t))
         timer--scheduled-actions)))

(defun timer--debounce (function idle &optional secs per-buffer)
  "Run FUNCTION after a timer.

If IDLE is non-nil use an idle timer.

Cancel any currently running timer for FUNCTION (debounce).

When PER-BUFFER is set create a timer for each buffer and
FUNCTION combination instead of a timer per FUNCTION.
Additionally, FUNCTION will run in the context of the buffer that
was active at the time of scheduling.

The default for SECS is `timer-default-timeout'."
  (let* ((buffer (and per-buffer (current-buffer)))
         (action (if buffer `(,function . ,buffer) function)))
    (timer--unschedule action)
    (push
     `(,action
       .
       ,(apply (if idle #'run-with-idle-timer #'run-with-timer)
               (or secs timer-default-timeout)
               nil
               #'(lambda ()
                   ;; Only display a message for idle timers.
                   (when (and (bound-and-true-p ec-debug-p) idle)
                     (message (format "Running delayed function %s"
                                      (ec-center-truncate function 30))))
                   (timer--unschedule action)
                   (if buffer
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer (funcall function)))
                     (funcall function)))
               nil))
     timer--scheduled-actions)))

(defun timer-debounce (function &optional secs per-buffer)
  "Run FUNCTION after a timeout.

Cancel any currently running timer for FUNCTION (debounce).

When PER-BUFFER is set create a timer for each buffer and
FUNCTION combination instead of a timer per FUNCTION.
Additionally, FUNCTION will run in the context of the buffer that
was active at the time of scheduling.

The default for SECS is `timer-default-timeout'."
  (timer--debounce function nil secs per-buffer))

(defun timer-idle-debounce (function &optional secs per-buffer)
  "Run FUNCTION after idling.

Cancel any currently running timer for FUNCTION (debounce).

When PER-BUFFER is set create a timer for each buffer and
FUNCTION combination instead of a timer per FUNCTION.
Additionally, FUNCTION will run in the context of the buffer that
was active at the time of scheduling.

The default for SECS is `timer-default-timeout'."
  (timer--debounce function t secs per-buffer))

(defun timer-flush ()
  "Immediately run all scheduled actions."
  (let ((actions timer--scheduled-actions))
    (setq timer--scheduled-actions nil)
    (dolist (action actions)
      (timer-event-handler (cdr action)))))

(add-hook 'hibernate-hook #'timer-flush)
(add-hook 'kill-emacs-hook #'timer-flush)

;;; timer.el ends here
