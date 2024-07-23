;;; record.el --- Record the screen. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar ec--record-process nil "Current recording process.")

(defun ec--record-exec (file fn &rest args)
  "Execute ARGS then call FN on success with FILE."
  (if (not ec--record-process)
      (setq ec--record-process
            (apply
             #'ec--exec-with-sentinel
             nil
             (lambda (process event)
               (pcase (process-exit-status process)
                 (0 (funcall fn file))
                 (_ (message "%s: %s" (car args) (string-trim event))))
               (setq ec--record-process nil)
               (force-mode-line-update))
             args))
    (interrupt-process ec--record-process)
    (setq ec--record-process nil))
  (force-mode-line-update))

(defun ec-record-screen ()
  "Record the screen."
  (interactive)
  (ec--record-exec "/tmp/recording.mp4"
                   #'browse-url-xdg-open
                   "rec" (when current-prefix-arg "--show-keys")
                   "--output /tmp/recording.mp4"))

(defun ec-screenshot ()
  "Take a screenshot."
  (interactive)
  (ec--record-exec "/tmp/screenshot.png"
                   #'find-file
                   "scr" "--output /tmp/screenshot.png"))

;;; record.el ends here
