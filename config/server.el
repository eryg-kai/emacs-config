;;; server.el --- Start and configure servers. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(osd
                                   pinentry))

;; Pinentry.
(setq epa-pinentry-mode 'loopback
      pinentry-popup-prompt-window nil)

;; Notifications.
(setq appt-display-format 'window
      appt-disp-window-function 'osd-org-appt-display)

(defun ec-enable-pinentry-soon()
  (run-with-idle-timer 1 nil #'pinentry-start))

(defun ec--maybe-start-server ()
  "Start server, pinentry, and OSD."
  (require 'server) ; There's no autoload on `server-running-p'.

  ;; The goal is to start these things on only one instance of Emacs, so do that
  ;; on whatever instance is running the main server. If there is no server make
  ;; this instance run the server.
  (let ((is-server (or (daemonp) (not (server-running-p)))))
    (unless (server-running-p) (server-start))
    (when is-server
      (when (fboundp 'pinentry-start) (pinentry-start))
      (when (fboundp 'osd-start) (osd-start)))))

(add-hook 'emacs-startup-hook #'ec--maybe-start-server)

;;; server.el ends here
