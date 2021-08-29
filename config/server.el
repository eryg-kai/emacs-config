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

(defun ec-maybe-start-servers ()
  "Start servers."
  (require 'server) ; There's no autoload on `server-running-p'.

  ;; The goal is to start these things on only one instance of Emacs, so do that
  ;; on whatever instance is running the main server. If there is no server make
  ;; this instance run the server.
  (let ((is-server (or (daemonp) (not (server-running-p)))))
    (unless (server-running-p) (server-start))
    (when is-server
      (when (fboundp 'pinentry-start) (pinentry-start))
      (when (fboundp 'osd-start) (osd-start))
      (with-eval-after-load 'org ; Regen appts when the day changes.
        (run-at-time "00:01" (* 60 60 24) #'ec--agenda-to-appt-with-timer))
      (add-hook 'org-mode-hook #'ec--hook-appt-schedule)
      (when (and (fboundp 'ec-exwm) (display-graphic-p)) (ec-exwm)))))

(add-hook 'emacs-startup-hook #'ec-maybe-start-servers)

;;; server.el ends here
