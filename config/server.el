;;; server.el --- Start and configure servers. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Notifications.
(defun ec-maybe-start-servers ()
  "Start servers."
  (require 'server) ; There's no autoload on `server-running-p'.

  ;; The goal is to start these things on only one instance of Emacs, so do that
  ;; on whatever instance is running the main server. If there is no server make
  ;; this instance run the server.
  (let ((is-server (or (daemonp) (not (server-running-p)))))
    (unless (server-running-p) (server-start))
    (when is-server
      (ec-battery-mode)
      (display-time-mode))))

(add-hook 'emacs-startup-hook #'ec-maybe-start-servers)

;;; server.el ends here
