;;; email.el --- Configuration for email. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq mail-user-agent 'mu4e-user-agent
      mu4e-mu-home (expand-file-name "mu" (xdg-cache-home))
      mu4e-attachment-dir (expand-file-name "mu4e" (xdg-cache-home))
      mu4e-user-agent-string nil
      mu4e-use-fancy-chars nil
      mu4e-change-filenames-when-moving t ; Need this or mbsync won't work.
      message-kill-buffer-on-exit t
      message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format "On %a, %b %d, %Y at %I:%M %p %Z %f wrote:\n"
      mu4e-headers-fields '((:account    . 16)
                            (:human-date . 10)
                            (:flags      . 4)
                            (:from       . 22)
                            (:subject    . nil))
      mu4e-date-format-long "%F %T"
      mu4e-view-date-format "%F %T"
      mu4e-headers-time-format "%T"
      mu4e-headers-date-format "%F"
      mu4e-view-show-addresses t
      mu4e-context-policy 'pick-first
      mu4e-compose-format-flowed t
      mu4e-cache-maildir-list t)

;; Skip .git directory when getting maildirs since recursing through it is slow.
(defun ec--ignore-git (fn path mdir)
  "Run FN with PATH and MDIR after ensuring MDIR is not .git."
  (unless (string= (file-name-nondirectory (directory-file-name mdir)) ".git")
    (apply fn path mdir nil)))

(advice-add 'mu4e~get-maildirs-1 :around #'ec--ignore-git)

;; Ensure mu4e runs locally (it only half-supports Tramp).
(advice-add 'mu4e :around #'ec-localize)

;; Switch to default context on exit.
(defun ec-reset-browser-profile (&rest _)
  (mu4e-context-switch nil "default"))

(advice-add 'mu4e~stop :after #'ec-reset-browser-profile)

(define-key global-map (kbd "C-c E") #'mu4e)

(with-eval-after-load 'mm-decode
  (add-to-list 'mm-discouraged-alternatives "text/html"))

(defun ec--mu4e-account (msg)
  "Return the account and folder name that MSG belongs to."
  (let ((parts (split-string (mu4e-message-field msg :maildir) "/")))
    (format "%s/%s" (cadr parts) (car (last parts)))))

;; TODO: Send delay.

(setq smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls
      message-send-mail-function 'smtpmail-send-it)

(defun ec-register-email-account (label address letvars)
  "Register ADDRESS with name LABEL for use with mu4e.

LETVARS is an alist of variables that will be set when this
account is activated."
  ;; Remove any existing context with this label.
  (setq mu4e-contexts
        (cl-loop for context in mu4e-contexts
                 unless (string= (mu4e-context-name context) label)
                 collect context))

  ;; Append the new context.
  (push
   (make-mu4e-context
    :name label
    :leave-func #'mu4e-clear-caches
    ;; Match on the `to' field. If that doesn't match try the mailbox. This
    ;; means contexts should be in order of specificity so the most general
    ;; doesn't always match first.
    :match-func
    (lambda (msg)
      (when msg
        (or (mu4e-message-contact-field-matches msg :to address)
            (string-prefix-p (format "/%s" label)
                             (mu4e-message-field msg :maildir)))))
    :vars letvars)
   mu4e-contexts))

(with-eval-after-load 'mu4e
  (add-to-list 'mu4e-header-info-custom
               '(:account
                 :name "Account"
                 :shortname "Account"
                 :function ec--mu4e-account))

  ;; Remove +T from the trash action. In Gmail this seems to cause it to remove
  ;; it from the Trash directory and it remains in All Mail. In other providers
  ;; it permanently deletes which is also not what I want.
  (setf (plist-get (alist-get 'trash mu4e-marks) :action)
        '(lambda (docid msg target)
           (mu4e--server-move docid (mu4e--mark-check-target target) "-N")))

  (setq mu4e-get-mail-command (concat "mbsync --config "
                                      (expand-file-name ".mbsyncrc" mail-source-directory)
                                      " --all"))

  ;; This file should call `ec-register-email-account' for each email account.
  ;; For example:
  ;; (ec-register-email-account
  ;;  "label" "me@domain.com"
  ;;  `((mu4e-refile-folder . "/Archive")
  ;;    (mu4e-sent-folder . "/Sent")
  ;;    (mu4e-drafts-folder . "/Drafts")
  ;;    (mu4e-trash-folder . "/Trash")
  ;;    (user-full-name . "Name")
  ;;    (user-mail-address . "me@domain.com")
  ;;    (smtpmail-smtp-user . "me@domain.com")
  ;;    (smtpmail-local-domain . "domain.com")
  ;;    (smtpmail-default-smtp-server . "smtp.domain.com")
  ;;    (smtpmail-smtp-server . "smtp.domain.com")
  ;;    (mu4e-sent-messages-behavior . 'sent) ; Use 'delete for Gmail.
  ;;    (browse-url-firefox-arguments . ("--profile" ,(expand-file-name "~/.mozilla/firefox/profile")))
  ;;    (mu4e-maildir-shortcuts . (("/Inbox" . ?i)
  ;;                               ("/Archive" . ?a)))))
  (load (expand-file-name "accounts.el" mail-source-directory) nil t))

;;; email.el ends here
