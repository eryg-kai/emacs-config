;;; news.el --- Configuration for news. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(keymap-set global-map "C-c G" #'gnus)

(setq message-directory (expand-file-name "mail" (xdg-data-home))
      gnus-directory (expand-file-name "news" (xdg-data-home))
      gnus-startup-file (expand-file-name "news/newsrc" (xdg-config-home))
      mail-host-address "system.local"
      ;; Make sure to customize `gnus-secondary-select-methods' and
      ;; `gnus-posting-styles'.
      gnus-select-method '(nnnil nil)
      gnus-use-full-window nil
      gnus-use-dribble-file nil
      gnus-user-agent nil
      gnus-inhibit-startup-message t
      mail-user-agent 'gnus-user-agent
      message-kill-buffer-on-exit t
      message-citation-line-function 'message-insert-formatted-citation-line
      message-citation-line-format "On %a, %b %d, %Y at %I:%M %p %Z %f wrote:\n")

;; Prevent Emacs from mangling the message.  Instead of reflowing leave the
;; message as it is and add a space to the end of every soft newline leaving the
;; hard newlines alone.  In other words, soft newlines will reflow on client
;; displays (space is the reflow indicator) when they are too long and hard
;; newlines will always break where they break.
(defun ec-fill-flowed-encode(&optional buffer)
  "Encode BUFFER or the current buffer for format=flowed."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (let ((pos (1- (point))))
          (unless (get-text-property pos 'hard)
            (replace-match " \n" t t)))))))

(advice-add 'fill-flowed-encode :override #'ec-fill-flowed-encode)

;; The idea is to send the email exactly as it is and assume soft newlines are
;; meant to be wrapped when they are too long for the display.
(defun ec--set-format-flowed ()
  "Set up format=flowed."
  (require 'flow-fill)
  (use-hard-newlines 1 'always)
  (setq fill-column fill-flowed-encode-column
        mml-enable-flowed t))

(add-hook 'message-mode-hook #'ec--set-format-flowed)

(with-eval-after-load 'mm-decode
  (add-to-list 'mm-discouraged-alternatives "text/html"))

;; TODO: Send delay.

(setq smtpmail-smtp-service 587
      smtpmail-stream-type 'starttls
      message-send-mail-function 'smtpmail-send-it)

;;; news.el ends here
