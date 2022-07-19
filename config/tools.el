;;; tools.el --- Various tools. -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(nconc package-selected-packages '(docker
                                   elfeed
                                   beancount
                                   magit-annex
                                   rfc-mode
                                   speed-type
                                   pdf-tools))

;; Elfeed.
(define-key global-map (kbd "C-c e") #'elfeed)

(setq elfeed-db-directory (expand-file-name "elfeed" (xdg-cache-home)))

;; Dired.
(setq dired-listing-switches "-Fahvl --si --group-directories-first")

(advice-add 'dired-up-directory :around #'ec-run-and-bury)

(advice-add 'dired-find-file :around #'ec-run-and-bury)

;; Auth sources.
(when-let (auth-source (getenv "AUTH_SOURCE"))
  (setq auth-sources `(,auth-source)))

;; RFCs.
(setq rfc-mode-directory (expand-file-name "rfc" (xdg-cache-home)))

(define-derived-mode rfc-edit-mode text-mode "RFC-edit"
  "Mode for editing RFCs."
  (setq fill-column 72))

(add-to-list 'auto-mode-alist '("\\.rfc\\'" . rfc-edit-mode))

;; Typing practice.
(defun ec--evil-insert (&rest _)
  "Enter insert state, ignoring arguments."
  (evil-insert-state))

(advice-add #'speed-type--setup :after #'ec--evil-insert)

;; Calculator.
(setq calc-show-banner nil
      math-additional-units
      '((b nil "Bit")
        (B "b * 8" "Byte")

        (Kib "1024 * b" "Kibibit")
        (Mib "1024 * Kib" "Mebibit")
        (Gib "1024 * Mib" "Gibibit")
        (Tib "1024 * Gib" "Tebibit")
        (Pib "1024 * Tib" "Pebibit")

        (KiB "1024 * B" "Kibibyte")
        (MiB "1024 * KiB" "Mebibyte")
        (GiB "1024 * MiB" "Gibibyte")
        (TiB "1024 * GiB" "Tebibyte")
        (PiB "1024 * TiB" "Pebibyte")

        (Kb "1000 * b" "Kilobit")
        (Mb "1000 * Kb" "Megabit")
        (Gb "1000 * Mb" "Gigabit")
        (Tb "1000 * Gb" "Terabit")
        (Pb "1000 * Tb" "Petabit")

        (KB "1000 * b" "Kilobyte")
        (MB "1000 * KB" "Megabyte")
        (GB "1000 * MB" "Gigabyte")
        (TB "1000 * GB" "Terabyte")
        (PB "1000 * TB" "Petabyte")))

;; Processes.
(setq proced-format-alist
      '((short tree pcpu pmem
               (args comm))
        (medium user pid tree pcpu pmem vsize rss ttname state start time
                (args comm))
        (long user euid group pid tree pri nice pcpu pmem vsize rss ttname state start time
              (args comm))
        (verbose user euid group egid pid ppid tree pgrp sess pri nice pcpu pmem state thcount vsize rss ttname tpgid minflt majflt cminflt cmajflt start time utime stime ctime cutime cstime etime
                 (args comm))))

(defun ec--proced-format-args (fn &rest args)
  "Strip full paths from the result of FN ran with ARGS when using the short format."
  (let ((str (apply fn args)))
    (if (string= proced-format "short")
        (mapconcat #'file-name-nondirectory (split-string str " ") " ")
      str)))

(advice-add 'proced-format-args :around #'ec--proced-format-args)

;; ERC.
(setq erc-modules '(pcomplete netsplit fill button match track completion
                              readonly networks ring noncommands irccontrols
                              move-to-prompt stamp list notifications log)
      erc-header-line-format nil
      erc-mode-line-format "%n%a"
      erc-mode-line-away-status-format "{%Y-%m-%d %a %H:%M}"
      erc-log-channels-directory (expand-file-name "erc" (xdg-cache-home))
      erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t
      erc-log-insert-log-on-open t
      erc-prompt-for-password nil
      erc-track-exclude '("&bitlbee" "root")
      erc-join-buffer 'bury
      erc-query-display 'buffer
      erc-auto-query 'bury)

(defvar erc-sasl-server-regexp-list '()
  "List of regexps matching server host names for which SASL should be used.")

(defun erc-sasl-use-sasl-p ()
  "Return t if SASL should be used in the current session."
  (and (boundp 'erc-session-server)
       (cl-loop for re in erc-sasl-server-regexp-list
                thereis (integerp (string-match re erc-session-server)))))

(with-eval-after-load 'erc-backend
  (define-erc-response-handler (CAP)
    "Client capability framework is used to request SASL auth, need to wait for ACK to begin" nil
    (let ((msg (erc-response.contents parsed)))
      (when (string-match " *sasl" msg)
        (erc-server-send "AUTHENTICATE PLAIN")
        ;; now wait for AUTHENTICATE +
        )))

  (define-erc-response-handler (AUTHENTICATE)
    "Handle empty server response indicating ready to receive authentication." nil
    (if erc-session-password
        (let ((msg (erc-response.contents parsed)))
          (when (string= "+" msg)
            ;; plain auth
            (erc-server-send
             (format "AUTHENTICATE %s"
                     (base64-encode-string
                      (concat "\0" (erc-current-nick)
                              "\0" erc-session-password) t)))))
      (progn
        (erc-display-message
         parsed 'error
         (if erc-server-connected 'active proc)
         "You must set a password in order to use SASL authentication.")
        ;; aborting SASL auth
        (erc-server-send (erc-server-send "AUTHENTICATE *")))))

  (define-erc-response-handler (903)
    "Handle a successful SASL authentication." nil
    (erc-server-send "CAP END")))

(defun erc-sasl-login ()
    "Perform user authentication at the IRC server."
    (erc-log (format "login: nick: %s, user: %s %s %s :%s"
                     (erc-current-nick)
                     (user-login-name)
                     (or erc-system-name (system-name))
                     erc-session-server
                     erc-session-user-full-name))
    (if erc-session-password
        (erc-server-send (format "PASS %s" erc-session-password))
      (message "Logging in without password"))
    (when (erc-sasl-use-sasl-p) (erc-server-send "CAP REQ :sasl"))
    (erc-server-send (format "NICK %s" (erc-current-nick)))
    (erc-server-send
     (format "USER %s %s %s :%s"
             ;; hacked - S.B.
             (if erc-anonymous-login erc-email-userid (user-login-name))
             "0" "*"
             erc-session-user-full-name))
    (erc-update-mode-line))

(advice-add 'erc :around #'ec-localize)
(advice-add 'erc-login :override #'erc-sasl-login)

;; Tramp.
(setq tramp-use-ssh-controlmaster-options nil
      tramp-histfile-override t
      tramp-connection-timeout 10
      tramp-copy-size-limit nil
      tramp-use-scp-direct-remote-copying t)

(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Boomarks.
(setq bookmark-save-flag 1)

;; Beancount.
(setq beancount-number-alignment-column (- fill-column 4)
      beancount-mode-map-prefix [(control c) (\')])
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
(with-eval-after-load 'evil
  (evil-define-key 'normal beancount-mode-map
    (kbd "<tab>") 'outline-cycle
    (kbd "g TAB") 'outline-cycle
    (kbd "<backtab>") 'outline-cycle-buffer))
(add-hook 'outline-minor-mode-hook #'(lambda () (hide-sublevels 1)))
(add-hook 'beancount-mode-hook #'outline-minor-mode)

(defun ec--beancount-run (fn prog &rest args)
  "Run FN with PROG, ARGS, and the cache env var set."
  (let ((process-environment
         `(,(concat "BEANCOUNT_LOAD_CACHE_FILENAME="
                    (expand-file-name
                     (file-relative-name buffer-file-name)
                     (xdg-cache-home))
                    ".cache")
           ,@process-environment)))
    (apply fn prog args)))

(advice-add 'beancount--run :around #'ec--beancount-run)

;; Some things like `ping' don't enable `net-utils-mode'.
(defun ec--net-utils-mode (&rest _)
  "Enable `net-utils-mode'."
  (net-utils-mode)
  (read-only-mode -1))

(advice-add 'net-utils-run-program :after #'ec--net-utils-mode)

;; PDFs.
(add-hook 'emacs-startup-hook #'pdf-loader-install)

;;; tools.el ends here
