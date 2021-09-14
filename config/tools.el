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

(setq elfeed-db-directory (expand-file-name "elfeed" ec-cache-dir))

;; Dired.
(setq dired-listing-switches "-Fahvl --si --group-directories-first")

(advice-add 'dired-up-directory :around #'ec-run-and-bury)

(advice-add 'dired-find-file :around #'ec-run-and-bury)

;; Auth sources.
(when-let (auth-source (getenv "AUTH_SOURCE"))
  (setq auth-sources `(,auth-source)))

;; RFCs.
(setq rfc-mode-directory (expand-file-name "rfc" ec-cache-dir))

(define-derived-mode rfc-edit-mode text-mode "RFC-edit"
  "Mode for editing RFCs."
  (setq fill-column 72))

(add-to-list 'auto-mode-alist '("\\.rfc\\'" . rfc-edit-mode))

;; Typing practice.
(defun ec--evil-insert (&rest _)
  "Enter insert state, ignoring arguments"
  (evil-insert-state))

(advice-add #'speed-type--setup :after #'ec--evil-insert)

;; Calculator.
(setq math-additional-units
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
  "Run FN with ARGS then return only the process without the path."
  (let ((parts (split-string (apply fn args) " ")))
    (file-name-nondirectory (car parts))))

(advice-add 'proced-format-args :around #'ec--proced-format-args)

;; ERC.
(setq erc-modules '(pcomplete netsplit fill button match track completion
                              readonly networks ring noncommands irccontrols
                              move-to-prompt stamp list)
      erc-prompt-for-password nil)

;; Tramp.
(setq tramp-use-ssh-controlmaster-options nil)

(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Boomarks.
(setq bookmark-save-flag 1)

;; Beancount.
(setq beancount-number-alignment-column 52)
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
                     ec-cache-dir)
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

;;; tools.el ends here
