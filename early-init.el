;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:

;; This file contains the optimizations (such as temporarily disabling garbage
;; collection) and UI thrashing prevention measures (like the toolbar appearing
;; just to disappear later).

;;; Code:

;; Use --debug-init or set DEBUG to toggle this.
(defvar ec-debug-p (or (getenv "DEBUG") init-file-debug) "If non-nil log more.")
(defun ec--log-load (file)
  "Log that FILE was loaded with a backtrace."
  (when ec-debug-p
    (message "+++ Loaded %s" file)))
(push #'ec--log-load after-load-functions)

(setq debug-on-error ec-debug-p
      jka-compr-verbose ec-debug-p)

;; Defer garbage collection.
(setq gc-cons-threshold most-positive-fixnum)

;; Inhibit potentially expensive font-related operations.
(setq inhibit-compacting-font-caches t ; Uses more memory.
      frame-inhibit-implied-resize t)

;; Not applicable to Linux/X. Less to process.
(setq command-line-ns-option-alist nil)

;; Slow down UI updates (from 0.5).
(setq idle-update-delay 1.0)

;; Increase how much is read from processes.
(setq read-process-output-max (* 64 1024))

;; More performant scrolling.
(setq fast-but-imprecise-scrolling t ; Can briefly cause bad fontification.
      redisplay-skip-fontification-on-input t)

;; Disable bidirectional text rendering.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Reduce scan work by not rendering cursors or regions in non-focused windows.
(setq highlight-nonselected-windows nil
      cursor-in-non-selected-windows nil)

;; Reduce startup noise. Start with an empty scratch buffer.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; Move packages to xdg-data-home.
(setq package-user-dir "~/.local/share/emacs/elpa"
      quelpa-dir "~/.local/share/emacs/quelpa")

;; The theme has not loaded yet, so set initial colors to avoid a flash of the
;; wrong color.  Also make it fullscreen otherwise it starts small and then
;; resizes once EXWM kicks in.
;; TODO: Store background based on last used theme and use that.
;; TODO: EXWM causes a flash and the mode-line disappears for a bit.
(setq-default default-frame-alist
              '((background-color . "#282c34")
                (foreground-color . "#bbc2cf")
                (fullscreen . maximized)))
(set-face-attribute 'fringe nil :background "#282c34")

;; Disable as early as possible to prevent brief visibility.
(menu-bar-mode -1)
(when (bound-and-true-p tool-bar-mode) (tool-bar-mode -1))
(when (bound-and-true-p scroll-bar-mode) (scroll-bar-mode -1))
(when (bound-and-true-p tooltip-mode) (tooltip-mode -1))
(setq use-dialog-box nil)
(setq-default mode-line-format nil)

;; Temorarily disable file name handler alist. This can make `load' and
;; `require' a bit faster.
(setq auto-mode-case-fold nil);; No second case-insensitive pass.
(defvar ec--file-name-handler-alist file-name-handler-alist)
(setq-default file-name-handler-alist nil)
(defun ec--reset-file-handler-alist ()
  "Add back entries from file handler alist."
  (setq file-name-handler-alist
        (delete-dups (append file-name-handler-alist
                             ec--file-name-handler-alist))))
(add-hook 'emacs-startup-hook #'ec--reset-file-handler-alist)

;; Delay `tty-run-terminal-initialization'. Apparently it can cause extreme
;; slowness in terminal Emacs.
(advice-add #'tty-run-terminal-initialization :override #'ignore)
(defun ec--init-tty ()
  "Delay tty initalization."
  (advice-remove #'tty-run-terminal-initialization #'ignore)
  (tty-run-terminal-initialization (selected-frame) nil t))
(add-hook 'window-setup-hook #'ec--init-tty)

;; To get raw startup time:
;; emacs -q --eval='(message "%s" (emacs-init-time))'
(defun ec--report-startup-time ()
  "Report init and startup time."
  (message "Init in %.2fs and startup in %.2fs with %d garbage collection%s."
           (float-time (time-subtract after-init-time before-init-time))
           (float-time (time-subtract (current-time) before-init-time))
           gcs-done
           (if (= 1 gcs-done) "" "s")))
(add-hook 'emacs-startup-hook #'ec--report-startup-time 101)

(defun ec--restore-garbage-collection ()
  "Re-enable garbage collection."
  (setq gc-cons-threshold 16777216))
(defun ec--restore-garbage-collection-soon ()
  "Re-enable garbage collection after idling for a bit."
  (run-with-idle-timer 1 nil #'ec--restore-garbage-collection))
(add-hook 'emacs-startup-hook #'ec--restore-garbage-collection-soon)

;;; early-init.el ends here
