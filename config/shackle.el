;;; shackle.el --- Configure buffer layout. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar ec--wconf nil "Window state alist for branched flows.")

;; Try to use windows with the same mode.
(setq display-buffer-base-action '(display-buffer-reuse-mode-window))

;; Not sure what the point of `suspend-frame' is or how to undo it but sometimes
;; I accidentally hit the keybinding and my frame becomes unusable.
(advice-add #'suspend-frame :override #'ignore)

(defun ec--restore-layout (buffer)
  "Restore the layout associated with BUFFER."
  (when-let* ((conf (alist-get buffer ec--wconf)))
    (setq ec--wconf (assq-delete-all buffer ec--wconf))
    (set-window-configuration conf)))

(defun ec--store-layout (buffer)
  "Associate the current layout with BUFFER.

If there is already a layout associated with BUFFER, do nothing."
  (unless (assq buffer ec--wconf)
    ;; Keep the state from when we first entered the temporary branched flow.
    ;; This uses a global variable rather than a local because some modes (man)
    ;; set their mode late which wipes local variables.
    (push `(,buffer . ,(current-window-configuration)) ec--wconf)))

(defun ec--with-restore (&optional fn &rest args)
  "Run FN if provided then restore stored window configuration if any.

ARGS will be passed directly through to FN.

Window configuration is only restored if the buffer is no longer
displaying after running FN."
  (let ((buffer (current-buffer)))
    (when fn (apply fn args))
    (when (not (get-buffer-window buffer))
      (ec--restore-layout buffer))))

(defun ec--shackle-condition (mode buffer _action)
  "Check if BUFFER's major mode is MODE."
  (eq mode (buffer-local-value 'major-mode (get-buffer buffer))))

(defun ec--shackle-action (actions plist buffer alist)
  "Run ACTIONS with BUFFER and ALIST until non-nil.

Special behavior will be exhibited based on PLIST options."
  (when (and (plist-get plist :only) (not (window-minibuffer-p)))
    (ec--store-layout buffer)
    ;; Don't mess with the layout if the buffer is already visible.
    (unless (get-buffer-window buffer) (delete-other-windows)))
  (when-let* ((window (cl-some (lambda (a) (funcall a buffer alist)) actions)))
    (when (and (plist-get plist :focus) (not (window-minibuffer-p)))
      (select-window window))
    (when (and (plist-get plist :float) (fboundp 'exwm-floating--set-floating))
      (with-current-buffer buffer
        (exwm-floating--set-floating exwm--id)))
    ;; Returning the window lets `display-buffer' know it should stop.
    window))

(defun ec--shackle (conditions actions plist)
  "Display buffer matching CONDITIONS using ACTIONS and PLIST."
  (let ((conditions (if (listp conditions) conditions (list conditions)))
        (actions (append '(display-buffer-reuse-window)
                         (if (listp actions) actions (list actions)))))
    (dolist (condition conditions)
      (add-to-list
       'display-buffer-alist
       `(,(if (symbolp condition)
              (apply-partially #'ec--shackle-condition condition)
            condition)
         ,(apply-partially #'ec--shackle-action actions plist)
         (window-height . ,(or (plist-get plist :height) #'fit-window-to-buffer))
         (window-width  . ,(or (plist-get plist :width) (+ 2 fill-column)))
         (direction     . ,(or (plist-get plist :direction) 'right))
         (side          . ,(or (plist-get plist :side) 'right))
         (allow-no-window . t))))))

(defun ec-shackle (rules &optional reset)
  "Display buffers using RULES after resetting if RESET is non-nil.

This is a wrapper around `display-buffer-alist' that uses a plist
instead of an alist with shortened and additional options, can
take a major mode in addition to regular expressions, and can
take multiple conditions.

Options are:
  `:height' -- Number or function, defaults to `fit-window-to-buffer'.
  `:focus'  -- Focus the buffer after display.
  `:width'  -- Number or function, defaults to 2 + `fill-column'.
  `:side'   -- Side for directional displays.
  `:only'   -- Delete other windows temporarily (like `org-agenda')."
  (when reset (setq display-buffer-alist nil))
  (dolist (rule rules)
    (ec--shackle
     (car rule)
     (cadr rule)
     (cddr rule))))

(defun ec--setup-shackle ()
  "Set up shackle rules."
  (advice-add 'quit-window :around #'ec--with-restore)
  (advice-add 'quit-restore-window :around #'ec--with-restore)
  (advice-add 'kill-buffer :around #'ec--with-restore)

  (ec-shackle '(
                ;; Split below selected.
                (("^\\*lsp-help\\*"
                  "^\\*Bookmark Annotation\\*$")
                 (display-buffer-below-selected) :height 0.2)

                (("^\\*Completions\\*$")
                 (display-buffer-below-selected) :height 0.5)

                ;; Split below selected and also focus.
                (("^\\*dig" "^\\*Ping"
                  compilation-mode
                  "^\\*Shell Command Output\\*$" "^\\*Async Shell Command\\*$")
                 (display-buffer-below-selected) :height 0.3 :focus t)

                ;; Bottom in a side window.
                (("\\*Notifications\\*" "\\*Warnings\\*")
                 (display-buffer-in-side-window) :height 0.1 :side bottom)

                ;; Display at the bottom.
                (("^\\*Error\\*$" "^\\*Calendar\\*" "^\\*Disabled Command\\*$")
                 (display-buffer-at-bottom) :height 0.3)

                ;; Display floating.
                (("^\\*[zZ]oom\\*$") (display-buffer-no-window) :float t)

                (("^\\*Dictionary\\*")
                 (display-buffer-in-direction) :only t :width 0.5)

                ;; Dedicated temporary/branch state (like how `org-agenda' works
                ;; by default).
                (("^\\*Help\\*$" "^\\*ripgrep-search\\*$" "^\\*Man" "^\\*grep\\*$"
                  "^\\*Process List\\*$" "^\\*Password-Store\\*$" "^\\*eldoc\\*$"
                  "^\\*xref\\*" "^\\*Occur\\*$" "^\\*info\\*$"
                  "^\\*docker-images\\*" "^\\*docker-containers\\*"
                  Buffer-menu-mode proced-mode)
                 (display-buffer-in-direction) :only t :focus t :width 0.5))
              t))

(add-hook 'emacs-startup-hook #'ec--setup-shackle)

;; Calc does not seem to use `display-buffer' and it does not seem to call
;; `quit-window' either so the shackle code is duplicated a little here.  The
;; goal is to display the current buffer only with the calculator and trail on
;; the right then restore the previous layout on quit.
(defun ec--display-calc-window ()
  "Display the calc window."
  (let ((buffer (current-buffer)))
    (with-current-buffer buffer
      (unless (assq buffer ec--wconf)
        (push `(,buffer . ,(current-window-configuration)) ec--wconf)))
    (unless (get-buffer-window buffer) (delete-other-windows))
    (select-window (display-buffer-in-side-window buffer '((side . right)
                                                           (slot . -1))))))

(defun ec--display-calc-trail-window ()
  "Display the calc trail window."
  (display-buffer-in-side-window (current-buffer) '((side . right))))

(add-hook 'calc-window-hook #'ec--display-calc-window)
(add-hook 'calc-trail-window-hook #'ec--display-calc-trail-window)
(advice-add 'calc-quit :around #'ec--with-restore)

;; Restore layout after ediff.
(defun ec--before-ediff (&rest _)
  "Store window layout."
  (ec--store-layout nil))

(defun ec--after-ediff ()
  "Restore window layout."
  (ec--restore-layout nil))

;; Ideally you could use `ediff-before-setup-hook' to store the previous window
;; configuration, but it is called too late when using region diffs, because you
;; end up storing the split of the region selection layout itself that shows up
;; before ediff is launched.  Because of this, avoid the hook.
(advice-add #'ediff-buffers :before #'ec--before-ediff)
(advice-add #'ediff-regions-linewise :before #'ec--before-ediff)
(advice-add #'ediff-regions-wordwise :before #'ec--before-ediff)
(add-hook 'ediff-quit-hook #'ec--after-ediff)

;;; shackle.el ends here
