;;; shackle.el --- Configure buffer layout. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar ec--wconf nil "Window state alist for branched flows.")

;; Try to use windows with the same mode.
(setq display-buffer-base-action '(display-buffer-reuse-mode-window))

;; Not sure what the point of `suspend-frame' is or how to undo it but sometimes
;; I accidentally hit the keybinding and my frame becomes unusable.
(advice-add #'suspend-frame :override #'ignore)

(defun ec--with-restore (&optional fn &rest args)
  "Run FN if provided then restore stored window configuration if any.

ARGS will be passed directly through to FN.

Window configuration is only restored if the buffer is no longer
displaying after running FN."
  (let* ((buffer (current-buffer))
         (conf (alist-get buffer ec--wconf)))
    (when fn (apply fn args))
    (when (and conf (not (get-buffer-window buffer)))
      (setq ec--wconf (assq-delete-all buffer ec--wconf))
      (set-window-configuration conf))))

(defun ec--shackle-condition (mode buffer _action)
  "Check if BUFFER's major mode is MODE."
  (eq mode (buffer-local-value 'major-mode (get-buffer buffer))))

(defun ec--shackle-action (actions plist buffer alist)
  "Run ACTIONS with BUFFER and ALIST until non-nil.

Special behavior will be exhibited based on PLIST options."
  (when (plist-get plist :only)
    (with-current-buffer buffer
      ;; Keep the state from when we first entered the temporary branched flow.
      ;; This uses a global variable rather than a local because some modes
      ;; (man) set their mode late which wipes local variables.
      (unless (assq buffer ec--wconf)
        (push `(,buffer . ,(current-window-configuration)) ec--wconf)))
    ;; Don't mess with the layout if the buffer is already visible.
    (unless (get-buffer-window buffer) (delete-other-windows)))
  (when-let (window (cl-some (lambda (a) (funcall a buffer alist)) actions))
    (when (and (plist-get plist :focus) (not (minibufferp)))
      (select-window window))
    (when (and (plist-get plist :float) (fboundp 'exwm-floating--set-floating))
    (with-current-buffer buffer
      (exwm-floating--set-floating exwm--id)))))

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
                ;; Split to the right.
                ("^\\*Shell Command Output\\*"
                 (display-buffer-in-direction))

                ;; Split below selected.
                ((compilation-mode shell-mode
                                   "^\\*lsp-help\\*"
                                   "^\\*Bookmark Annotation\\*$")
                 (display-buffer-below-selected) :height 0.2)

                (("^\\*Completions\\*$")
                 (display-buffer-below-selected) :height 0.1)

                ;; Split below selected and also focus.
                (("^\\*dig" "^\\*Ping")
                 (display-buffer-below-selected) :height 0.3 :focus t)

                ;; Bottom in a side window.
                (("\\*Notifications\\*")
                 (display-buffer-in-side-window) :height 0.1 :side bottom)

                ;; Display at the bottom.
                (("^\\*Error\\*$" "^\\*Calendar\\*" "^\\*Disabled Command\\*$")
                 (display-buffer-at-bottom) :height 0.3)

                ;; Display floating.
                (("^\\*zoom\\*$") (display-buffer-no-window) :float t)

                ;; Dedicated temporary/branch state (like how `org-agenda' works
                ;; by default).
                (("^\\*Help\\*$" "^\\*ripgrep-search\\*$" "^\\*Man" "^\\*grep\\*$"
                  "^\\*Process List\\*$" "^\\*Password-Store\\*$" "^\\*eldoc\\*$"
                  "^\\*xref\\*" "^\\*Occur\\*$")
                 (display-buffer-in-direction) :only t :focus t :width 0.5))
              t))

(add-hook 'emacs-startup-hook #'ec--setup-shackle)

;;; shackle.el ends here
