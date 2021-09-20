;;; shackle.el --- Configure buffer layout. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar ec--wconf nil "Window state before entering a branched flow state.")

(defvar-local ec--restore-wconf nil "Whether to restore state on quit.")

;; Try to use windows with the same mode.
(setq display-buffer-base-action '(display-buffer-reuse-mode-window))

(defun ec--with-restore (&optional fn &rest args)
  "Run FN if provided then restore locally stored window configuration if any.

ARGS will be passed directly through to FN."
  (let ((restore ec--restore-wconf)
        (conf ec--wconf))
    (when fn (apply fn args))
    (when (and restore conf)
      (setq ec--wconf nil)
      (set-window-configuration conf))))

(defun ec--shackle-condition (buffer mode)
  "Check if the BUFFER's major mode is MODE."
  (eq mode (buffer-local-value 'major-mode (get-buffer buffer))))

(defun ec--shackle-action (actions buffer alist plist)
  "Run ACTIONS with BUFFER and ALIST until non-nil.

Special behavior will be exhibited based on PLIST options."
  (when (plist-get plist :only)
    (with-current-buffer buffer
      ;; Keep the state from when we first entered the temporary branched flow.
      (unless ec--wconf (setq ec--wconf (current-window-configuration)))
      ;; Restore the configuration (exit branch state) when this buffer quits.
      (setq-local ec--restore-wconf t))
    ;; Don't mess with the layout if the buffer is already visible.
    (unless (get-buffer-window buffer) (delete-other-windows)))
  (when-let (window (cl-some (lambda (a) (funcall a buffer alist)) actions))
    (when (plist-get plist :focus)
      (select-window window)
      (when (fboundp 'evil-local-set-key)
        (evil-local-set-key 'normal "q" 'quit-window)))))

(defun ec--shackle (conditions actions plist)
  "Display buffer matching CONDITIONS using ACTIONS and PLIST."
  (let ((conditions (if (listp conditions) conditions (list conditions)))
        (actions (append '(display-buffer-reuse-window)
                         (if (listp actions) actions (list actions)))))
    (dolist (condition conditions)
      (add-to-list
       'display-buffer-alist
       `(,(if (symbolp condition)
              (lambda (b _) (ec--shackle-condition b condition))
            condition)
         ,(if (or (plist-get plist :only) (plist-get plist :focus))
              (lambda (b a) (ec--shackle-action actions b a plist))
            actions)
         (window-height . ,(or (plist-get plist :height) #'fit-window-to-buffer))
         (window-width  . ,(or (plist-get plist :width) (+ 2 fill-column)))
         (direction     . ,(or (plist-get plist :direction) 'right))
         (side          . ,(or (plist-get plist :side) 'right)))))))

(defun ec-shackle (rules &optional reset)
  "Display buffers using RULES after resetting if RESET is non-nil.

This is a wrapper around `display-buffer-alist' that uses a plist
instead of an alist with shortened and additional options, can
take a major mode in addition to regular expressions, and can
take multiple conditions.

Options are:
  `:height' -- Number or function, defaults to `fit-window-to-buffer'.
  `:focus'  -- Focus the buffer after display and bind q to `quit-window'.
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

  (ec-shackle '(
                ;; Split to the right.
                ("^\\*Shell Command Output\\*"
                 (display-buffer-in-direction))

                ;; Split below selected.
                ((compilation-mode shell-mode
                                   "^\\*lsp-help\\*"
                                   "^\\*Bookmark Annotation\\*$")
                 (display-buffer-below-selected) :height 0.2)

                (("^\\*Completions\\*$" )
                 (display-buffer-below-selected) :height 0.1 :focus t)

                ;; Split below selected and also focus.
                (("^\\*dig" "^\\*Ping")
                 (display-buffer-below-selected) :height 0.3 :focus t)

                ;; Bottom in a side window.
                ("\\*Notifications\\*"
                 (display-buffer-in-side-window) :height 0.1 :side bottom)

                ;; Display at the bottom.
                (("^\\*Error\\*$" "^\\*Calendar\\*" "^\\*Disabled Command\\*$")
                 (display-buffer-at-bottom) :height 0.3)

                ;; Dedicated temporary/branch state (like how `org-agenda' works
                ;; by default).
                (("^\\*Help\\*$" "^\\*ripgrep-search\\*$" "^\\*Man" "^\\*grep\\*$"
                  "^\\*Process List\\*$" "^\\*Password-Store\\*$" "^\\*eldoc\\*$"
                  "^\\*tide-references\\*" "^\\*xref\\*" "^\\*Occur\\*$")
                 (display-buffer-in-direction) :only t :focus t :width 0.5))
              t))

(add-hook 'emacs-startup-hook #'ec--setup-shackle)

;;; shackle.el ends here
