;;; exwm.el --- EXWM configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(when (display-graphic-p)
  (nconc package-selected-packages '(exwm exwm-edit)))

(keymap-set global-map "C-c r" #'ec-exec)

(defun ec--enable-exwm ()
  "Enable EXWM."
  (interactive)
  (ec-exwm-update-screens)
  (exwm-randr-mode)
  (exwm-xim-mode)
  (exwm-wm-mode))

(keymap-set global-map "s-x" #'ec--enable-exwm)

(defcustom ec-monitor-xrandr-alist nil "Xrandr flags for each monitor."
  :type '(alist :key-type string :value-type string)
  :group 'exwm)

(setq x-no-window-manager t)

;; Used to determine if the screen script needs to run.
(defvar ec--connected-monitors nil "Currently connected monitors.")

(setq exwm-input-global-keys
      `((,(kbd "s-q") . exwm-input-send-next-key)
        (,(kbd "s-c") . exwm-input-grab-keyboard)
        (,(kbd "s-e") . exwm-edit--compose)

        (,(kbd "<s-tab>")         . ec-exwm-workspace-next)
        (,(kbd "<s-iso-lefttab>") . ec-exwm-workspace-prev)

        (,(kbd "<M-tab>")         . ec-exwm-frame-next)
        (,(kbd "<M-iso-lefttab>") . ec-exwm-frame-prev)

        (,(kbd "<C-tab>")         . ec-exwm-next-buffer)
        (,(kbd "<C-iso-lefttab>") . ec-exwm-previous-buffer)

        (,(kbd "<C-\\>") . toggle-input-method)

        (,(kbd "s-t") . exwm-floating-toggle-floating)

        (,(kbd "<f5>") . ec-screenshot)
        (,(kbd "<f6>") . ec-record-screen)

        (,(kbd "s-j") . evil-window-down)
        (,(kbd "s-k") . evil-window-up)
        (,(kbd "s-l") . evil-window-right)
        (,(kbd "s-h") . evil-window-left)
        (,(kbd "<s-left>") . (lambda () (interactive) (ec-exwm-rotate-screen 'left)))
        (,(kbd "<s-up>") . (lambda () (interactive) (ec-exwm-rotate-screen 'inverted)))
        (,(kbd "<s-right>") . (lambda () (interactive) (ec-exwm-rotate-screen 'right)))
        (,(kbd "<s-down>") . (lambda () (interactive) (ec-exwm-rotate-screen 'normal)))

        (,(kbd "s-J") . evil-window-decrease-height)
        (,(kbd "s-K") . evil-window-increase-height)
        (,(kbd "s-L") . evil-window-increase-width)
        (,(kbd "s-H") . evil-window-decrease-width)

        (,(kbd "<XF86MonBrightnessUp>")   . (lambda () (interactive) (ec-exec "light -A 5")))
        (,(kbd "<XF86MonBrightnessDown>") . (lambda () (interactive) (ec-exec "light -U 5")))

        (,(kbd "<XF86AudioLowerVolume>")  . ec-decrease-volume)
        (,(kbd "<XF86AudioRaiseVolume>")  . ec-increase-volume)
        (,(kbd "<XF86AudioMute>")         . ec-toggle-muted)
        (,(kbd "<XF86AudioMicMute>")      . ec-toggle-microphone)

        (,(kbd "<XF86AudioPause>")        . ec-play-pause)
        (,(kbd "<XF86AudioPlay>")         . ec-play-pause)
        (,(kbd "<XF86AudioPrev>")         . ec-play-previous)
        (,(kbd "<XF86AudioNext>")         . ec-play-next)))

(setq exwm-input-simulation-keys
      `((,(kbd "j")        . [down])
        (,(kbd "k")        . [up])
        (,(kbd "l")        . [right])
        (,(kbd "h")        . [left])
        (,(kbd "C-u")      . [prior])
        (,(kbd "C-d")      . [next])
        (,(kbd "C-n")      . [C-n])     ;; New window.
        (,(kbd "H")        . [M-left])  ;; Back in history.
        (,(kbd "L")        . [M-right]) ;; Forward in history.
        (,(kbd "<tab>")    . [tab])
        (,(kbd "<return>") . [return])))

(setq exwm-input-line-mode-passthrough t
      frame-alpha-lower-limit 0
      exwm-floating-border-width 10)

(defun ec--setup-floating ()
  "Set up a floating frame."
  (set-frame-parameter (selected-frame) 'alpha 90)
  ;; Prevent cursor from showing through.
  (setq cursor-in-non-selected-windows nil))

(add-hook 'exwm-floating-setup-hook #'ec--setup-floating)

;; Launch with emacs -f ec-float
(defun ec-float ()
  "Set up Emacs inside a floating frame."
  (set-frame-parameter (selected-frame) 'alpha-background 0))

;; TODO: Any way to restrict `other-frame' to the frames belonging to the
;; current workspace?
(defun ec-exwm-frame-prev ()
  "Move to the previous frame."
  (interactive)
  (other-frame -1))

(defun ec-exwm-frame-next ()
  "Move to the next frame."
  (interactive)
  (other-frame 1))

(defun ec-exwm-previous-buffer ()
  "Switch to the previous exwm buffer."
  (interactive)
  (let ((switch-to-prev-buffer-skip
         (lambda (window new-buffer bury-or-kill)
           (not (eq 'exwm-mode (with-current-buffer new-buffer major-mode))))))
    (previous-buffer)))

(defun ec-exwm-next-buffer ()
  "Switch to the next exwm buffer."
  (interactive)
  (let ((switch-to-prev-buffer-skip
         (lambda (window new-buffer bury-or-kill)
           (not (eq 'exwm-mode (with-current-buffer new-buffer major-mode))))))
    (next-buffer)))

(defun ec--exwm-update-title ()
  "Rename the buffer to `exwm-title'."
  (exwm-workspace-rename-buffer (concat "*" exwm-title "*")))

(add-hook 'exwm-update-title-hook #'ec--exwm-update-title)

(defun ec--exwm-workspace-switch (n)
  "Switch to the workspace N away from the current."
  (let* ((workspaceCount (exwm-workspace--count))
         (targetIndex (+ n exwm-workspace-current-index))
         (over? (>= targetIndex workspaceCount))
         (under? (< targetIndex 0)))
    (cond (over? (exwm-workspace-switch 0))
          (under? (exwm-workspace-switch (- workspaceCount 1)))
          (t (exwm-workspace-switch targetIndex)))))

(defun ec-exwm-workspace-prev ()
  "Move to the previous workspace."
  (interactive)
  (ec--exwm-workspace-switch -1))

(defun ec-exwm-workspace-next ()
  "Move to the next workspace."
  (interactive)
  (ec--exwm-workspace-switch 1))

(defun ec-exwm-update-screens ()
  "Update screens when they change."
  (interactive)
  (let* ((default-directory "~")
         (xrandr (shell-command-to-string "xrandr"))
         (monitors
          (mapcar (lambda (s) (car (split-string s " ")))
                  (seq-filter (lambda (s) (string-match " connected" s))
                              (split-string xrandr "\n")))))
    ;; If there are multiple monitors, disable the internal screen.
    ;; TODO: Do so only if lid is closed and turn back on if lid is opened.
    (when (length> monitors 1) (setq monitors (delete "eDP-1" monitors)))
    (unless (and (not (called-interactively-p)) (equal ec--connected-monitors monitors))
      (let ((command (concat
                      "xrandr "
                      (mapconcat
                       (lambda (m) (format "--output %s %s" m (cdr (assoc m ec-monitor-xrandr-alist))))
                       monitors
                       " ")
                      " "
                      (mapconcat
                       (lambda (m) (format "--output %s --off" m))
                       (seq-difference ec--connected-monitors monitors)
                       " "))))
        (unless monitors
          (message ">>> %s" xrandr)
          (error "Refusing to turn off all monitors"))
        (setq ec--connected-monitors monitors)
        (ec-exec command)))))

(defcustom ec-touchscreen-plist nil "Xrandr and xinput ids of the touchscreen."
  :type '(plist :key-type (choice (const :xrandr) (const :xinput))
                :value-type (choice string (repeat string)))
  :group 'exwm)

(defvar ec--monitor-rotation-transformations '((normal . "1 0 0 0 1 0 0 0 1")
                                               (inverted . "-1 0 1 0 -1 1 0 0 1")
                                               (left . "0 -1 1 1 0 0 0 0 1")
                                               (right . "0 1 0 -1 0 1 0 0 1"))
  "Transformation matrices for monitor rotations.")

(defun ec-exwm-rotate-screen (new-rotation)
  "Rotate a connected monitor to NEW-ROTATION.

If the monitor is a touchscreen also adjust the touch input."
  (interactive)
  (let* ((monitor (if (= 1 (length ec--connected-monitors))
                      (car ec--connected-monitors)
                    (completing-read "Monitor: " ec--connected-monitors nil t)))
         (default-directory "~")
         (xrandr (shell-command-to-string "xrandr"))
         (command (format "xrandr --output %s --rotate %s"
                          monitor
                          new-rotation))
         (rotation (alist-get new-rotation ec--monitor-rotation-transformations)))
    (message ">>> %s" command)
    (ec-exec command)
    (when (string= monitor (plist-get ec-touchscreen-plist :xrandr))
      (dolist (device (plist-get ec-touchscreen-plist :xinput))
        (let ((command (format
                        "xinput set-prop '%s' 'Coordinate Transformation Matrix' %s"
                        device
                        rotation)))
          (message ">>> %s" command)
          (ec-exec command))))))

(defvar ec--exwm-timer nil "EXWM screen update timer.")

(defun ec--exwm-update-screens-soon ()
  "Update screens soon."
  (when ec--exwm-timer (cancel-timer ec--exwm-timer))
  (setq ec--exwm-timer (run-with-idle-timer 5 nil #'ec-exwm-update-screens)))

(add-hook 'exwm-randr-screen-change-hook #'ec--exwm-update-screens-soon)

(with-eval-after-load 'exwm
  ;; There is some weird behavior with prompting for input after EXWM has begun
  ;; to shut down.  It is also a bit annoying to be asked twice because there is
  ;; already a shutdown confirmation from EXWM.
  (setq confirm-kill-processes nil)

  (setq exwm-manage-configurations `(((string= exwm-instance-name "Navigator")
                                      floating-mode-line nil
                                      x 10
                                      y 10
                                      width ,(* (frame-char-width) 80)
                                      height ,(- (x-display-pixel-height) 130)
                                      char-mode t)
                                     (t floating-mode-line nil
                                        char-mode t)))

  ;; Do not remember desktop position, always open on current.
  (advice-add #'exwm--on-net-wm-desktop :override #'ignore)

  (keymap-set exwm-mode-map "C-w" #'evil-window-map)
  (keymap-set exwm-mode-map "i" #'exwm-input-release-keyboard)
  (keymap-set exwm-mode-map ":" #'evil-ex)

  (with-eval-after-load 'evil
    (evil-set-initial-state 'exwm-mode 'emacs)))

;;; exwm.el ends here
