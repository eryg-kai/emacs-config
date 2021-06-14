;;; exwm.el --- EXWM configuration.-*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(when (display-graphic-p)
  (push 'exwm package-selected-packages))

(define-key global-map (kbd "C-c r") #'ec-exec)

(defcustom ec-monitor-xrandr-alist nil "Xrandr flags for each monitor.")

(defun ec-exec (&rest args)
  "Execute ARGS asynchronously without a buffer.
ARGS are simply concatenated with spaces.
If no ARGS are provided, prompt for the command."
  (interactive (list (read-shell-command "$ ")))
  (let ((command (mapconcat 'identity args " " )))
    (start-process-shell-command command nil command)))

;; Used to determine if the screen script needs to run.
(defvar ec--connected-monitors nil "Currently connected monitors.")

(setq exwm-input-global-keys
      `((,(kbd "s-q") . exwm-input-send-next-key)
        (,(kbd "s-c") . exwm-input-grab-keyboard)

        (,(kbd "<s-tab>")         . (lambda () (interactive) (ec--exwm-workspace-switch 1)))
        (,(kbd "<s-iso-lefttab>") . (lambda () (interactive) (ec--exwm-workspace-switch -1)))

        (,(kbd "s-j") . (lambda () (interactive) (ec-windmove 'down)))
        (,(kbd "s-k") . (lambda () (interactive) (ec-windmove 'up)))
        (,(kbd "s-l") . (lambda () (interactive) (ec-windmove 'right)))
        (,(kbd "s-h") . (lambda () (interactive) (ec-windmove 'left)))

        (,(kbd "<XF86MonBrightnessUp>")   . (lambda () (interactive) (ec-exec "brightness -- +5%")))
        (,(kbd "<XF86MonBrightnessDown>") . (lambda () (interactive) (ec-exec "brightness -- -5%")))
        (,(kbd "<XF86AudioLowerVolume>")  . (lambda () (interactive) (ec-exec "amixer set Master unmute 1%-")))
        (,(kbd "<XF86AudioRaiseVolume>")  . (lambda () (interactive) (ec-exec "amixer set Master unmute 1%+")))
        (,(kbd "<XF86AudioMute>")         . (lambda () (interactive) (ec-exec "amixer set Master toggle")))
        (,(kbd "<XF86AudioMicMute>")      . (lambda () (interactive) (ec-exec "amixer set Capture toggle")))))

(setq exwm-input-simulation-keys
      `((,(kbd "j")        . [down])
        (,(kbd "k")        . [up])
        (,(kbd "l")        . [right])
        (,(kbd "h")        . [left])
        (,(kbd "C-u")      . [prior])
        (,(kbd "C-d")      . [next])
        (,(kbd "C-H")      . [C-prior])
        (,(kbd "C-L")      . [C-next])
        (,(kbd "H")        . [M-left])
        (,(kbd "L")        . [M-right])
        (,(kbd "<tab>")    . [tab])
        (,(kbd "<return>") . [return])))

(setq exwm-input-line-mode-passthrough t
      exwm-manage-configurations '((t char-mode t)))

;; TODO: handle screen edges.
(defun ec-windmove (direction)
  "Move to the window in DIRECTION."
  (interactive)
  (pcase direction
    ('left (windmove-left))
    ('right (windmove-right))
    ('up (windmove-up))
    ('down (windmove-down))))

(defun ec--exwm-update-title ()
  "Rename the buffer to `exwm-title'."
  (exwm-workspace-rename-buffer (concat "*" exwm-title "*")))

(add-hook 'exwm-update-title-hook #'ec--exwm-update-title)

(defun ec--exwm-workspace-switch (n)
  "Switch to the workspace N away from the current."
  (interactive)
  (let* ((workspaceCount (exwm-workspace--count))
         (targetIndex (+ n exwm-workspace-current-index))
         (over? (>= targetIndex workspaceCount))
         (under? (< targetIndex 0)))
    (cond (over? (exwm-workspace-switch 0))
          (under? (exwm-workspace-switch (- workspaceCount 1)))
          (t (exwm-workspace-switch targetIndex)))))

(defun ec--exwm-update-screens ()
  "Update screens when they change."
  (let ((monitors
         (split-string
          (shell-command-to-string
           "xrandr 2>/dev/null | grep ' connected' | cut -d' ' -f1"))))
    (unless (equal ec--connected-monitors monitors)
      (setq ec--connected-monitors monitors)
      (let ((command (concat
                      "xrandr "
                      (mapconcat
                       (lambda (monitor) (concat
                                          "--output "
                                          monitor
                                          " "
                                          (cdr (assoc monitor ec-monitor-xrandr-alist))))
                       ec--connected-monitors
                       " "))))
        (when (bound-and-true-p ec-debug-p)
          (message (format ">>> %s" command)))
        (ec-exec command)))))

(defun ec--exwm-update-screens-soon ()
  "Update screens after idling for a bit."
  (timer-idle-debounce #'ec--exwm-update-screens 1))

(add-hook 'exwm-randr-screen-change-hook #'ec--exwm-update-screens-soon)

(defun ec-exwm ()
  "Start EXWM."
  (require 'exwm)
  (require 'exwm-randr)
  (require 'exwm-systemtray)
  (require 'exwm-config)

  (define-key exwm-mode-map (kbd "C-w") #'evil-window-map)
  (define-key exwm-mode-map (kbd "i") #'exwm-input-release-keyboard)
  (define-key exwm-mode-map (kbd ":") #'evil-ex)

  (ec--exwm-update-screens)

  (exwm-randr-enable)
  (exwm-systemtray-enable))

(with-eval-after-load 'evil
  (evil-set-initial-state 'exwm-mode 'emacs))

;;; exwm.el ends here
