;;; mode-line.el --- Mode-line customizations. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(anzu minions))

;; Search information.
(defun ec--mode-line-anzu (&rest args)
  "Call `anzu--update-mode-line-default' with ARGS then trim parentheses."
  (replace-regexp-in-string "(\\|)" ""
                            (apply #'anzu--update-mode-line-default args)))

(setq anzu-cons-mode-line-p nil
      anzu-mode-line-update-function #'ec--mode-line-anzu)

(with-eval-after-load 'anzu
  (keymap-set global-map "<remap> <query-replace>" #'anzu-query-replace)
  (keymap-set global-map "<remap> <query-replace-regexp>" #'anzu-query-replace-regexp)
  (keymap-set isearch-mode-map "<remap> <isearch-query-replace>"  #'anzu-isearch-query-replace)
  (keymap-set isearch-mode-map "<remap> <isearch-query-replace-regexp>" #'anzu-isearch-query-replace-regexp))

(when (fboundp 'global-anzu-mode)
  (add-hook 'emacs-startup-hook #'global-anzu-mode))

;; Evil state.
(defun ec--mode-line-state-face ()
  "Face for the current evil state (if enabled)."
  (let ((state (and (bound-and-true-p evil-local-mode) (bound-and-true-p evil-state))))
    (if state (intern (format "evil-%s-state" state)) 'mode-line)))

;; Selection.
(defun ec--mode-line-selection ()
  "Selection information for the mode-line."
  (when (or mark-active (and (bound-and-true-p evil-local-mode) (bound-and-true-p evil-state) (eq 'visual evil-state)))
    (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
           (chars (- (1+ (region-end)) (region-beginning)))
           (cols (1+ (abs (- (ec-column-number-at-pos (region-end))
                             (ec-column-number-at-pos (region-beginning))))))
           (is-visual (and (bound-and-true-p evil-state) (eq 'visual evil-state)))
           (evil-selection (and is-visual (bound-and-true-p evil-visual-selection)))
           (rect (and (> lines 1) (or (bound-and-true-p rectangle-mark-mode)
                                      (eq 'block evil-selection))))
           (multi-line (or (> lines 1) (eq 'line evil-selection))))
      (cond (rect (format " %d×%d" lines (if is-visual cols (1- cols))))
            (multi-line (format " %d" lines))
            (t (let ((chars (if is-visual chars (1- chars))))
                 (format " %d" chars)))))))

;; Org clock.
(setq org-clock-string-limit 20)

(defun ec--org-clock-get-clock-string (fn &rest args)
  "Call FN with ARGS then customize the returned string."
  (let* ((str (replace-regexp-in-string "^ \\|(\\|)\\|\\[\\|\\]" "" (apply fn args)))
         (index (string-match-p " " str))
         (time (substring str 0 index))
         (body (substring str (+ 1 index)))
         (len (- org-clock-string-limit (length time))))
    (concat time "|" (string-replace " " "" (ec-center-truncate body (- len 1))))))

(advice-add 'org-clock-get-clock-string :around #'ec--org-clock-get-clock-string)

;; The mode-line is not updated by default when clocking out.
(add-hook 'org-clock-out-hook #'org-clock-update-mode-line)

;; Battery.
(defvar ec-battery-mode-line nil "Mode line string for battery information.")

;; `display-battery-mode' exists but it does not give you separate laptop and
;; headset battery information.  Instead, use it for the dbus subscription and
;; parsing each battery's info but put together the mode line manually.
(defun ec-battery-mode ()
  "Start polling batteries."
  (require 'battery)
  (setq battery-update-timer
        (run-at-time nil battery-update-interval #'ec--battery-update))
  ;; Listen for being plugged in/out.
  (when (featurep 'dbusbind)
    (battery--upower-subscribe)))

(defun ec--battery-update ()
  "Update variable `ec-battery-mode-line'."
  (setq ec-battery-mode-line
        (mapconcat #'ec--mode-line-battery (ec-battery) " "))
  ;; Force because it might have been triggered by dbus.
  (force-mode-line-update t))

(defun ec--battery-signal-handler (&rest _ignore)
  "Run the battery update now."
  (cancel-timer battery-update-timer)
  (setq battery-update-timer
        (run-at-time nil battery-update-interval #'ec--battery-update)))

;; `battery--upower-signal-handler' runs `timer-event-handler' which for some
;; reason after running the timer increases the delay by the repeat time instead
;; of resetting the delay (so from 60 seconds to 2 minutes to 3 minutes, etc).
;; At some point I had a delay of over an hour, I think because my cable was bad
;; and kept tripping the signal.
(advice-add #'battery--upower-signal-handler :override #'ec--battery-signal-handler)

(defun ec-battery ()
  "Get battery percentages for all devices."
  (seq-filter
   (lambda (props)
     (let ((type (cdr (assoc "Type" props))))
       (or (eq type 2) (eq type 19)))) ;; 2 == battery, 19 == headphones
   (mapcar #'battery--upower-device-properties (battery--upower-devices))))

(defun ec--mode-line-battery (props)
  "Turn battery PROPS into a mode line string.

If battery is low, send a notification."
  (let* ((state   (battery--upower-state props nil))
         (percent (cdr (assoc "Percentage"  props)))
         (model   (cdr (assoc "Model"       props)))
         (tte     (cdr (assoc "TimeToEmpty" props)))
         (ttf     (cdr (assoc "TimeToFull"  props)))
         (secs (if (< 0 tte) tte ttf))
         (mins (/ secs 60))
         (hrs (/ secs 3600))
         (left (if (or (< 0 mins) (< 0 hrs))
                   (format " (%d:%02d)" hrs (% mins 60))
                 ""))
         (face (cond ((eq state 'fully-charged) 'success)
                     ((eq state 'charging) 'success)
                     ((< percent battery-load-critical) 'error)
                     ((< percent battery-load-low) 'warning)
                     ('warning))))
    (when (and (eq state 'discharging) (> battery-load-low percent))
      (osd-notify (list (format "%s battery is %.0f%%" model percent)
                        "poweroff imminent" "emacs")))
    (cond (t (propertize (format "%.0f%%%%%s" percent left)
                         'help-echo (format "%s battery" model)
                         'mouse-face 'mode-line-highlight
                         'face face)))))

;; Appointment information.
(defvar ec-mode-line-agenda-keymap
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<mode-line> <mouse-1>" #'org-agenda-list)
    map) "\
Keymap for managing appointments in mode-line.")

(defun ec--appt-mode-line (min-to-app &optional abbrev)
  "Appointment string using list of strings MIN-TO-APP; ABBREV is ignored."
  (let* ((multiple (> (length min-to-app) 1))
         (imin (if (or (not multiple)
                       (not (delete (car min-to-app) min-to-app)))
                   (car min-to-app))))
    (propertize
     (if (equal imin "0") "now"
       (format "%s min" (or imin (mapconcat #'identity min-to-app ","))))
     'help-echo "mouse-1: Open agenda"
     'local-map ec-mode-line-agenda-keymap
     'mouse-face 'mode-line-highlight)))

(advice-add 'appt-mode-line :override #'ec--appt-mode-line)

;; Time and load average.
(setq display-time-string-forms
      '((propertize
         ;; Use this instead of the default to get rid of the space and to
         ;; move the menu to `mouse-1'.
         (format "%.2f" (nth (or display-time-load-average 0) (load-average t)))
         'local-map (make-mode-line-mouse-map
                     'mouse-1 'display-time-next-load-average)
         'mouse-face 'mode-line-highlight
         'help-echo (concat
                     "System load average for past "
                     (pcase display-time-load-average
                       (0 "1 minute")
                       (1 "5 minutes")
                       (_ "15 minutes"))
                     "; mouse-1: next"))
        ;; Use this instead of the default to customize the help echo.
        " " (propertize (concat 24-hours ":" minutes)
                        'help-echo (format-time-string "%Y-%m-%d %a %H:%M" now)
                        'mouse-face 'mode-line-highlight
                        'face 'mode-line-emphasis)))

;; Position.
(setq mode-line-position '("%l:%C " (-3 "%p")))

;; Modes.
(setq mode-line-modes-delimiters '("" . "")
      mode-line-collapse-minor-modes-to "…"
      mode-line-collapse-minor-modes t
      minions-mode-line-lighter "…")

(when (fboundp 'minions-mode)
  (add-hook 'emacs-startup-hook #'minions-mode))

;; Flymake.
(with-eval-after-load 'flymake
  (setq flymake-mode-line-counter-format
        (mapcar (lambda (m) (if (and (stringp m)
                                     (or (string= m "[") (string= m "]")))
                                ""
                              m))
                (bound-and-true-p flymake-mode-line-counter-format))))

(defun ec--mode-line-flymake-face (fn type prop &optional default)
  "Return face for PROP for diagnostic TYPE.

When PROP is `mode-line-face' and the mode-line is inactive
return the inactive face.  In all other cases defer to FN."
  (if (and (eq prop 'mode-line-face) (not (mode-line-window-selected-p)))
      'mode-line-inactive
    (apply fn type prop default nil)))

(advice-add 'flymake--lookup-type-property :around #'ec--mode-line-flymake-face)

;; Version control.
(defun ec--mode-line-vc (_ &optional backend)
  "Remove BACKEND from the mode-line."
  (when (and backend (stringp vc-mode))
    (setq vc-mode (ec-center-truncate
                   (replace-regexp-in-string (format "^ %s" backend) "" vc-mode)
                   8))))

(advice-add 'vc-mode-line :after #'ec--mode-line-vc)

;; Windows and buffers.
(defvar ec-mode-line-window-keymap
  (let ((map (make-sparse-keymap)))
    (keymap-set map "<mode-line> <mouse-1>" `(menu-item "Menu Bar" ignore :filter (lambda (_) (mouse-menu-bar-map))))
    (keymap-set map "<mode-line> <mouse-2>" #'ec-exwm-workspace-prev)
    (keymap-set map "<mode-line> <mouse-3>" #'ec-exwm-workspace-next)
    map) "\
Keymap for managing windows in mode-line.")

(keymap-set mode-line-buffer-identification-keymap "<mode-line> <mouse-1>" #'mouse-buffer-menu)
(keymap-set mode-line-buffer-identification-keymap "<mode-line> <mouse-2>" #'mode-line-previous-buffer)

;; Putting it all together.
(defun ec--mode-line-render (left right &optional height)
  "Return mode-line with LEFT and RIGHT aligned and made HEIGHT tall."
  (list
   ;; HACK: This zero-width character is used to fake vertical padding.
   (when (and height (display-graphic-p))
     (propertize "\u200b" 'display `((raise ,(/ (1- height) -2.0)) (height ,height))))
   left
   (propertize " " 'display `((space :align-to
                                     (- (+ right right-fringe right-margin)
                                        ,(string-width (format-mode-line right))))))
   right))

(defun ec-set-mode-line ()
  "Customize the mode line."
  (setq-default
   mode-line-format
   '((:eval
      (ec--mode-line-render
       `("%e" ; Error about full memory.
         " " (:eval (propertize (if (bound-and-true-p winum-mode)
                                    (winum-get-number-string)
                                  "?")
                                'face (when (mode-line-window-selected-p)
                                        (ec--mode-line-state-face))
                                'mouse-face 'mode-line-highlight
                                'local-map ec-mode-line-window-keymap
                                'help-echo "mouse-1: Display global menu\nmouse-2: Previous frame\nmouse-3: Next frame"))
         (:eval (ec--mode-line-selection))
         (:eval (when (bound-and-true-p anzu--state)
                  (list " " anzu--mode-line-format)))
         " " mode-line-mule-info mode-line-modified mode-line-remote
         " " (:propertize "%I"
                          help-echo "Buffer size"
                          mouse-face mode-line-highlight)
         " " (:eval (propertize (ec-center-truncate (format-mode-line "%b") 20)
                                'face 'mode-line-buffer-id
                                'help-echo "Buffer name mouse-1: Display buffer menu\nmouse-2: Previous buffer\nmouse-3: Next buffer"
                                'mouse-face 'mode-line-highlight
                                'local-map mode-line-buffer-identification-keymap))
         (vc-mode vc-mode)
         " " ,(seq-filter (lambda (m) (not (and (stringp m) (string-blank-p m))))
                          mode-line-modes)
         (:eval (when (eq major-mode 'erc-mode)
                  (list " " mode-line-buffer-identification)))
         (:eval (when (bound-and-true-p flymake-mode)
                  (list " " flymake-mode-line-counters)))
         (:eval (when (and (mode-line-window-selected-p)
                           (fboundp 'org-clocking-p)
                           (org-clocking-p))
                  (list " " org-mode-line-string))))
       '((:eval (when (and (mode-line-window-selected-p)
                           (bound-and-true-p appt-mode-string))
                  (list " " (string-trim appt-mode-string))))
         (:eval (when (and (mode-line-window-selected-p)
                           (or defining-kbd-macro
                               executing-kbd-macro
                               ec--record-process))
                  (list " " (propertize "•REC" 'face 'mode-line-emphasis))))
         (:eval (when (and (mode-line-window-selected-p)
                           (bound-and-true-p erc-modified-channels-alist))
                  (list " " (string-trim erc-modified-channels-object))))
         (:eval (when (mode-line-window-selected-p)
                  (list " " ec-battery-mode-line
                        " " (bound-and-true-p display-time-string))))
         " " mode-line-position
         " ")
       1.2))))
  (ec--refresh-mode-line))

(defun ec--refresh-mode-line ()
  "Refresh the mode line in all existing buffers."
  (dolist (buffer (buffer-list))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (setq mode-line-format (default-value 'mode-line-format))))))

;; Load it later so the theme kicks in to avoid an unstyled mode line.
(add-hook 'emacs-startup-hook #'ec-set-mode-line 10)

;;; mode-line.el ends here
