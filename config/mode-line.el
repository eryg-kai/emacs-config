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

;; Put it all together.
(defcustom ec-mode-line-format-buffer
  `("%e" ; Error about full memory.
    " " mode-line-mule-info mode-line-modified mode-line-remote
    " " (:propertize "%I"
                     help-echo "Buffer size"
                     mouse-face mode-line-highlight)
    " " (:eval (propertize (ec-center-truncate (format-mode-line "%b") 20)
                           'face 'mode-line-buffer-id))
    (vc-mode vc-mode)
    " " ,(seq-filter (lambda (m) (not (and (stringp m) (string-blank-p m))))
                     mode-line-modes)
    (:eval (when (bound-and-true-p flymake-mode)
             (list " " flymake-mode-line-counters)))
    (:eval (ec--mode-line-selection))
    (:eval (when (bound-and-true-p anzu--state)
             (list " " anzu--mode-line-format)))
    " " mode-line-position)
  "Buffer-specific mode-line."
  :type 'sexp
  :group 'mode-line)

(defcustom ec-mode-line-format-global
  '((:eval (when (or defining-kbd-macro
                     executing-kbd-macro
                     ec--record-process)
             (list " " (propertize "•REC" 'face 'mode-line-emphasis))))
    (:eval (when (and (fboundp 'org-clocking-p)
                      (org-clocking-p))
             (list " " org-mode-line-string)))
    (:eval (when (bound-and-true-p erc-modified-channels-alist)
             (list " " (string-trim erc-modified-channels-object))))
    " " (:eval ec-battery-mode-line)
    " " (:eval (bound-and-true-p display-time-string)))
  "Global mode-line."
  :type 'sexp
  :group 'mode-line)

(defvar ec--mode-line-overlays nil)

(defun ec-modeline-update (&rest _)
  "Redisplay mode-line."
  (unless (or (not ec--mode-line-overlays)
              (active-minibuffer-window))
    (let* ((buffer (format-mode-line ec-mode-line-format-buffer))
           (global (format-mode-line ec-mode-line-format-global))
           (space (propertize
                   " " 'display
                   `(space :align-to (- right-fringe
                                        ,(if (display-graphic-p) 0 1)
                                        (+ ,(string-width buffer) ,(string-width global)))))))
      (dolist (o ec--mode-line-overlays)
        (when (overlay-buffer o)
          (overlay-put o 'after-string (concat space buffer global))))
      (with-current-buffer " *Minibuf-0*"
        (erase-buffer)
        (insert space buffer global)))))

;; React to mode-line and window changes.
(advice-add 'force-mode-line-update :after #'ec-modeline-update)
(add-hook 'window-state-change-hook #'ec-modeline-update)

(defun ec-set-mode-line ()
  "Set up mode-line."
  (setq ec--mode-line-overlays nil)
  (dolist (buf '(" *Echo Area 0*" " *Echo Area 1*"))
    (with-current-buffer (get-buffer-create buf)
      (remove-overlays (point-min) (point-max))
      (push (make-overlay (point-min) (point-max) nil nil t)
            ec--mode-line-overlays)))
  (ec-modeline-update))

;; Load it later so the theme kicks in to avoid an unstyled mode line.
(add-hook 'emacs-startup-hook #'ec-set-mode-line 10)

;;; mode-line.el ends here
