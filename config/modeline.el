;;; modeline.el --- Modeline customizations. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(anzu
                                   fancy-battery
                                   minions))

;; Selected window.
(defvar ec-selected-window nil "Currently selected window.")

(defun ec--set-selected-window (&rest args)
  "Set `ec-selected-window' ignoring ARGS."
  (unless (minibuffer-window-active-p (selected-window))
    (setq ec-selected-window (selected-window))))

(add-hook 'buffer-list-update-hook #'ec--set-selected-window)

(defun ec-is-active-window ()
  "Return t if the selected window is active."
  (eq ec-selected-window (selected-window)))

;; Search information.
(defun ec--mode-line-anzu (&rest args)
  "Call `anzu--update-mode-line-default' with ARGS then trim parentheses."
  (replace-regexp-in-string "(\\|)" ""
                            (apply #'anzu--update-mode-line-default args)))

(setq anzu-cons-mode-line-p nil
      anzu-mode-line-update-function #'ec--mode-line-anzu)

(with-eval-after-load 'anzu
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

(when (fboundp 'global-anzu-mode)
  (add-hook 'emacs-startup-hook #'global-anzu-mode))

;; Evil state.
(defun ec--modeline-state-face ()
  "Face for the current evil state (if enabled)."
  (let ((state (and (bound-and-true-p evil-local-mode) (bound-and-true-p evil-state))))
    (if state (intern (format "evil-%s-state" state)) 'mode-line)))

;; Selection.
(defun ec--column-number-at-pos (pos)
  "Column number at POS.  Analog to `line-number-at-pos'."
  (save-excursion (goto-char pos) (current-column)))

(defun ec--modeline-selection ()
  "Selection information for the mode-line."
  (when (or mark-active (and (bound-and-true-p evil-local-mode) (bound-and-true-p evil-state) (eq 'visual evil-state)))
    (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
           (chars (- (1+ (region-end)) (region-beginning)))
           (cols (1+ (abs (- (ec--column-number-at-pos (region-end))
                             (ec--column-number-at-pos (region-beginning))))))
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
(when (fboundp 'fancy-battery-mode)
  (setq fancy-battery-mode-line '(:eval (ec--modeline-battery)))

  (add-hook 'emacs-startup-hook #'fancy-battery-mode))

(defun ec--modeline-battery ()
  "Battery for the mode-line based on `fancy-battery-last-status'."
  (when-let (status fancy-battery-last-status)
    (let* ((type (cdr (assq ?L status)))
           (p (cdr (assq ?p status)))
           (percentage (cond ((not p) "")
                             ((string= "N/A" p) "")
                             (t (concat p "%%"))))
           (time (cdr (assq ?t status)))
           (left (cond ((string= "0:00" time) "")
                       ((string= "N/A"  time) "")
                       ((string= ""     time) "")
                       (t (concat "|" time)))))
      (cond ((string= "on-line" type) "")
            ((string= ""        type) "")
            (t (list " " (propertize (concat percentage left)
                                     'help-echo "Battery"
                                     'mouse-face 'mode-line-highlight
                                     'face (ec--modeline-battery-face status))))))))

(defun ec--modeline-battery-face (status)
  "Face for the mode-line battery based on STATUS."
  (let ((type (cdr (assq ?L status))))
    (if (and type (string= "AC" type)) 'fancy-battery-charging
      (pcase (cdr (assq ?b status))
        ("!"  'fancy-battery-critical)
        ("+"  'fancy-battery-charging)
        ("-"  'fancy-battery-discharging)
        (_ 'fancy-battery-discharging)))))

;; Appointment information.
(defun ec--appt-mode-line (min-to-app &optional abbrev)
  "Appointment string using list of strings MIN-TO-APP; ABBREV is ignored."
  (let* ((multiple (> (length min-to-app) 1))
         (imin (if (or (not multiple)
                       (not (delete (car min-to-app) min-to-app)))
                   (car min-to-app))))
    (if (equal imin "0") "now"
      (format "%s min" (or imin (mapconcat #'identity min-to-app ","))))))

(advice-add 'appt-mode-line :override #'ec--appt-mode-line)

;; Time and load average.
(setq display-time-string-forms
      '(" " (propertize
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

(add-hook 'emacs-startup-hook #'display-time-mode)

;; Position.
(setq mode-line-position '("%l:%C " (-3 "%p")))

;; Minions.
(setq minions-mode-line-delimiters '("" . "")
      minions-mode-line-lighter "…")

(add-hook 'emacs-startup-hook #'minions-mode)

;; Flymake.
(with-eval-after-load 'flymake
  (setq flymake-mode-line-counter-format
        (mapcar (lambda (m) (if (and (stringp m)
                                     (or (string= m "[") (string= m "]")))
                                ""
                              m))
                flymake-mode-line-counter-format)))

(defun ec--mode-line-flymake-face (fn type prop &optional default)
  "Return face for PROP for diagnostic TYPE.

When PROP is `mode-line-face' and the mode-line is inactive
return the inactive face.  In all other cases defer to FN."
  (if (and (eq prop 'mode-line-face) (not (ec-is-active-window)))
      'mode-line-inactive
    (apply fn type prop default nil)))

(advice-add 'flymake--lookup-type-property :around #'ec--mode-line-flymake-face)

;; Version control.
(defun ec--mode-line-vc (_ &optional backend)
  "Remove.BACKEND from the mode-line."
  (when (and backend (stringp vc-mode))
    (setq vc-mode (ec-center-truncate
                   (replace-regexp-in-string (format "^ %s" backend) "" vc-mode)
                   8))))

(advice-add 'vc-mode-line :after #'ec--mode-line-vc)

;; Putting it all together.
(defun ec--modeline-render (left right &optional height)
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
      (ec--modeline-render
       `("%e" ; Error about full memory.
         (:eval (when (fboundp 'winum-get-number)
                  (let ((num (format " %s" (winum-get-number))))
                    (if (ec-is-active-window)
                        (propertize num 'face (ec--modeline-state-face))
                      num))))
         (:eval (ec--modeline-selection))
         (:eval (when (bound-and-true-p anzu--state)
                  (list " " anzu--mode-line-format)))
         " " mode-line-mule-info mode-line-modified mode-line-remote
         " " (:propertize "%I"
                          help-echo "Buffer size"
                          mouse-face mode-line-highlight)
         " " (:eval (propertized-buffer-identification
                     (ec-center-truncate (format-mode-line "%b") 20)))
         (vc-mode vc-mode)
         " " ,(seq-filter (lambda (m) (not (and (stringp m) (string-blank-p m))))
                          (or (bound-and-true-p minions-mode-line-modes) mode-line-modes))
         (:eval (when (eq major-mode 'erc-mode)
                  (list " " mode-line-buffer-identification)))
         (:eval (when (bound-and-true-p flymake-mode)
                  (list " " flymake-mode-line-counters)))
         (:eval (when (and (ec-is-active-window) (fboundp 'org-clocking-p) (org-clocking-p))
                  (list " " org-mode-line-string))))
       '((:eval (when (and (ec-is-active-window) (bound-and-true-p appt-mode-string))
                  (list " " (org-trim appt-mode-string))))
         (:eval (when (and (ec-is-active-window) (or defining-kbd-macro executing-kbd-macro))
                  (list " " (propertize "•REC" 'face 'mode-line-emphasis))))
         (:eval (when (and (ec-is-active-window) (bound-and-true-p erc-modified-channels-alist))
                  (list " " (org-trim erc-modified-channels-object))))
         (:eval (when (ec-is-active-window) fancy-battery-mode-line))
         (:eval (when (ec-is-active-window) display-time-string))
         " " mode-line-position
         " ")
       1.4))))
  (ec--refresh-mode-line))

(defun ec--refresh-mode-line ()
  "Refresh the mode line in all existing buffers."
  (dolist (buffer (buffer-list))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (setq mode-line-format (default-value 'mode-line-format))))))

(add-hook 'emacs-startup-hook #'ec-set-mode-line)

;;; modeline.el ends here
