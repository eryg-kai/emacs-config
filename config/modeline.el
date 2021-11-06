;;; modeline.el --- Modeline customizations. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(anzu
                                   fancy-battery))

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
(setq anzu-cons-mode-line-p nil)

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
      (cond (rect (format " (%d×%d block)" lines (if is-visual cols (1- cols))))
            (multi-line (format " (%d line%s)" lines (if (= 1 lines) "" "s")))
            (t (let ((chars (if is-visual chars (1- chars))))
                 (format " (%d char%s)" chars (if (= 1 chars) "" "s"))))))))

;; Org clock.
(defun ec--modeline-org-clock ()
  "Org clock for the mode-line."
  (when (and (fboundp 'org-clocking-p) (org-clocking-p))
    (concat
     (ec-center-truncate
      (substring-no-properties (org-clock-get-clock-string))
      19))))

;; Battery.
(when (fboundp 'fancy-battery-mode)
  (add-hook 'emacs-startup-hook #'fancy-battery-mode))

(defun ec--modeline-battery ()
  "Battery for the mode-line based on `fancy-battery-last-status'."
  (when-let (status (and (bound-and-true-p fancy-battery-last-status)))
    (let* ((type (cdr (assq ?L status)))
           (p (cdr (assq ?p status)))
           (percentage (cond ((not p) "")
                             ((string= "N/A" p) "")
                             (t (concat " " p "%%"))))
           (time (cdr (assq ?t status)))
           (left (cond ((string= "0:00" time) "")
                       ((string= "N/A"  time) "")
                       ((string= ""     time) "")
                       (t (concat " (" time ")")))))
      (cond ((string= "on-line" type) "")
            ((string= ""        type) "")
            (t (propertize (concat (if (string= "AC" type) " AC" "") percentage left) 'face (ec--modeline-battery-face status)))))))

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

;; Load average.
;; TODO: Any way to get this on Android?
(defvar ec--load-average-supported
  (ignore-errors (load-average) t)
  "Whether `load-average' is supported.")

;; Putting it all together.
(defun ec--modeline-render (left right)
  "Return mode-line with LEFT and RIGHT aligned appropriately."
  (let ((padding 1.4))
    (list
     ;; HACK: This zero-width character is used to fake vertical padding.
     (when (display-graphic-p)
       (propertize "\u200b" 'display `((raise ,(/ (1- padding) -2.0)) (height ,padding))))
     left
     (propertize " " 'display `((space :align-to
                                       (- (+ right right-fringe right-margin)
                                          ,(string-width (format-mode-line right))))))
     right)))

(defun ec-set-mode-line ()
  "Customize the mode line."
  (setq-default
   mode-line-format
   '((:eval
      (ec--modeline-render
       '("%e"
         (:eval (when (fboundp 'winum-get-number)
                  (let ((num (format " %s" (winum-get-number))))
                    (if (ec-is-active-window)
                        (propertize num 'face (ec--modeline-state-face))
                      num))))
         (:eval (ec--modeline-selection))
         (:eval (when (and (ec-is-active-window) (bound-and-true-p anzu--state))
                  (list " " (anzu--update-mode-line))))
         " %[%Z%1*%1+%1@%]"
         " %I"
         (:eval
          (when (ec-is-active-window)
            (let ((input-method
                   (or current-input-method
                       (and (bound-and-true-p evil-mode)
                            (bound-and-true-p evil-input-method)))))
              (when input-method
                (list " " (nth 3 (assoc input-method input-method-alist)))))))
         (:eval (list " " (propertize
                           (ec-center-truncate (format-mode-line "%b") 20)
                           'face 'mode-line-buffer-id)))
         " " mode-name
         (:eval (when (and mode-line-process
                           (not (equal '("") mode-line-process)))
                  (list " " mode-line-process)))
         (vc-mode vc-mode)
         (:eval (when (eq major-mode 'erc-mode)
                  (list " " mode-line-buffer-identification)))
         (:eval (when (bound-and-true-p flymake-mode)
                  (list " " flymake-mode-line-counters)))
         (:eval (when (ec-is-active-window)
                  (ec--modeline-org-clock)))
         " %n")
       '((:eval (when (and (ec-is-active-window) (bound-and-true-p appt-mode-string))
                  (propertize appt-mode-string 'face 'mode-line-emphasis)))
         (:eval (when (and (ec-is-active-window) (or defining-kbd-macro executing-kbd-macro))
                  (propertize "•REC" 'face 'mode-line-emphasis)))
         (:eval (when (and (ec-is-active-window) (bound-and-true-p erc-modified-channels-alist))
                  (list " " erc-modified-channels-object)))
         (:eval (when (ec-is-active-window) (ec--modeline-battery)))
         " %l:%C"
         (:eval (when (ec-is-active-window)
                  (list " " (propertize (format-time-string "%H:%M") 'face 'mode-line-emphasis))))
         (:eval (when (and (ec-is-active-window) ec--load-average-supported)
                  (list " " (format "%.2f" (car (load-average t))))))
         " " (-3 "%p")
         " ")))))
  (ec--refresh-mode-line))

(defun ec--refresh-mode-line ()
  "Refresh the mode line in all existing buffers."
  (dolist (buffer (buffer-list))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (setq mode-line-format (default-value 'mode-line-format))))))

(add-hook 'emacs-startup-hook #'ec-set-mode-line)

;;; modeline.el ends here
