;;; ibuffer.el --- Configure ibuffer. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(define-key global-map (kbd "C-c bi") #'ibuffer)

(defvar ec-ibuffer-filter-group-order '("Default" "Shell"))

(defvar ec-ibuffer-startup-filter-group "ec")

(setq ibuffer-eliding-string "…"
      ibuffer-formats
      '((mark modified read-only " " (name-c 20 -1 :left)
              " " (size-h 9 -1 :right)
              " " (mode 16 16 :left :elide) " " filename-and-process)
        (mark " " (name-c 20 -1) " " filename))
      ibuffer-saved-filter-groups
      `((,ec-ibuffer-startup-filter-group
         ("EXWM" (mode . exwm-mode))
         ("EWW" (mode . eww-mode))
         ("Images" (mode . image-mode))
         ("Shell" (or (mode . eshell-mode) (mode . term-mode) (mode . shell-mode)))
         ("Scratch" (name . "\\`\\*.*scratch\\*\\'"))
         ("Compilation" (mode . compilation-mode))
         ("Dired" (mode . dired-mode))
         ("Chat" (or (mode . erc-mode) (mode . rcirc-mode)))
         ("Magit" (name . "magit"))
         ("Special" (name . "\\`\\*.+\\*\\'")))))

(defun ec--ibuffer-sort-filter-groups (groups)
  "Sort GROUPS using `ec-ibuffer-filter-group-order' and then alphabetically."
  ;; This sorts in reverse because ibuffer prints in reverse.
  (sort groups
        (lambda (a b)
          (let ((apos (cl-position (car a) ec-ibuffer-filter-group-order :test 'equal))
                (bpos (cl-position (car b) ec-ibuffer-filter-group-order :test 'equal)))
            (cond ((and apos bpos) (> apos bpos))
                  (apos nil)
                  (bpos t)
                  (t (string> (car a) (car b))))))))

(advice-add 'ibuffer-generate-filter-groups :filter-return #'ec--ibuffer-sort-filter-groups)

(defun ec--ibuffer-apply-startup-filter-group ()
  "Apply the startup filter group from `ec-ibuffer-startup-filter-group'."
  (when ec-ibuffer-startup-filter-group
    (ibuffer-switch-to-saved-filter-groups ec-ibuffer-startup-filter-group)))

(add-hook 'ibuffer-mode-hook #'ec--ibuffer-apply-startup-filter-group)

(defun ec--ibuffer-jump-to-last-buffer ()
  "Jump to the buffer that was just active."
  (ibuffer-jump-to-buffer (buffer-name (cadr (buffer-list)))))

(add-hook 'ibuffer-hook #'ec--ibuffer-jump-to-last-buffer)

(with-eval-after-load 'ibuffer
  (define-ibuffer-column size-h
    (:name "Size"
           :inline t
           :summarizer
           ;; OPTIMIZE: Would be better to get the original values and sum those.
           (lambda (strings)
             (file-size-human-readable
              (seq-reduce
               (lambda (total value)
                 (let* ((suffixes '("" "k" "M" "G" "T" "P" "E" "Z" "Y"))
                        (suffix (car (member (substring value -1) suffixes)))
                        (power 1000.0)
                        (bytes (string-to-number value)))
                   (while (and suffix (car suffixes) (not (string= (car suffixes) suffix)))
                     (setq bytes (* bytes power)
                           suffixes (cdr suffixes)))
                   (+ total bytes)))
               strings 0)
              "si")))
    (file-size-human-readable (buffer-size) "si"))

  ;; TODO: Would be nice to have an option to elide the center.
  ;;       For now use a custom name column.
  (define-ibuffer-column name-c
    (:inline t
             :summarizer (lambda (strings)
                           (let ((bufs (length strings)))
                             (format "%s buffer%s" bufs (if (= 1 bufs) "" "s")))))
    (let ((string (propertize (buffer-name) 'font-lock-face
                              (ibuffer-buffer-name-face buffer mark))))
      (ec-center-truncate
       (replace-regexp-in-string "\n" (propertize "^J" 'font-lock-face 'escape-glyph) string)
       20))))

;; Bury ibuffer so it doesn't come up when switching to other buffer or previous
;; buffer, etc.
(advice-add 'ibuffer-visit-buffer :around #'ec-run-and-bury)

;;; ibuffer.el ends here
