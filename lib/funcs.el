;;; funcs.el --- Utility functions. -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(defun ec-diff (a b)
  "Focus the frame and diff A with B."
  (select-frame-set-input-focus (selected-frame))
  (ediff-files a b))

;; You can use the T flag to compute time durations but I can't get that to work
;; while still being able to do a regex match on the date, so this is the
;; solution. I keep a running total of overtime or time I need to make up using
;; the following:
;;
;; #+TBLFM: $3='(0x0049/to-string (- (+ (cond ((= @# 2) $start) (t
;; (0x0049/to-seconds "@-1"))) (cond ((string-match-p "Sat\\|Sun" "$1") 0) (t
;; $per))) (0x0049/to-seconds "$2")));L
;;
;; Where $per = seconds to work per day and $start = current overtime or time to
;; make up before the table started (also in seconds). The table has the
;; following columns: "Date"", "Time" (HH:MM format), and "Remaining".
(defun ec-to-seconds (s)
  "Convert string S in %H:%M:%S, %H:%M, and %S formats to seconds."
  (cond ((stringp s)
         (let ((sign (if (string-match "^-" s) -1 1)))
           (cond ((string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)" s)
                  (let ((hour (string-to-number (match-string 1 s)))
                        (min (string-to-number (match-string 2 s)))
                        (sec (string-to-number (match-string 3 s))))
                    (* (+ (* hour 3600) (* min 60) sec) sign)))
                 ((string-match "\\([0-9]+\\):\\([0-9]+\\)" s)
                  (let ((hour (string-to-number (match-string 1 s)))
                        (min (string-to-number (match-string 2 s))))
                    (* (+ (* hour 3600) (* min 60)) sign)))
                 (t (string-to-number s)))))
        (t s)))

;; format-seconds doesn't seem to handle negative seconds correctly.
(defun ec-format-seconds (seconds)
  "Convert number SECONDS to hours:minutes."
  (cond ((>= seconds 0) (format-seconds "%h:%.2m" seconds))
        (t (concat "-" (format-seconds "%h:%.2m" (* seconds -1))))))

(defun ec-center-truncate (item len)
  "Replace center of ITEM with `truncate-string-ellipsis' to make it length LEN.

When the length is odd the right side will be one longer than the left."
  (let ((item (if (stringp item) item (format "%s" item))))
    (if (> (length item) len)
        (let* ((len (- len 1))
               (mid (/ len 2)))
          (concat (substring item 0 mid)
                  (apply #'propertize truncate-string-ellipsis (text-properties-at (- mid 1) item))
                  (substring item (- mid len) nil)))
      item)))

(defun ec-run-and-bury (fn &rest args)
  "Run the FN with ARGS then bury the current buffer."
  (let ((buf (buffer-name)))
    (apply fn args)
    (bury-buffer buf)))

;; Set PAGER to the following script:
;;
;; temp=$(mktemp --suffix .pager)
;; cat - >> "$temp"
;; height=$(tput lines)
;; lines=$(wc -l < "$temp")
;; if [ "$height" -lt "$lines" ] ; then
;;   emacsclient --no-wait "$temp"
;; else
;;   cat "$temp"
;; fi
;; rm -f -- "$temp"
;;
;; --no-wait is optional but I prefer to free up the terminal for more work.
;;
;; That way long output will use Emacs as a pager. Then `ec-colorize' can be
;;used to colorize ANSI codes when necessary.
(defun ec-colorize (beg end)
  "Colorize ANSI codes from BEG to END or the entire buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (ansi-color-apply-on-region beg end))

(defun ec-find-node-module-binary (name &optional directory)
  "Travel upward from DIRECTORY looking for NAME in node_modules."
  (let* ((root (or directory default-directory))
         (binary (and root (expand-file-name (concat "node_modules/.bin/" name) root))))
    (cond ((and binary (file-executable-p binary)) binary)
          ((string-equal root "/") nil)
          (t (ec-find-node-module-binary name (directory-file-name (file-name-directory root)))))))

(defun ec-column-number-at-pos (pos)
  "Column number at POS.  Analog to `line-number-at-pos'."
  (save-excursion (goto-char pos) (current-column)))

(defun ec-localize (fn &rest args)
  "Run FN with ARGS after ensure `default-directory' is local."
  (let ((default-directory "~"))
    (apply fn args)))

(defun ec-trampify (fn &rest args)
  "Override `call-process' with `process-file' for FN called with ARGS."
  (cl-letf (((symbol-function 'call-process) #'process-file))
    (apply fn args)))

;;; funcs.el ends here
