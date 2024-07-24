;;; funcs.el --- Utility functions. -*- lexical-binding: t -*-

;;; Commentary:

;;

;;; Code:

(defun ec-diff (a b)
  "Focus the frame and diff A with B."
  (select-frame-set-input-focus (selected-frame))
  (ediff-files a b))

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
  "Run FN with ARGS after ensuring `default-directory' is local."
  (let ((default-directory user-emacs-directory))
    (apply fn args)))

(defun ec-trampify (fn &rest args)
  "Override `call-process' with `process-file' for FN called with ARGS."
  (cl-letf (((symbol-function 'call-process) #'process-file))
    (apply fn args)))

(defun ec-exec (&rest args)
  "Execute ARGS asynchronously without a buffer.
ARGS are concatenated with spaces.
If no ARGS are provided, prompt for the command."
  (interactive (list (read-shell-command "$ ")))
  (apply #'ec--exec-with-sentinel nil
         (lambda (_ event) (message "%s: %s" (car args) (string-trim event)))
         args))

(defun ec--exec-with-sentinel (buffer sentinel &rest args)
  "Execute ARGS asynchronously with BUFFER.
ARGS are concatenated with spaces.
Gives process the sentinel SENTINEL.
Return the process object for the command."
  (let* ((command (string-join args " "))
         (proc (start-process-shell-command command buffer command)))
    (set-process-sentinel proc sentinel)
    proc))

(defun ec-exec-and-display (&rest args)
  "Execute ARGS synchronously and display the output.
ARGS are concatenated with spaces.
If no ARGS are provided, prompt for the command."
  (interactive (list (read-shell-command "$ ")))
  (message "%s"
   (replace-regexp-in-string
    "[\n ]+" " "
    (string-trim (shell-command-to-string
                  (string-join (seq-filter #'identity args) " "))))))

;;; funcs.el ends here
