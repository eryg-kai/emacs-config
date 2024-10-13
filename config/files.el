;;; files.el --- File history, tracking, and backups. -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(setq create-lockfiles nil
      version-control t
      kept-new-versions 100
      kept-old-versions 0
      delete-old-versions t
      vc-make-backup-files t
      backup-directory-alist
      `((".*" . ,(expand-file-name "backups/" (xdg-data-home))))
      auto-save-list-file-prefix (expand-file-name "auto-save-list/" (xdg-data-home))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" (xdg-data-home)) t)))

;; Recentf.
(setq recentf-max-menu-items 1000
      recentf-max-saved-items 1000
      recentf-auto-cleanup 'never)

(defvar ec--recentf-save-timer nil "Recentf save timer.")

(defun ec--recentf-save-soon ()
  "Save recent file list soon."
  (when ec--recentf-save-timer (cancel-timer ec--recentf-save-timer))
  (setq ec--recentf-save-timer (run-with-idle-timer 30 nil #'recentf-save-list)))

(with-eval-after-load 'recentf
  (add-hook 'find-file-hook #'ec--recentf-save-soon))

(add-hook 'emacs-startup-hook #'recentf-mode)

;; Buffers that should be read only but aren't.
(defun ec--maybe-make-read-only ()
  "Make the buffer read-only if it appears to be a lockfile."
  (let ((name (buffer-name)))
    (when (or (string= name "package-lock.json") (string-suffix-p ".lock" name))
      (setq buffer-read-only t))))

(add-hook 'json-mode-hook #'ec--maybe-make-read-only)

;; Tracking.
(defvar ec--tracked-buffers nil "Tracked buffer data that is pending a write.")
;; This is needed since the buffer list is written out and then discarded.
;; Without this the last buffer is unknown, potentially allowing duplicates.
(defvar ec--last-written-buffer nil "Last buffer cons written.")

(defcustom ec-track-directory (xdg-data-home) "Directory for buffer tracking."
  :type 'string
  :group 'files)

(defun ec--save-tracked-buffers ()
  "Save tracked buffers."
  (let ((buffers ec--tracked-buffers)
        (dir (expand-file-name "track" ec-track-directory))
        (ediff 0)
        (queue nil))
    (setq ec--last-written-buffer (car ec--tracked-buffers))
    (setq ec--tracked-buffers nil)
    (make-directory dir t)
    (append-to-file
     (concat
      (mapconcat
       #'(lambda (entry)
           (concat
            (car entry)
            "\t"
            (format-time-string "%FT%T%z" (cdr entry))))
       (reverse buffers)
       "\n")
      "\n")
     nil
     (expand-file-name "buffers.tsv" dir))))

(add-hook 'kill-emacs-hook #'ec--save-tracked-buffers)

(defvar ec--track-buffer-save-timer nil "Buffer tracking save timer.")

;; TODO: Track which buffer this buffer replaced if any.
(defun ec--track-buffer ()
  "Track the current buffer."
  (let* ((buffer(current-buffer))
         (name (or (buffer-file-name buffer) (buffer-name buffer)))
         (time (current-time))
         (previous (car ec--tracked-buffers)))
    ;; Ignore some buffers.
    (unless (or (window-minibuffer-p)
                ;; There are buffers that are not actually visible.
                (not (eq (window-buffer (selected-window)) (current-buffer)))
                (eq major-mode 'ibuffer-mode))
      (when (bound-and-true-p ec-debug-p)
        (message "tracking: %s" name))
      ;; Remove short-lived buffers.
      (when (and previous (time-less-p time (time-add (cdr previous) 1)))
        (pop ec--tracked-buffers)
        (setq previous (car ec--tracked-buffers)))
      ;; Track the buffer, if it changed.
      (unless (string= name (car (or previous ec--last-written-buffer)))
        (push `(,name . ,time) ec--tracked-buffers)
        (when ec--track-buffer-save-timer (cancel-timer ec--track-buffer-save-timer))
        (setq ec--track-buffer-save-timer (run-with-idle-timer 30 nil #'ec--save-tracked-buffers))))))

(add-hook 'buffer-list-update-hook #'ec--track-buffer)

;; Save cursor position.
(add-hook 'emacs-startup-hook #'save-place-mode)

;; Ignore certain files.
(let ((ignore (format "%s\\'"(regexp-opt
                              `("git-rebase-todo"
                                "COMMIT_EDITMSG"
                                "bookmarks")))))

  (setq save-place-ignore-files-regexp ignore
        recentf-exclude (list ignore)))

;; Backups on every save.
(defun ec--force-backup ()
  "Set `buffer-backed-up' to nil."
  (setq buffer-backed-up nil))

(add-hook 'before-save-hook #'ec--force-backup)

;;; files.el ends here
