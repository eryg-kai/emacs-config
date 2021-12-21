;;; files.el --- File history, tracking, and backups. -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:
(setq create-lockfiles nil
      version-control t
      kept-new-versions most-positive-fixnum
      kept-old-versions 0
      delete-old-versions t
      make-backup-files nil
      vc-make-backup-files t
      backup-by-copying t
      backup-directory-alist
      `((".*" . ,(expand-file-name "backups/" user-emacs-directory)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))

;; Recentf.
(setq recentf-max-menu-items 1000
      recentf-max-saved-items 1000
      recentf-auto-cleanup 'never
      recentf-exclude
      `("/git-rebase-todo\\'"
        "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'"
        "COMMIT_EDITMSG\\'" ,(expand-file-name user-emacs-directory)))

(defun ec--recentf-save-list-soon ()
  "Save recent file list soon."
  (timer-idle-debounce #'recentf-save-list))

(with-eval-after-load 'recentf
  (add-hook 'find-file-hook #'ec--recentf-save-list-soon))

(add-hook 'emacs-startup-hook #'recentf-mode)

;; Backups.
(defun ec--backup-buffer ()
  "Make a backup of the file visited by the current buffer.

The backup is made asynchronously via mkdir and cp and is placed on the same
machine as the file.  The destination for a file's backup can be configured
through `backup-directory-alist'.  If the destination includes the local user's
home directory it will use the remote user's home directory instead."
  (when-let (file (or (file-remote-p (buffer-file-name) 'localname)
                      (buffer-file-name)))
    (cl-every (lambda (elt)
                (when (string-match (car elt) file)
                  (let* ((ext (file-name-extension file))
                         (dest (string-replace
                                ;; ~ will be interpreted literally in quotes so
                                ;; replace it with the HOME environment variable.
                                "~" "$HOME"
                                ;; The remote home might not be the same as the
                                ;; local home so abbreviate in order to convert
                                ;; the home into ~.
                                (abbreviate-file-name
                                 (expand-file-name
                                  (string-remove-prefix
                                   "/"
                                   (concat (file-name-directory file)
                                           (file-name-base file)
                                           "-"
                                           (format-time-string "%FT%T%z")
                                           (when ext ".")
                                           ext))
                                  (cdr elt))))))
                    (set-process-sentinel
                     (start-file-process "backup" (when ec-debug-p "*backup*") "bash" "-c"
                                         (format
                                          "mkdir -p \"%s\" && cp \"%s\" \"%s\" && chmod -w \"%s\""
                                          (file-name-directory dest)
                                          file dest
                                          dest))
                     (lambda (_ event)
                       (message "Backup %s %s" file (string-trim event)))))))
              backup-directory-alist)))

(add-hook 'after-save-hook #'ec--backup-buffer)

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

(defun ec--save-tracked-buffers ()
  "Save tracked buffers."
  (let ((buffers ec--tracked-buffers)
        (dir (expand-file-name "track" user-emacs-directory))
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

;; TODO: Track which buffer this buffer replaced if any.
(defun ec--track-buffer ()
  "Track the current buffer."
  (let* ((buffer (current-buffer))
         (name (or (buffer-file-name buffer) (buffer-name buffer)))
         (time (current-time))
         (previous (car ec--tracked-buffers)))
    ;; Remove short-lived buffers.
    (when (and previous (time-less-p time (time-add (cdr previous) 1)))
      (pop ec--tracked-buffers)
      (setq previous (car ec--tracked-buffers)))
    (unless (string= name (car (or previous ec--last-written-buffer)))
      (push `(,name . ,time) ec--tracked-buffers)
      (timer-idle-debounce #'ec--save-tracked-buffers))))

(defun ec--track-buffer-debounce ()
  "Track the current buffer."
  ;; The last buffer to go through `buffer-list-update-hook' isn't necessarily
  ;; the actual current buffer (or even a visible buffer) so wait a bit and
  ;; check again to be sure.
  (timer-debounce #'ec--track-buffer 0.1))

(add-hook 'buffer-list-update-hook #'ec--track-buffer-debounce)
(add-hook 'exwm-update-title-hook #'ec--track-buffer-debounce)

;;; files.el ends here
