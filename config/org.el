;;; org.el --- Org configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(typo
                                   gnuplot
                                   org-bullets
                                   ob-async
                                   ob-go
                                   ob-http
                                   ob-restclient
                                   ob-typescript
                                   org-edna
                                   org-drill))

(setq org-fold-core-style 'overlays)

;; Set the following in customize:
;; - org-directory
;; - org-default-notes-file
;; - org-agenda-files
;; - org-agenda-text-search-extra-files
;; - org-refile-targets
;; - ec-language-directory

(defcustom ec-language-directory nil "Directory for language flashcards."
  :type 'string
  :group 'org)

(add-hook 'org-mode-hook #'typo-mode)

(add-hook 'org-mode-hook #'org-bullets-mode)

;; Go into insert state when capturing/inserting/logging.
(add-hook 'org-insert-heading-hook #'evil-insert-state)
(add-hook 'org-log-buffer-setup-hook #'evil-insert-state)
(add-hook 'org-capture-mode-hook #'evil-insert-state)

;; Keybindings.
(define-key global-map (kbd "C-c ol") #'org-store-link)
(define-key global-map (kbd "C-c oc") #'org-capture)

;; Must set emphasis components before org loads (or use org-set-emph-re).
;; (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
(setq org-emphasis-regexp-components
      '("-[:space:]('\"{…—/“"            ; Pre
        "-[:space:].,:!?;'\")}\\[…—/’s”" ; Post
        "[:space:]"                      ; Border
        "."                              ; Body-regexp
        1))                              ; Max newlines

;; Core.
(setq org-modules '(ol-irc ol-info org-id org-habit ol-eww)
      org-element-cache-persistent nil)

;; HTML export.
(setq org-html-checkbox-type 'html)

;; Folding.
(setq org-startup-folded t
      org-catch-invisible-edits 'smart)

;; Priorities, tags, and cookies.
(setq org-highest-priority ?A
      org-lowest-priority ?Z
      org-default-priority ?M
      org-hierarchical-todo-statistics nil
      org-tags-exclude-from-inheritance '("prj"))

;; Style and indentation.
(setq org-startup-indented nil
      org-adapt-indentation t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-pretty-entities t
      ;; TODO: Too many false positives on sub/superscripts.
      org-pretty-entities-include-sub-superscripts nil)

;; Clock.
(setq org-log-into-drawer t ; Also affects state changes.
      org-clock-out-remove-zero-time-clocks t
      org-duration-format 'h:mm)

;; Refiling.
(setq org-refile-use-cache t
      org-refile-targets '((org-agenda-files :maxlevel . 1)
                           (nil :maxlevel . 5))
      org-outline-path-complete-in-steps nil
      org-refile-use-outline-path t
      org-blank-before-new-entry nil)

;; Keywords and state changes.
(setq org-treat-insert-todo-heading-as-state-change t
      org-log-done nil ; Redundant with the state change.
      ;; !  = add timestamp when entering the state
      ;; @  = add both a note and timestamp
      ;; /! = add timestamp when leaving the state if the target state does not
      ;;      already record it
      org-todo-keywords
      '((sequence "[ ](T!)" "[>](N!)" "[-](S!)" "[~](A!)" "[?](W@/!)" "|" "[!](C@/!)" "[X](D!)")
        (sequence
         "PROJECT(p!)" "TASK(t!)" "MEET(m!)" "CALL(l!)"
         "HABIT(b!)" "NEXT(n!)" "START(s!)" "PART(a!)"
         "WAIT(w@/!)" "|" "SKIP(k!)" "CANCEL(c@/!)" "DONE(d!)"))
      org-todo-state-tags-triggers '(("PROJECT" ("prj" . t)))
      org-clock-out-when-done '("WAIT" "SKIP" "CANCEL" "DONE" "[?]" "[!]" "[X]")
      org-clock-in-switch-to-state #'ec--clock-in-switch-to-state
      org-use-fast-todo-selection 'expert)

(defvar ec-start-states '("START" "MEET" "CALL" "[-]" "HABIT") "List of start states.")

(defun ec--clock-in-switch-to-state (state)
  "Switch to the appropriate start state if STATE is not already a start state."
  (when (and state (not (seq-contains-p ec-start-states state)))
    (if (string-prefix-p "[" state) "[-]" "START")))

(defun ec--prevent-duplicate-states (change-plist)
  "Return nil if the :from and :to properties of CHANGE-PLIST match."
  (cond ((and (eq (plist-get change-plist :type) 'todo-state-change)
              (equal (plist-get change-plist :from) (plist-get change-plist :to)))
         (setq org-block-entry-blocking "no change") ; Sets the displayed message.
         nil) ; Blocks the state change.
        (t t)))

(add-hook 'org-blocker-hook #'ec--prevent-duplicate-states)

;; Capturing.
(define-key global-map (kbd "C-c C-z") #'org-add-note)

(defun ec-capture-user (file &optional user)
  "Return the path to FILE for USER or the current user."
  (expand-file-name
   (format "%s/%s" (or user user-login-name) file)
   org-directory))

(defun ec-get-template (name)
  "Return the path to capture template NAME."
  (expand-file-name (format "templates/org/%s" name) ec-dir))

(defun ec-capture-default ()
  "Capture to the current org file or the default."
  (unless (derived-mode-p 'org-mode)
    (set-buffer (org-capture-target-buffer org-default-notes-file)))
  (goto-char (point-max)))

(defun ec-capture-log ()
  "Capture a note into the logbook."
  (let ((marker (if (eq major-mode 'org-agenda-mode)
                    (org-get-at-bol 'org-marker)
                  org-clock-marker)))
    (cond ((derived-mode-p 'org-mode)
           (goto-char (org-entry-beginning-position)))
          ((and (markerp marker)
                (marker-buffer marker))
           (set-buffer (marker-buffer marker))
           (goto-char marker))
          (t (user-error "No suitable capture target"))))
  (let* ((template (split-string (string-trim-right (org-capture-get :template)) "\n"))
         (offset (if org-adapt-indentation (+ (org-current-level) 1) 2))
         (indentation (make-string offset ? ))
         (drawer (org-log-into-drawer)))
    (goto-char (org-log-beginning nil))
    (when (and drawer
               (not (save-excursion (forward-line -1) (looking-at-p org-logbook-drawer-re))))
      (setq template (append `(,(concat ":" drawer ":"))
                             template
                             '(":END:"))))
    (org-capture-put :template (mapconcat
                                (lambda (line) (concat indentation line))
                                template "\n"))))

;; Ripped from `org-store-log-note'.
(defun ec--log-heading ()
  "Format and return log heading based on purpose."
  (org-replace-escapes
   (cdr (assq org-log-note-purpose org-log-note-headings))
   (list (cons "%u" (user-login-name))
         (cons "%U" user-full-name)
         (cons "%t" (format-time-string
                     (org-time-stamp-format 'long 'inactive)
                     org-log-note-effective-time))
         (cons "%T" (format-time-string
                     (org-time-stamp-format 'long nil)
                     org-log-note-effective-time))
         (cons "%d" (format-time-string
                     (org-time-stamp-format nil 'inactive)
                     org-log-note-effective-time))
         (cons "%D" (format-time-string
                     (org-time-stamp-format nil nil)
                     org-log-note-effective-time))
         (cons "%s" (cond
                     ((not org-log-note-state) "")
                     ((string-match-p org-ts-regexp
                                      org-log-note-state)
                      (format "\"[%s]\""
                              (substring org-log-note-state 1 -1)))
                     (t (format "\"%s\"" org-log-note-state))))
         (cons "%S"
               (cond
                ((not org-log-note-previous-state) "")
                ((string-match-p org-ts-regexp
                                 org-log-note-previous-state)
                 (format "\"[%s]\""
                         (substring
                          org-log-note-previous-state 1 -1)))
                (t (format "\"%s\""
                           org-log-note-previous-state)))))))

(defun ec-add-log (fn &rest args)
  "Like `org-add-log-note' except use capture templates.

Call FN with ARGS for any log entry that does not take a note."
  (interactive)
  (if (or (eq org-log-note-purpose 'note)
          (and (eq org-log-note-purpose 'state)
               (eq org-log-note-how 'note)))
      (progn
        (remove-hook 'post-command-hook 'org-add-log-note)
        (setq org-log-setup nil)
        (org-capture nil "cn"))
    (apply fn args)))

;; Replace default note capture with a version that uses capture templates.
(advice-add 'org-add-log-note :around #'ec-add-log)

;; `org-directory' will be void until org loads so delay setting capture
;; templates as they build paths off the org directory.
(with-eval-after-load 'org-capture
  (setq org-capture-templates
        `(("t" "task")
          ("tt" "task" entry
           (function ec-capture-default)
           (file ,(ec-get-template "task")))
          ("ts" "start" entry
           (function ec-capture-default)
           (file ,(ec-get-template "task"))
           :clock-in t
           :clock-resume t)
          ("tr" "respond" entry
           (function ec-capture-default)
           (file ,(ec-get-template "respond"))
           :immediate-finish t)
          ("th" "habit" entry
           (file ,(ec-capture-user "habits.org"))
           (file ,(ec-get-template "habit")))
          ("n" "note")
          ("nn" "note" entry
           (function ec-capture-default)
           (file ,(ec-get-template "note")))
          ("nl" "link" entry
           (function ec-capture-default)
           (file ,(ec-get-template "link")))
          ("nc" "clock" plain ; Same as C-c C-z
           (function ec-capture-log)
           (file ,(ec-get-template "log")))
          ("c" "clock")
          ("cm" "meeting" entry
           (function ec-capture-default)
           (file ,(ec-get-template "meeting"))
           :clock-in t
           :clock-resume t)
          ("cc" "call" entry
           (function ec-capture-default)
           (file ,(ec-get-template "call"))
           :clock-in t
           :clock-resume t)
          ("cn" "note" plain ; Same as C-c C-z
           (function ec-capture-log)
           (file ,(ec-get-template "log")))
          ("p" "personal")
          ("pj" "journal" entry
           (file+olp+datetree ,(ec-capture-user "journal.org"))
           (file ,(ec-get-template "journal")))
          ("pd" "dream" entry
           (file+olp+datetree ,(ec-capture-user "dreams.org"))
           (file ,(ec-get-template "dream")))
          ("pm" "measure" table-line
           (file+function ,(ec-capture-user "log.org") ec-capture-measurement)
           "|%U|%^{Value}|" ; Temporary stand-in; will be replaced.
           :immediate-finish t)
          ("f" "flashcards")
          ("fv" "vocab" entry
           (function ec-capture-language)
           (file ,(ec-get-template "vocab"))
           :prepend t
           :no-save t
           :immediate-finish t)
          ("fk" "kanji" entry
           (function ec-capture-language)
           (file ,(ec-get-template "kanji"))
           :prepend t
           :no-save t
           :immediate-finish t)
          ("fr" "readings" entry
           (function ec-capture-language)
           (file ,(ec-get-template "reading"))
           :prepend t
           :no-save t
           :immediate-finish t)
          ("fR" "radicals" entry
           (function ec-capture-language)
           (file ,(ec-get-template "radical"))
           :prepend t
           :no-save t
           :immediate-finish t))))

(defun ec-capture-measurement ()
  "Capture a measurement to the table under the selected heading."
  (let ((org-refile-targets '((nil :maxlevel . 1))))
    (goto-char (nth 3 (org-refile-get-location "Heading")))
    ;; The ideal would be to use this as the template function but that runs
    ;; before visiting so the table information at point is not available.
    (org-capture-put :template (ec-measurement-template))))

(defun ec-measurement-template ()
  "Return a capture template based on first table under the current heading."
  (let* ((end (save-excursion (outline-next-heading) (point)))
         (strip "[ \t\n\r|]+")
         (content (save-excursion
                    (unless (re-search-forward org-table-dataline-regexp end t)
                      ;; TODO: Ask for the inputs instead.
                      (user-error "No table found"))
                    (string-trim (thing-at-point 'line t) strip strip)))
         (parts (mapcar 'string-trim (split-string content "|"))))
    (concat "|" (mapconcat
                 (lambda (item)
                   (if (string= item "Date") "%U" (format "%%^{%s}" item)))
                 parts "|")
            "|")))

(defun ec-init-capture ()
  "Initialize a capture."
  (pcase (plist-get org-capture-plist :description)
    ("kanji" (org-align-tags) (org-id-get-create))
    ("readings" (org-align-tags) (org-id-get-create))
    ("radicals" (org-align-tags) (org-id-get-create))
    ("vocab" (let ((end (plist-get org-capture-plist :end-marker)))
               (org-align-tags)
               (org-id-get-create)
               (org-next-visible-heading 1) ; Skip to sentences.
               (evil-indent (point) end)
               (fill-region (point) end)))))

(add-hook 'org-capture-before-finalize-hook #'ec-init-capture)

;; Babel.
(setq org-src-tab-acts-natively t
      org-src-window-setup 'current-window)

(defvar ec-babel-map
  '((bash . shell)
    (elisp . emacs-lisp)
    (javascript . js))
  "Alist of language aliases.")

(defun ec--load-language (fn &optional arg info params)
  "Load language if not already done then call FN with ARG, INFO, and PARAMS."
  (let* ((info (or info (org-babel-get-src-block-info)))
         (actual-language (intern (car info)))
         (language (or (alist-get actual-language ec-babel-map) actual-language)))
    (unless (alist-get language org-babel-load-languages)
      (add-to-list 'org-babel-load-languages (cons language t))
      (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
    (setcar info (symbol-name language))
    (funcall fn arg info params)))

(advice-add 'org-babel-execute-src-block :around #'ec--load-language)

;; Bolded headers mess with the :pretty option.
(setq ob-http:curl-custom-arguments '("--no-styled-output"))

;; TEMP: Workaround for ob-http not working over Tramp.
(advice-add 'org-babel-execute:http :around #'ec-trampify)

;; Task dependencies.
(setq org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t)

(with-eval-after-load 'org
  (org-edna-mode))

;; Agenda.
(define-key global-map (kbd "C-c oa") #'org-agenda)

(setq org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-dim-blocked-tasks nil
      org-agenda-sticky t
      org-agenda-restore-windows-after-quit t
      org-agenda-span 2
      org-agenda-use-time-grid t
      org-agenda-time-grid '((daily today require-timed) () "~~~~~~" "---")
      org-agenda-current-time-string "← now"
      org-agenda-time-leading-zero t
      org-agenda-start-on-weekday nil
      org-habit-graph-column 74
      org-stuck-projects '("+prj/-CANCEL-DONE" ("NEXT" "START") nil nil)
      org-agenda-block-separator "---"
      org-agenda-compact-blocks t
      org-agenda-remove-tags t
      org-agenda-log-mode-items '(closed clock state)
      org-agenda-custom-commands
      '(("--" "Extended agenda"
         ((agenda "" nil)
          (todo "START|\\[-\\]" ((org-agenda-overriding-header "Started")))
          (todo "NEXT|\\[>\\]" ((org-agenda-overriding-header "Next")))
          (todo "WAIT|\\[\\?\\]" ((org-agenda-overriding-header "Postponed")))
          (stuck "" ((org-agenda-overriding-header "Stuck")))
          (tags "goal" ((org-agenda-overriding-header "Goals")))
          (tags "refile" ((org-agenda-overriding-header "Refile"))))
         nil)
        ("-d" "Discovery"
         ((tags "+PRIORITY<\"M\"" ((org-agenda-overriding-header "Priorities")))
          (tags "review" ((org-agenda-overriding-header "Review")))
          (tags "research" ((org-agenda-overriding-header "Research"))))))
      org-agenda-sorting-strategy
      '((agenda time-up habit-down priority-down effort-down category-keep)
        (todo priority-down effort-down category-keep)
        (tags priority-down effort-down category-keep)
        (search category-keep)))

;; Appointments.
(setq appt-message-warning-time 30
      appt-display-interval 15)

(defun ec--agenda-to-appt ()
  "Generate appointments from the agenda."
  (org-agenda-to-appt t)
  (appt-check))

(defun ec--agenda-to-appt-with-timer ()
  "Generate appointments after a timer."
  (timer-idle-debounce #'ec--agenda-to-appt))

(defun ec--appt-schedule ()
  "Generate appointments after a timer if the current file is an agenda file."
  (when (member (buffer-file-name) (org-agenda-files))
    (ec--agenda-to-appt-with-timer)))

(defun ec--hook-appt-schedule ()
  "Regenerate appointments when saving the current file."
  (add-hook 'before-save-hook #'ec--appt-schedule nil t))

;; Flashcards.
(setq org-drill-maximum-items-per-session 150
      org-drill-maximum-duration 30
      ;; org-drill-use-visible-cloze-face-p t ;TODO: this removes some heading styles
      org-drill-save-buffers-after-drill-sessions-p nil)

(defun ec-get-language-dirs ()
  "Get language flashcard directories."
  (when (file-exists-p ec-language-directory)
    (mapcar #'file-name-as-directory
            (cl-remove-if-not
             #'file-directory-p
             (directory-files ec-language-directory t "^[^.]")))))

(defun ec-capture-language ()
  "Capture to the selected language or current if already visiting."
  (let* ((file (concat (plist-get org-capture-plist :description) ".org"))
         (dirs (ec-get-language-dirs))
         (dir (if (member default-directory dirs)
                  default-directory
                (expand-file-name
                 (completing-read "Target: "
                                  (mapcar
                                   (lambda (dir)
                                     (string-remove-prefix ec-language-directory dir))
                                   dirs)
                                  nil t)
                 ec-language-directory))))
    (set-buffer (org-capture-target-buffer (expand-file-name file dir))))
  (goto-char (point-min)))

;; REVIEW: See if I can send upstream?
(defvar ec-mks-expert t
  "Non-nil means use an expert selection scheme with `\\[org-mks]'.

No special window with the keyword will be shown and choices will
only be listed in the prompt.")

(defun ec--mks (table title &optional prompt specials)
  "Select a member of an alist with multiple keys.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"...

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let* ((inhibit-quit t)
           (buffer (unless ec-mks-expert (org-switch-to-buffer-other-window "*Org Select*")))
           (prompt (or prompt "Select: "))
           (body nil)
           (special-body nil)
           case-fold-search
           current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq body nil)
              (when buffer
                (erase-buffer)
                (insert title "\n\n"))
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (setq body (concat
                                     (if body (concat body " ") "{")
                                     "[" k "] ..." desc "..."))
                         (when buffer (insert prefix "[" k "]" "..." "  " desc "..." "\n"))))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (setq body (concat
                                     (if body (concat body " ") "{")
                                     "[" k "] " desc))
                         (when buffer (insert prefix "[" k "]" "     " desc "\n"))
                         (push k allowed-keys)))
                      (_ nil))))
                (when body (setq body (concat body "}")))
                ;; Insert special entries, if any.
                (setq special-body nil)
                (when (and (not ec-mks-expert) specials)
                  (when buffer (insert "----------------------------------------------------\
---------------------------\n"))
                  (pcase-dolist (`(,key ,description) specials)
                    (setq special-body
                          (concat
                           (if special-body (concat special-body " ") "{")
                           "[" key "] " description))
                    (when buffer (insert (format "[%s]     %s\n" key description)))
                    (push key allowed-keys))
                  (when special-body (setq special-body (concat special-body "}"))))
                ;; Display UI and let user select an entry or
                ;; a sub-level prefix.
                (when buffer
                  (goto-char (point-min))
                  (unless (pos-visible-in-window-p (point-max))
                    (org-fit-window-to-buffer)))
                (let ((pressed
                       (org--mks-read-key
                        allowed-keys
                        (if buffer
                            prompt
                          (concat "[a-z...]:Set\n" body special-body))
                        (when buffer
                          (not (pos-visible-in-window-p (1- (point-max))))))))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))

(advice-add 'org-mks :override #'ec--mks)

(defun ec--org-log-beginning (&optional create)
  "Copy of `org-log-beginning' except without an extra newline."
  (org-with-wide-buffer
   (let ((drawer (org-log-into-drawer)))
     (cond
      (drawer
       (org-end-of-meta-data)
       (let ((regexp (concat "^[ \t]*:" (regexp-quote drawer) ":[ \t]*$"))
	     (end (if (org-at-heading-p) (point)
		    (save-excursion (outline-next-heading) (point))))
	     (case-fold-search t))
	 (catch 'exit
	   ;; Try to find existing drawer.
	   (while (re-search-forward regexp end t)
	     (let ((element (org-element-at-point)))
	       (when (eq (org-element-type element) 'drawer)
		 (let ((cend  (org-element-property :contents-end element)))
		   (when (and (not org-log-states-order-reversed) cend)
		     (goto-char cend)))
		 (throw 'exit nil))))
	   ;; No drawer found.  Create one, if permitted.
	   (when create
             ;; Unless current heading is the last heading in buffer
             ;; and does not have a newline, `org-end-of-meta-data'
             ;; should move us somewhere below the heading.
             ;; Avoid situation when we insert drawer right before
             ;; first "*".  Otherwise, if the previous heading is
             ;; folded, we are inserting after visible newline at
             ;; the end of the fold, thus breaking the fold
             ;; continuity.
             (unless (eobp)
               (when (org-at-heading-p) (backward-char)))
             (org-fold-core-ignore-modifications
	       (unless (bolp) (insert-and-inherit "\n"))
	       (let ((beg (point)))
	         (insert-and-inherit ":" drawer ":\n:END:")
	         (org-indent-region beg (point))
	         (org-fold-region (line-end-position -1) (point) t (if (eq org-fold-core-style 'text-properties) 'drawer 'outline)))))
	   (end-of-line 0))))
      (t
       (org-end-of-meta-data org-log-state-notes-insert-after-drawers)
       (let ((endpos (point)))
         (skip-chars-forward " \t\n")
         (beginning-of-line)
         (unless org-log-states-order-reversed
	   (org-skip-over-state-notes)
	   (skip-chars-backward " \t\n")
	   (beginning-of-line 2))
         ;; When current headline is at the end of buffer and does not
         ;; end with trailing newline the above can move to the
         ;; beginning of the headline.
         (when (< (point) endpos) (goto-char endpos))))))
   (if (bolp) (point) (line-beginning-position 2))))

(advice-add 'org-log-beginning :override #'ec--org-log-beginning)

;;; org.el ends here
