;;; org.el --- Org configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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
(setq org-emphasis-regexp-components
      '("-[:space:]('\"{…—/s“"           ; Pre
        "-[:space:].,:!?;'\")}\\[…—/’s”" ; Post
        "[:space:]"                      ; Border
        "."                              ; Body-regexp
        1))                              ; Max newlines

;; Core.
(setq org-directory ec-org-dir
      org-modules '(ol-irc ol-info org-id org-habit))

;; HTML export.
(setq org-html-checkbox-type 'html)

;; Folding.
(setq org-startup-folded t
      org-catch-invisible-edits 'smart)

;; Priorities and tags.
(setq org-highest-priority ?A
      org-lowest-priority ?Z
      org-default-priority ?M
      org-tags-exclude-from-inheritance '("prj"))

;; Style and indentation.
(setq org-startup-indented nil
      org-adapt-indentation t
      org-hide-leading-stars t
      org-hide-emphasis-markers t
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-pretty-entities t)

;; Clock.
(setq org-log-into-drawer t ; Also affects state changes.
      org-clock-out-remove-zero-time-clocks t
      org-duration-format 'h:mm)

;; Refiling.
(setq org-refile-use-cache t
      org-refile-targets '((org-agenda-files :maxlevel . 5)
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
  "Switch to the first start state if STATE is not already a start state."
  (when (and state (not (seq-contains-p ec-start-states state))) (car ec-start-states)))

(defun ec--prevent-duplicate-states (change-plist)
  "Return nil if the :from and :to properties of CHANGE-PLIST match."
  (cond ((and (eq (plist-get change-plist :type) 'todo-state-change)
              (equal (plist-get change-plist :from) (plist-get change-plist :to)))
         (setq org-block-entry-blocking "no change") ; Sets the displayed message.
         nil) ; Blocks the state change.
        (t t)))

(add-hook 'org-blocker-hook #'ec--prevent-duplicate-states)

;; Capturing.
(defun ec-capture-user (file &optional user)
  "Return the path to FILE for USER or the current user."
  (expand-file-name
   (format "%s/%s" (or user user-login-name) file)
   ec-org-dir))

(defun ec-get-template (name)
  "Return the path to capture template NAME."
  (expand-file-name (format "org/%s" name) ec-tmpl-dir))

(defun ec-capture-default ()
  "Capture to the current org file or the default."
  (unless (derived-mode-p 'org-mode)
    (set-buffer (org-capture-target-buffer org-default-notes-file))))

(setq org-default-notes-file (expand-file-name "refile.org" ec-org-dir)
      org-capture-templates
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
        ("n" "note" entry
         (function ec-capture-default)
         (file ,(ec-get-template "note")))
        ("c" "clock")
        ("cm" "meeting" entr
         (function ec-capture-default)
         (file ,(ec-get-template "meeting"))
         :clock-in t
         :clock-resume t)
        ("cc" "call" entry
         (function ec-capture-default)
         (file ,(ec-get-template "call"))
         :clock-in t
         :clock-resume t)
        ("p" "personal")
        ("pj" "journal" entry
         (file+olp+datetree ,(ec-capture-user "journal.org"))
         (file ,(ec-get-template "journal")))
        ("pd" "dream" entry
         (file+olp+datetree ,(ec-capture-user "dreams.org"))
         (file ,(ec-get-template "dream")))
        ("pm" "measure" table-line
         (file+headline ,(ec-capture-user "log.org") "Measurements")
         (file ,(ec-get-template "measure"))
         :immediate-finish t)
        ("f" "flashcards")
        ("fv" "vocab" entry
         (function ec-capture-vocab)
         (file ,(ec-get-template "vocab"))
         :prepend t
         :no-save t
         :immediate-finish t)
        ("fr" "reading")
        ("frk" "kanji" entry
         (function ec-capture-reading)
         (file ,(ec-get-template "reading-kanji"))
         :prepend t
         :no-save t
         :immediate-finish t)
        ("frw" "word" entry
         (function ec-capture-reading)
         (file ,(ec-get-template "reading-word"))
         :prepend t
         :no-save t
         :immediate-finish t)))

;; Babel.
(setq org-src-tab-acts-natively t
      org-src-window-setup 'other-window)

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
      org-agenda-files (concat ec-org-dir "/.agenda.el")
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
      '(("--" "Agenda"
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

;; Regenerate appointments when the day changes.
(with-eval-after-load 'org
  (run-at-time "00:01" (* 60 60 24) #'ec--agenda-to-appt-with-timer))

(defun ec--hook-appt-schedule ()
  "Regenerate appointments when saving the current file."
  (add-hook 'before-save-hook #'ec--appt-schedule t t))

(add-hook 'org-mode-hook #'ec--hook-appt-schedule)

;; HACK: Make sure notes don't exceed the column where they will be inserted. It
;; doesn't seem to always work when used from the agenda. Ideally this would use
;; narrowing instead like capture templates.
(defvar ec--log-current-level nil)

(defun ec--log-set-current-level (&optional _)
  "Store the current entry level."
  (setq ec--log-current-level (org-current-level)))

(advice-add 'org-add-log-note :before #'ec--log-set-current-level)

(defun ec--log-adjust-fill-column ()
  "Adjust the fill column so the note will be correct wrapped when inserted."
  (let ((offset (if org-adapt-indentation (+ ec--log-current-level 3) 2)))
    (setq fill-column (- fill-column offset))))

(add-hook 'org-log-buffer-setup-hook #'ec--log-adjust-fill-column)

;; Flashcards.
(defun ec-get-lang-fc-dirs ()
  "Get language flashcard directories."
  (mapcar #'file-name-as-directory
          (cl-remove-if-not
           #'file-directory-p
           (directory-files
            (or (getenv "ORG_FC_HOME")
                (expand-file-name "research/languages" ec-org-dir))
            t "^[^.]"))))

(defun ec--capture-language (file)
  "Capture to FILE in the selected language or current if already visiting."
  (let* ((dirs (ec-get-lang-fc-dirs))
         (dir (if (member default-directory dirs)
                  default-directory
                (completing-read "Language: " dirs nil t))))
    (set-buffer (org-capture-target-buffer (expand-file-name file dir))))
  (goto-char (point-min)))

(defun ec-capture-reading ()
  "Capture reading for the selected language or current if already visiting."
  (ec--capture-language "readings.org"))

(defun ec-capture-vocab ()
  "Capture vocab for the selected language or current if already visiting."
  (ec--capture-language "vocab.org"))

(defun ec-init-fc ()
  "Initialize a captured flashcard."
  (pcase (plist-get org-capture-plist :description)
    ("kanji" (org-fc-type-cloze-init 'enumeration))
    ("word" (org-fc-type-normal-init))
    ("vocab" (org-fc-type-double-init))))

(add-hook 'org-capture-before-finalize-hook #'ec-init-fc)

(setq org-fc-directories (ec-get-lang-fc-dirs))

(define-key global-map (kbd "C-c of") #'org-fc-dashboard)

(with-eval-after-load 'org-fc
  ;; org-fc-keymap-hint has no autoloads.
  (require 'org-fc-keymap-hint))

(with-eval-after-load 'evil
  (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-flip-mode
    (kbd "RET") 'org-fc-review-flip
    (kbd "n") 'org-fc-review-flip
    (kbd "s") 'org-fc-review-suspend-card
    (kbd "q") 'org-fc-review-quit)

  (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-rate-mode
    (kbd "a") 'org-fc-review-rate-again
    (kbd "h") 'org-fc-review-rate-hard
    (kbd "g") 'org-fc-review-rate-good
    (kbd "e") 'org-fc-review-rate-easy
    (kbd "s") 'org-fc-review-suspend-card
    (kbd "q") 'org-fc-review-quit))

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

;;; org.el ends here
