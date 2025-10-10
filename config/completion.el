;;; completion.el --- Provide completion. -*- lexical-binding: t -*-

;;; Commentary:

;; Completions for code, files, keys, etc and anything to do with prompts.

;;; Code:

(nconc package-selected-packages '(cape
                                   tempel
                                   consult))

;; Completion.
(setq history-delete-duplicates t
      tab-always-indent 'complete
      completion-auto-select t
      completion-auto-help t
      completion-auto-select t
      completion-ignore-case t
      completion-styles '(flex)
      completion-category-overrides '((file (styles . (substring)))
                                      ;; `consult' uses multi-category.
                                      (multi-category (styles . (substring))))

      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; Apropos.
(setq apropos-do-all t)

;; Which-key.
(setq which-key-idle-delay 1)

(defun ec--split-name (name)
  "Split NAME into its prefix and suffix."
  (and name
       (save-match-data
         (when (string-match
                "^\\(\\([^/]+\\)\\(/\\)+\\|\\([^-]+\\)\\(-\\)+\\)\\(.+\\)$"
                name)
           (list (or (match-string 2 name) (match-string 4 name))
                 (or (match-string 3 name) (match-string 5 name))
                 (match-string 6 name))))))

(defun ec--replace-then-run (fn key-binding)
  "Call FN with stripped KEY-BINDING."
  (let* ((name (cdr key-binding))
         (split (ec--split-name name)))
    (if (and split
             (or (equal (nth 0 split) "ec")
                 (featurep (intern (nth 0 split)))))
        (apply fn `(,(car key-binding) . ,(nth 2 split)) nil)
      (apply fn key-binding nil))))

(advice-add 'which-key--maybe-replace :around 'ec--replace-then-run)

(when (fboundp 'which-key-mode)
  (add-hook 'emacs-startup-hook #'which-key-mode))

(with-eval-after-load 'which-key
  ;; Instead of -0, display 0..9
  (push '((nil . "select-window-0-or-10") . ("0..9" . "window 0..9"))
        which-key-replacement-alist)
  ;; Don't show the 1-9 bindings.
  (push '((nil . "select-window-[1-9]") . t) which-key-replacement-alist)

  (which-key-add-key-based-replacements
    "C-c o" "org"
    "C-c oh" "heading"
    "C-c f" "files"
    "C-c b" "buffer"
    "C-c '" "mode"
    "C-x p" "project"
    "C-c s" "scratch"
    "C-c w" "window"
    "C-c g" "git"

    "C-c l" "lsp"
    "C-c l =" "format"
    "C-c l F" "workspace"
    "C-c l G" "peek"
    "C-c l T" "toggle"
    "C-c l a" "action"
    "C-c l g" "goto"
    "C-c l h" "help"
    "C-c l r" "rename"
    "C-c l s" "server"))

;; Man pages.
(setq Man-notify-method 'aggressive)

;; Ispell.
(setq ispell-personal-dictionary
      (expand-file-name "aspell/dictionary" (xdg-data-home))
      ;; Make curly quotes work with spellcheck.
      ispell-local-dictionary-alist
      '((nil "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-B") nil utf-8))
      ispell-extra-args
      `("--repl" ,(expand-file-name "aspell/replacements" (xdg-data-home))))

(add-hook 'text-mode-hook #'ispell-minor-mode)
(add-hook 'prog-mode-hook #'ispell-minor-mode)

;; `ding' aborts macros so disable it to make them always complete (ispell for
;; example will trigger this if it thinks a word is misspelled).
(advice-add #'ding :override #'ignore)

;; Savehist.
(setq savehist-file (expand-file-name "savehist" user-emacs-directory)
      history-length 10000
      savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history)
      savehist-autosave-interval 60)

(add-hook 'emacs-startup-hook #'savehist-mode)

;; Icomplete.
(setq icomplete-compute-delay 0.1
      icomplete-separator " "
      icomplete-prospects-height 1
      icomplete-hide-common-prefix nil
      icomplete-tidy-shadowed-file-names t
      icomplete-show-matches-on-no-input t
      completion-in-region-function #'ec--completion-in-region)

(with-eval-after-load 'icomplete
  (keymap-set icomplete-minibuffer-map "C-p" #'icomplete-backward-completions)
  (keymap-set icomplete-minibuffer-map "C-n" #'icomplete-forward-completions)
  (keymap-set icomplete-minibuffer-map "C-w" #'evil-delete-backward-word)

  (keymap-set icomplete-minibuffer-map "RET" #'icomplete-force-complete-and-exit)
  (keymap-set icomplete-minibuffer-map "M-RET" #'exit-minibuffer)

  (keymap-set completion-list-mode-map "k" #'previous-line)
  (keymap-set completion-list-mode-map "j" #'next-line)
  (keymap-set completion-list-mode-map "h" #'minibuffer-previous-completion)
  (keymap-set completion-list-mode-map "l" #'minibuffer-next-completion))

(defun ec--completion-in-region(start end collection &optional predicate)
  "Make in-buffer completion use icomplete.

See `completion-in-region' for the descriptions of START, END,
COLLECTION, and PREDICATE."
  (if (minibufferp)
      (completion--in-region start end collection predicate)
    (consult-completion-in-region start end collection predicate)))

(add-hook 'emacs-startup-hook #'icomplete-mode)

;; Consult.
(keymap-set global-map "C-c m" #'consult-man)
(keymap-set global-map "C-c f r" #'consult-recent-file)
(keymap-set global-map "M-g f" #'consult-flymake)

(keymap-set global-map "<remap> <switch-to-buffer>" #'consult-buffer)
(keymap-set global-map "<remap> <bookmark-jump>" #'consult-bookmark)
(keymap-set global-map "<remap> <insert-register>" #'consult-register)
(keymap-set global-map "<remap> <project-switch-to-buffer>" #'consult-project-buffer)

(keymap-set global-map "M-g o" #'consult-outline)
(keymap-set global-map "M-g i" #'consult-imenu)
(keymap-set global-map "M-g I" #'consult-imenu-multi)
(with-eval-after-load 'org-mode
  (keymap-set org-mode-map "M-g o" #'consult-org-heading)
  (keymap-set org-mode-map "M-g O" #'consult-org-agenda)
  ;; Remove `org-cycle' binding so TAB does completion in headings.  In normal
  ;; mode TAB will still cycle.
  (keymap-set org-mode-map "<TAB>" nil))
(keymap-set global-map "<remap> <goto-line>" #'consult-goto-line)
(keymap-set global-map "M-g m" #'consult-mark)
(keymap-set global-map "M-g M" #'consult-global-mark)

(keymap-set global-map "M-s g" #'consult-ripgrep)
(keymap-set global-map "M-s d" #'consult-fd)
(keymap-set global-map "M-s k" #'consult-keep-lines)
(keymap-set global-map "M-s u" #'consult-focus-lines)

;; Make TAB work for completion.  Unfortunately `indent-for-tab-command' does
;; not seem to work correctly in Markdown (for some reason it always puts my
;; cursor at the start of the line), so go straight to completion instead.
(with-eval-after-load 'markdown-mode
  (keymap-set markdown-mode-map "<TAB>" #'completion-at-point))

(setq register-preview-function #'consult-register-format
      xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; `consult-preview-at-point-mode' has no autoload.
(autoload 'consult-preview-at-point-mode "consult")

(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

(with-eval-after-load 'evil
  (keymap-set evil-normal-state-map "C-y" #'consult-yank-from-kill-ring)
  (keymap-set evil-insert-state-map "C-y" #'consult-yank-from-kill-ring))

;; Search.
(setq lazy-highlight-initial-delay 1)

;; Find file at point.
(keymap-set global-map "<remap> <find-file>" #'ec-ffap)

;; `ffap-string-at-point' doesn't have an autoload so `ec-ffap' will fail
;; without this (or without manually loading `ffap').
(autoload 'ffap-string-at-point "ffap")

(defun ec-ffap ()
  "Like `find-file-at-point' but handles line/column numbers and remote paths."
  (interactive)
  ;; In `dired' this gets in the way of using C-x C-f to create files.  Use RET
  ;; instead to go to the file at point.
  (if (eq major-mode 'dired-mode)
      (call-interactively ffap-file-finder)
    ;; Otherwise handle line/column numbers and remote paths before delegating
    ;; to `find-file-at-point'.
    (let* ((str (ffap-string-at-point))
           (index (seq-some (lambda (r) (string-match r str))
                            '(":\\([0-9]+\\):\\([0-9]+\\)$"
                              ":\\([0-9]+\\)$"
                              "#L\\([0-9]+\\)$"
                              "#L\\([0-9]+\\)-L\\([0-9]+\\)$")))
           (line (when index (match-string 1 str)))
           (column (when index (match-string 2 str)))
           (file (if index (substring str 0 index) str))
           ;; Only check non-remote paths for existence.
           (exists (or (ffap-file-remote-p file) (ffap-file-exists-string file))))
      ;; Use the prompter to provide a chance to verify.
      (find-file-at-point (ffap-prompter (when exists file)))
      (when (and exists index)
        (goto-char (point-min))
        (forward-line (1- (string-to-number line)))
        (when column
          (forward-char (1- (string-to-number column))))))))

;; Completion sources..
(setq dabbrev-case-replace nil
      cape-dabbrev-min-length 0
      ;; It does not appear possible to use aspell databases for completion, so
      ;; a separate plain text word list is needed.
      cape-dict-file (list ispell-personal-dictionary
                           (getenv "WORDLIST")))

(when (fboundp 'cape-capf-prefix-length)
  (defalias 'ec--capf (cape-capf-prefix-length
                       (cape-capf-super
                        #'cape-dabbrev ; `dabbrev-capf' exists but it errors.
                        #'cape-dict    ; `ispell-complete-word' might also.
                        #'cape-keyword
                        #'cape-line)
                       1))

  (defalias 'ec--capf-file (cape-capf-prefix-length #'cape-file 1))
  (defalias 'ec--capf-emoji (cape-capf-prefix-length #'cape-emoji 1))
  (defalias 'ec--capf-template (cape-capf-prefix-length #'tempel-complete 1))

  (defun ec--add-capf (&optional global)
    "Add capf functions to GLOBAL hook if non-nil, else local."
    (let ((local (not global)))
      (add-hook 'completion-at-point-functions #'ec--capf -10 local)
      (add-hook 'completion-at-point-functions #'ec--capf-template -10 local)
      (add-hook 'completion-at-point-functions #'ec--capf-file -10 local)
      (add-hook 'completion-at-point-functions #'ec--capf-emoji -10 local)
      (when local
        ;; Do not run the global hook; everything is already added locally.
        (delq t completion-at-point-functions))))

  (add-hook 'emacs-lisp-mode-hook #'ec--add-capf)

  (defun ec--eglot-capf ()
    "Add back capf functions."
    (when (eglot-managed-p) (ec--add-capf)))

  (add-hook 'eglot-managed-mode-hook #'ec--eglot-capf)

  (ec--add-capf t))

;; Transient.
(setq transient-history-file (expand-file-name "transient/history.el" (xdg-data-home)))

;;; completion.el ends here
