;;; completion.el --- Provide completion. -*- lexical-binding: t -*-

;;; Commentary:

;; Completions for code, files, keys, etc and anything to do with prompts.

;;; Code:

(nconc package-selected-packages '(which-key
                                   yasnippet
                                   yasnippet-snippets
                                   ispell
                                   consult))

(setq history-delete-duplicates t
      completion-auto-select t)

;; Apropos.
(setq apropos-do-all t)

;; Which-key.
(setq which-key-idle-delay 0.2)

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
    "C-c &" "snippet"
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

;; Snippets.
(setq yas-indent-line 'fixed)

(with-eval-after-load 'yasnippet
  (define-key yas-minor-mode-map (kbd "TAB") #'hippie-expand)
  (push #'yas-hippie-try-expand hippie-expand-try-functions-list)

  ;; In the terminal smartparens-strict-mode causes some weirdness with
  ;; parenthesis in snippets so disable it while expanding.
  (defvar ec--smartparens-mode)

  (defun ec--disable-smartparens (&rest _)
    (setq ec--smartparens-mode smartparens-mode)
    (setq smartparens-mode nil))

  (advice-add 'yas-hippie-try-expand :after #'ec--disable-smartparens)

  (defun ec--reset-smartparens (&rest _)
    (setq smartparens-mode ec--smartparens-mode))

  (advice-add 'hippie-expand :after #'ec--reset-smartparens '((depth . -100))))

(when (fboundp 'yas-minor-mode)
  (add-hook 'text-mode-hook #'yas-minor-mode)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

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
(setq completion-auto-help t
      completion-auto-select t
      completion-ignore-case t
      completion-styles '(flex)
      completion-category-overrides '((file (styles . (substring)))
                                      ;; `consult' uses multi-category.
                                      (multi-category (styles . (substring))))

      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      icomplete-compute-delay 0.1
      icomplete-separator " "
      icomplete-prospects-height 1
      icomplete-hide-common-prefix nil
      icomplete-tidy-shadowed-file-names t
      icomplete-show-matches-on-no-input t)

(with-eval-after-load 'icomplete
  (define-key icomplete-minibuffer-map (kbd "C-p") #'icomplete-backward-completions)
  (define-key icomplete-minibuffer-map (kbd "C-n") #'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map (kbd "C-w") #'evil-delete-backward-word)

  (define-key completion-list-mode-map (kbd "k") #'previous-line)
  (define-key completion-list-mode-map (kbd "j") #'next-line)
  (define-key completion-list-mode-map (kbd "h") #'minibuffer-previous-completion)
  (define-key completion-list-mode-map (kbd "l") #'minibuffer-next-completion))

(add-hook 'emacs-startup-hook #'icomplete-mode)

;; Consult.
(define-key global-map (kbd "C-c m") #'consult-man)
(define-key global-map (kbd "C-c fr") #'consult-recent-file)
(define-key global-map (kbd "M-g f") #'consult-flymake)

(define-key global-map (kbd "C-x b") #'consult-buffer)          ;; original: switch-to-buffer
(define-key global-map (kbd "C-x rb") #'consult-bookmark)       ;; original: bookmark-jump
(define-key global-map (kbd "C-x ri") #'consult-register)       ;; original: insert-register
(define-key global-map (kbd "C-x pb") #'consult-project-buffer) ;; original: project-switch-to-buffer

(define-key global-map (kbd "M-g o") #'consult-outline)
(define-key global-map (kbd "M-g i") #'consult-imenu)
(define-key global-map (kbd "M-g I") #'consult-imenu-multi)
(with-eval-after-load 'org-mode
  (define-key org-mode-map (kbd "M-g o") #'consult-org-heading)
  (define-key org-mode-map (kbd "M-g O") #'consult-org-agenda))
(define-key global-map (kbd "M-g g") #'consult-goto-line)    ;; original: goto-line
(define-key global-map (kbd "M-g M-g") #'consult-goto-line)  ;; original: goto-line
(define-key global-map (kbd "M-g m") #'consult-mark)
(define-key global-map (kbd "M-g M") #'consult-global-mark)

(define-key global-map (kbd "M-s g") #'consult-ripgrep)
(define-key global-map (kbd "M-s d") #'consult-fd)
(define-key global-map (kbd "M-s k") #'consult-keep-lines)
(define-key global-map (kbd "M-s u") #'consult-focus-lines)

(defun ec--completion-in-region(start end collection &optional predicate)
  "Make in-buffer completion use icomplete.

See `completion-in-region' for the descriptions of START, END,
COLLECTION, and PREDICATE."
  (if (minibufferp)
      (completion--in-region start end collection predicate)
    (consult-completion-in-region start end collection predicate)))

(setq register-preview-function #'consult-register-format
      xref-show-xrefs-function #'consult-xref
      completion-in-region-function #'ec--completion-in-region
      xref-show-definitions-function #'consult-xref)

;; `consult-preview-at-point-mode' has no autoload.
(autoload 'consult-preview-at-point-mode "consult")

(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-y") #'consult-yank-from-kill-ring)
  (define-key evil-insert-state-map (kbd "C-y") #'consult-yank-from-kill-ring))

;; Search.
(setq lazy-highlight-initial-delay 1)

;; Find file at point.
(define-key global-map (kbd "C-x C-f") #'ec-ffap)

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
           (index (string-match ":[0-9]+\\(:[0-9]+\\)?$" str))
           (file (if index (substring str 0 index) str))
           (position (when index (split-string (substring str (1+ index)) ":")))
           ;; Only check non-remote paths for existence.
           (exists (or (ffap-file-remote-p file) (ffap-file-exists-string file))))
      ;; Use the prompter to provide a chance to verify.
      (find-file-at-point (ffap-prompter (when exists file)))
      (when (and exists (car position))
        (goto-char (point-min))
        (forward-line (1- (string-to-number (car position))))
        (when (cadr position)
          (forward-char (1- (string-to-number (cadr position)))))))))

;;; completion.el ends here
