;;; theme.el --- Theme. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(doom-themes
                                   evil-terminal-cursor-changer))

(defun ec--disable-scroll-margin ()
  "Disable scroll margins in the current buffer."
  (setq-local scroll-margin 0)
  (setq-local hscroll-margin 0))

(add-hook 'term-mode-hook #'ec--disable-scroll-margin)

(setq window-divider-default-places t
      window-divider-default-right-width 5
      window-divider-default-bottom-width 5
      scroll-margin 8
      hscroll-margin 8
      max-mini-window-height 0.1
      truncate-string-ellipsis "…"
      vc-annotate-background-mode nil)

(add-hook 'emacs-startup-hook #'window-divider-mode)

(blink-cursor-mode 0)

;; Theme overrides.
(defvar ec-themes '(doom-one doom-solarized-light) "List of themes to cycle between.")

(defvar ec--current-theme nil "The current theme.")

(defvar ec-override-suffix "override" "Suffix for override themes.")

(defvar ec-evil-cursors
  '((normal   . box)
    (insert   . (bar  . 2))
    (emacs    . box)
    (replace  . (hbar . 2))
    (visual   . (hbar . 2))
    (motion   . box)
    (operator . box))
  "Cursor types (`cursor-type') for each Evil state.")

(defun ec-theme-p (theme)
  "Return non-nil if THEME exists."
  (locate-file
   (concat (symbol-name theme) "-theme.el")
   (custom-theme--load-path)
   '("" "c")))

(defun ec--try-load-theme (theme &optional no-confirm no-enable)
  "Call `load-theme' with THEME, NO-CONFIRM, and NO-ENABLE if THEME exists."
  (when (bound-and-true-p ec-debug-p)
    (message (format "=== ec--try-load-theme %s" theme)))
  (when (ec-theme-p theme)
    (load-theme theme no-confirm no-enable)))

(defun ec--disable-themes (theme &rest _args)
  "Disable all current themes.

If THEME is an override theme, do nothing."
  (let ((theme-s (symbol-name theme)))
    (unless (or (string= ec-override-suffix theme-s)
                (string-suffix-p ec-override-suffix theme-s))
      (mapcar #'disable-theme custom-enabled-themes))))

(advice-add 'load-theme :before #'ec--disable-themes)

(defun ec--load-overrides (theme &optional no-confirm no-enable)
  "Load THEME and overrides with NO-CONFIRM and NO-ENABLE.

If THEME is an override theme (ends in `override'), do nothing."
  (let ((theme-s (symbol-name theme))
        (theme-prefix nil))
    (unless (or (string= ec-override-suffix theme-s)
                (string-suffix-p ec-override-suffix theme-s))
      (setq ec--current-theme theme)
      (ec--try-load-theme (intern ec-override-suffix) no-confirm no-enable)
      (dolist (part (split-string (symbol-name theme) "-"))
        (setq theme-prefix (if theme-prefix (concat theme-prefix "-" part) part))
        (dolist (sep (if (display-graphic-p) '("-") '("-" "-tty-")))
          (ec--try-load-theme
           (intern (concat theme-prefix sep ec-override-suffix))
           no-confirm no-enable)))
      (setq vc-annotate-background nil)
      (ec--evil-set-cursor-faces))))

(advice-add 'load-theme :after #'ec--load-overrides)

(defmacro ec-def-theme (name docstring &rest faces)
  "Define a theme named NAME with specified DOCSTRING and FACES."
  `(progn
     (deftheme ,name ,docstring)
     (custom-theme-set-faces
      ',name
      ,@(mapcar
         (lambda (face)
           `(list
             ',(car face)
             (list (list 't (list ,@(cdr face))))))
         faces))
     (provide-theme ',name)))

(defun ec-cycle-theme ()
  "Cycle through themes."
  (interactive)
  (let* ((pos (cl-position ec--current-theme ec-themes))
         (theme (or (and pos (nth (+ 1 pos) ec-themes)) (car ec-themes))))
    (ec--try-load-theme theme t)))

(defun ec--evil-set-cursor-faces ()
  "Set cursor faces for `evil-mode'."
  (dolist (pair ec-evil-cursors)
    (let* ((state (car pair))
           (cursor (cdr pair))
           (cursor-symbol (intern (format "evil-%s-state-cursor" state)))
           (state-symbol (intern (format "evil-%s-state" state)))
           (color (when (facep state-symbol)
                    (face-attribute state-symbol :foreground))))
      (when (boundp cursor-symbol)
        (set cursor-symbol `(,cursor ,color)))))
  (when (fboundp 'evil-refresh-cursor) (evil-refresh-cursor)))

(with-eval-after-load 'evil
  (dolist (state '(normal insert visual replace motion operator emacs))
    (eval `(defface ,(intern (format "evil-%s-state" state))
             `((t (:inherit 'mode-line)))
             (format "%s state face." (capitalize (format "%s" state)))
             :group 'evil)
          `((state . ,state)))))

(defun ec--load-theme (&rest _)
  "Load the current theme'."
  (set-fontset-font t 'unicode (font-spec :name "Noto Sans Mono CJK KR" :size 10.0) nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :name "Noto Sans Mono CJK JP" :size 10.0) nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :name "Symbols Nerd Font" :size 10.0) nil 'prepend)

  ;; Main font.
  (add-to-list 'default-frame-alist '(font . "Inconsolata"))

  (load-theme (or (bound-and-true-p ec--current-theme) (car ec-themes)) t))

(when (ec-theme-p 'doom-one)
  ;; Set theme on new frames. This will make emacsclient invocations themed when
  ;; using emacs --daemon.
  (cond ((daemonp) (add-hook 'server-after-make-frame-hook #'ec--load-theme))
        ;; Otherwise load on startup unless it looks like a virtual console.
        ((or (display-graphic-p)
             (not (string= "linux" (getenv "TERM"))))
         (add-hook 'emacs-startup-hook #'ec--load-theme))))

(with-eval-after-load 'org
  (defface org-bullet  '((t (:inherit org-priority :height 0.9)))
    "Face for Org bullets.")

  (setq org-ellipsis (if (display-graphic-p) "  " " ▼ ") ;; ⤵ ⮷ … ⋱ ↴ ⬎ ⤷
        org-bullets-face-name 'org-bullet
        org-bullets-bullet-list
        '(;; "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"
          ;; "∙"
          ;; "◉" "○" "✸"
          ;; "✿"
          ;; "■" "◆" "▲" "▶"
          ;; ""
          ;; "➔"
          "")
        org-habit-completed-glyph ?>
        org-habit-today-glyph ?=
        org-todo-keyword-faces
        '(("TASK"   :weight bold :inherit org-todo)
          ("MEET"   :weight bold :inherit org-todo)
          ("CALL"   :weight bold :inherit org-todo)
          ("START"  :weight bold :inherit org-date)
          ("[-]"    :weight bold :inherit org-date)
          ("PART"   :weight bold :inherit org-date)
          ("[~]"    :weight bold :inherit org-date)
          ("NEXT"   :weight bold :inherit org-formula)
          ("[>]"    :weight bold :inherit org-formula)
          ("WAIT"   :weight bold :inherit org-done :strike-through nil)
          ("[?]"    :weight bold :inherit org-done :strike-through nil)
          ("CANCEL" :weight bold :inherit org-done)
          ("[!]"    :weight bold :inherit org-done)))

  (doom-themes-org-config))

;; Paren.
(add-hook 'emacs-startup-hook #'show-paren-mode)

;; Whitespace.
(setq whitespace-display-mappings
      '((space-mark   ?\  [?·])
        (newline-mark ?\n [?$ ?\n])
        (tab-mark     ?\t [?\» ?\t] [?\\ ?\t]))
      whitespace-style
      '(face tabs spaces trailing space-before-tab newline indentation
             empty space-after-tab space-mark tab-mark newline-mark))

;; Customize the truncate and wrap markers.
(with-eval-after-load 'whitespace
  (set-display-table-slot standard-display-table
                          'truncation (make-glyph-code ?\… 'whitespace-tab))
  (set-display-table-slot standard-display-table
                          'wrap (make-glyph-code
                                 (if (display-graphic-p) ?\↙ ?\▼) 'whitespace-tab)))

(add-hook 'emacs-startup-hook #'global-whitespace-mode)

;; Global whitespace mode does not turn on for "special" buffers (with
;; asterisks) so turn it on manually when necessary.
(add-hook 'message-mode-hook 'whitespace-mode)

(defface whitespace-hard-newline
  '((t (:inherit 'error))) "Used for hard newlines.")

(defun ec--mark-hard-newlines (beg end &optional object)
  "Visibly mark hard newlines between BEG and END in OBJECT."
  (let ((sticky (get-text-property beg 'rear-nonsticky object)))
    (put-text-property beg end 'display
                       (copy-sequence
                        (propertize
                         "¶\n"
                         'font-lock-face 'whitespace-hard-newline))
                       object)
    (if (and (listp sticky) (not (memq 'display sticky)))
        (put-text-property beg end 'rear-nonsticky
                           (cons 'display sticky)
                           object))))

(ec--mark-hard-newlines 0 (length hard-newline) hard-newline)

(advice-add #'set-hard-newline-properties :after #'ec--mark-hard-newlines)

;; Fill column indicator.
(setq-default display-fill-column-indicator-character
              (if (display-graphic-p) ?\〱 ?\│))

(add-hook 'emacs-startup-hook #'global-display-fill-column-indicator-mode)

;; Line highlight.
(add-hook 'emacs-startup-hook #'global-hl-line-mode)

(with-eval-after-load 'evil
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate)))

;;; theme.el ends here
