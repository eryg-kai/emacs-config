;;; theme.el --- Theme. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(doom-themes
                                   evil-terminal-cursor-changer))

(setq window-divider-default-places t
      truncate-lines t
      scroll-margin 8
      hscroll-margin 8
      hscroll-step 50
      scroll-conservatively 101)

(blink-cursor-mode 0)

;; Set fonts for GUI Emacs.
(when (display-graphic-p)
  (set-frame-font (font-spec :family "Inconsolata Nerd Font" :size 12.0) t t)
  (set-fontset-font t 'unicode (font-spec :name "Source Han Code JP" :size 10.0) nil)
  (set-fontset-font t 'unicode (font-spec :name "Source Han Sans HW K" :size 12.0) nil))

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
        (set cursor-symbol `(,color ,cursor)))))
  (when (fboundp 'evil-refresh-cursor) (evil-refresh-cursor)))

(with-eval-after-load 'evil
  (dolist (state '(normal insert visual replace motion operator emacs))
    (eval `(defface ,(intern (format "evil-%s-state" state))
             `((t (:inherit 'mode-line)))
             (format "%s state face." (capitalize (format "%s" state)))
             :group 'evil)
          `((state . ,state)))))

(defun ec--doom-fallback (fn name docstring defs &rest args)
  "Ensure DEFS has all theme values set.

Once the values are set call FN with NAME, DOCSTRING, modified DEFS, and ARGS.

In particular, doom-one doesn't define bg and bg-alt for the terminal so many
faces are simply invisible."
  (dolist (def defs)
    (when (and (listp (cadr def))
               (listp (cadadr def))
               (consp (cdr (cadadr def)))
               (not (cadr (cadadr def))))
      (setcar (cdr (cadadr def)) "black")))
  (apply fn name docstring defs args))

(advice-add 'def-doom-theme :around #'ec--doom-fallback)

(defun ec--load-theme (&rest _)
  "Load the current theme'."
  (load-theme (or (bound-and-true-p ec--current-theme) (car ec-themes)) t))

(when (ec-theme-p 'doom-one)
  ;; Set theme on new frames. This will make emacsclient invocations themed when
  ;; using emacs --daemon.
  (cond ((daemonp) (add-hook 'after-make-frame-functions #'ec--load-theme))
        ;; Otherwise load on startup.
        (t (add-hook 'emacs-startup-hook #'ec--load-theme))))

(with-eval-after-load 'org
  (defface org-bullet  '((t (:inherit org-priority :height 0.9)))
    "Face for Org bullets.")

  (setq org-ellipsis "  " ;; ⤵, ⮷, …, ▼, ⋱, ↴, ⬎, ⤷
        org-bullets-face-name 'org-bullet
        org-bullets-bullet-list
        '(;; "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"
          ;; "∙"
          ;; "◉" "○" "✸"
          ;; "✿"
          ;; "■" "◆" "▲" "▶"
          ;; ""
          ;; "➔"
          "→")
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
        (tab-mark     ?\t [?\u00BB ?\t] [?\\ ?\t]))
      whitespace-style
      '(face tabs spaces trailing space-before-tab newline indentation
             empty space-after-tab space-mark tab-mark newline-mark))

;; Customize the truncate and wrap markers.
(with-eval-after-load 'whitespace
  (set-display-table-slot standard-display-table
                          'truncation (make-glyph-code ?\… 'whitespace-tab))
  (set-display-table-slot standard-display-table
                          'wrap (make-glyph-code ?\↙ 'whitespace-tab)))

(add-hook 'emacs-startup-hook #'global-whitespace-mode)

;; Fill column indicator.
(setq-default display-fill-column-indicator-character ?\〱)

(add-hook 'emacs-startup-hook #'global-display-fill-column-indicator-mode)

;; Line highlight.
(add-hook 'emacs-startup-hook #'global-hl-line-mode)

(with-eval-after-load 'evil
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate)))

;;; theme.el ends here
