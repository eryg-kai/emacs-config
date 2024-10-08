;;; override-theme.el --- Generic theme overrides. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(ec-def-theme
 override "Generic theme overrides"

 (default :height 120)

 (region              :foreground 'unspecified :distant-foreground 'unspecified)
 (highlight           :foreground 'unspecified :distant-foreground 'unspecified)
 (lazy-highlight      :foreground 'unspecified :distant-foreground 'unspecified :background 'unspecified :inherit 'highlight)
 (match               :foreground 'unspecified :background 'unspecified :bold 'unspecified :inherit 'highlight)
 (secondary-selection :foreground 'unspecified :background 'unspecified :inherit 'match)

 (org-drill-hidden-cloze-face :foreground 'unspecified :background 'unspecified)

 (lsp-face-highlight-textual :foreground 'unspecified :distant-foreground 'unspecified :background 'unspecified :weight 'normal :inherit 'highlight)

 (erc-default-face :inherit 'unspecified)

 (dictionary-word-definition-face :font-family 'unspecified)

 (hl-todo   :foreground 'unspecified :inherit 'warning :weight 'bold)
 (hl-temp   :inherit 'evil-operator-face :weight 'bold)
 (hl-note   :inherit 'success :weight 'bold)
 (hl-review :inherit 'success :weight 'bold)
 (hl-fixme  :inherit 'error :weight 'bold)
 (hl-hack   :inherit 'error :weight 'bold)

 (fringe :foreground 'unspecified :inherit 'success)

 (line-number-current-line :background 'unspecified :inherit 'hl-line)

 (dired-marked          :foreground 'unspecified :background 'unspecified :inverse-video 'unspecified)
 (dired-broken-symlink  :foreground 'unspecified :background 'unspecified :inherit 'error)

 (whitespace-tab                                      :background 'unspecified)
 (whitespace-space           :foreground 'unspecified :background 'unspecified :inherit 'whitespace-tab)
 (whitespace-trailing        :foreground 'unspecified :background 'unspecified :inherit 'error)
 (whitespace-space-after-tab :foreground 'unspecified :background 'unspecified :inherit 'error)
 (whitespace-indentation     :foreground 'unspecified :background 'unspecified :inherit 'error)
 (whitespace-empty                                    :background 'unspecified)

 (org-hide :foreground 'unspecified :inherit 'whitespace-tab)

 (markdown-header-face-1 :inherit 'outline-1)
 (markdown-header-face-2 :inherit 'outline-2)
 (markdown-header-face-3 :inherit 'outline-3)
 (markdown-header-face-4 :inherit 'outline-4)
 (markdown-header-face-5 :inherit 'outline-5)
 (markdown-header-face-6 :inherit 'outline-6)

 (isearch-fail :foreground 'unspecified :background 'unspecified :inherit 'error)

 (outline-1 :background 'unspecified)

 (anzu-mode-line :foreground 'unspecified :bold 'unspecified)

 (sp-pair-overlay-face :background 'unspecified)

 (bookmark-face :background 'unspecified)

 (org-headline-done :strike-through t)

 (org-block            :background 'unspecified)
 (org-quote            :background 'unspecified)
 (org-block-begin-line :background 'unspecified)
 (org-block-end-line   :background 'unspecified)

 (org-date-selected :inverse-video 'unspecified :foreground 'unspecified :inherit 'font-lock-keyword-face :bold t)

 (org-drawer :foreground 'unspecified :inherit 'org-special-keyword)

 (org-agenda-clocking :background 'unspecified :underline t)

 (org-habit-overdue-future-face :background 'unspecified)

 (org-mode-line-clock-overrun :background 'unspecified :inherit 'error)

 (fill-column-indicator :family "Source Han Code JP" :height 0.7)

 (avy-lead-face   :background 'unspecified :inherit 'highlight)
 (avy-lead-face-0 :background 'unspecified :inherit 'highlight)
 (avy-lead-face-1 :background 'unspecified :inherit 'highlight)
 (avy-lead-face-2 :background 'unspecified :inherit 'highlight)

 (speed-type-correct :foreground 'unspecified :weight 'bold)

 (magit-diff-added                      :background 'unspecified)
 (magit-diff-added-highlight            :background 'unspecified)
 (magit-diff-removed                    :background 'unspecified)
 (magit-diff-removed-highlight          :background 'unspecified)
 (magit-diff-revision-summary           :background 'unspecified)
 (magit-diff-revision-summary-highlight :background 'unspecified)
 (magit-diff-file-heading               :background 'unspecified)
 (magit-diff-file-heading-highlight     :background 'unspecified)
 (magit-diff-hunk-heading               :background 'unspecified :foreground 'unspecified)
 (magit-diff-hunk-heading-highlight     :background 'unspecified :foreground 'unspecified)

 (help-key-binding :background 'unspecified)

 (magit-branch-remote-head :box 'unspecified :underline t :inherit 'magit-branch-remote)

 (consult-preview-insertion :inherit 'success)

 (tempel-field :background 'unspecified :foreground 'unspecified))

;;; override-theme.el ends here
