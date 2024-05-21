;;; browser.el --- Browsing the web. -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for things that browse the internet.

;;; Code:

(defun ec--browse-url-xdg (url &rest _)
  "Use xdg-open to view the URL."
  (interactive)
  (ec-exec "xdg-open" url))

(setq browse-url-handlers
      '(("\\.mp4$" . ec--browse-url-xdg)
        ("." . eww-browse-url))
      browse-url-secondary-browser-function #'browse-url-firefox
      eww-auto-rename-buffer 'url
      eww-buffer-name-length most-positive-fixnum
      shr-use-fonts nil
      shr-max-image-proportion 1)

(defun ec--shr-bg (fn start end fg &optional bg)
  "Call FN with START, END, and FG, ignoring BG."
  (apply fn start end fg nil))

(advice-add #'shr-colorize-region :around #'ec--shr-bg)

(defcustom ec-url-transforms nil "Pairs of replacements to perform on URLs."
  :type '(alist :key-type string :value-type string)
  :group 'eww)

(defun ec--transform-url (url)
  "Transform URL."
  (seq-reduce
   (lambda (result pair)
     (string-replace (car pair) (cdr pair) result))
   ec-url-transforms
   url))

(with-eval-after-load 'eww
  (push #'ec--transform-url eww-url-transformers))

;; This is just to avoid having to make a separate function to create generic
;; URL bookmarks.
(defun ec-bookmark-url-jump (bookmark)
  "Open BOOKMARK using `browse-url'."
  (browse-url (bookmark-prop-get bookmark 'location)))

(advice-add #'eww-bookmark-jump :override #'ec-bookmark-url-jump)

;;; browser.el ends here
