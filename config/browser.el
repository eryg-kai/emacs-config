;;; browser.el --- Browsing the web. -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for things that browse the internet.

;;; Code:

(setq browse-url-handlers
      '(("\\.mp4$" . browse-url-xdg-open)
        ("\\.webm$" . browse-url-xdg-open)
        ("." . eww-browse-url))
      browse-url-secondary-browser-function #'browse-url-firefox
      eww-auto-rename-buffer 'url
      eww-buffer-name-length most-positive-fixnum
      shr-use-fonts nil
      shr-max-image-proportion 1
      url-user-agent "Mozilla/5.0 (Windows NT 6.1; rv:31.0) Gecko/20100101 Firefox/31.0"
      url-cookie-file (expand-file-name "emacs/cookies" (xdg-data-home)))

(defun ec--normalize-url (url)
  "Normalize URL."
  ;; Breadcrumb links get marked as suspicious since the link does not match.
  (list (string-replace " › " "/" (car url))))

(advice-add #'url-generic-parse-url :filter-args #'ec--normalize-url)

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
