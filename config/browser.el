;;; browser.el --- Browsing the web. -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for things that browse the internet.

;;; Code:

(setq browse-url-handlers
      '(("." . eww-browse-url))
      browse-url-secondary-browser-function #'browse-url-firefox
      eww-auto-rename-buffer 'url
      eww-buffer-name-length most-positive-fixnum
      shr-use-fonts nil
      shr-max-image-proportion 0.2)

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

;;; browser.el ends here
