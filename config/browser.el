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

;;; browser.el ends here
