;;; browser.el --- Browsing the web. -*- lexical-binding: t -*-

;;; Commentary:

;; Configuration for things that browse the internet.

;;; Code:

(setq browse-url-handlers
      '(("github" . browse-url-firefox)
        ("google" . browse-url-chrome)
        ("." . eww-browse-url)))

;; TODO: When going back it doesn't update.
(defun ec--eww-set-buffer-title ()
  "Set the EWW buffer title to the page's title."
  (let* ((title  (plist-get eww-data :title))
         (result (if (string= "" title) "*eww*" (concat "*" title "*"))))
    (rename-buffer result t)))

(add-hook 'eww-after-render-hook #'ec--eww-set-buffer-title)

(setq shr-use-fonts nil
      shr-max-image-proportion 0.2)

;;; browser.el ends here
