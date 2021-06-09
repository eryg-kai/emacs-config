;;; profiler.el --- For profiling startup times. -*- lexical-binding: t -*-

;;; Commentary:

;; Load both early-init.el and init.el for profiling.
;; To use run (esup (buffer-file-name)) from this file.

;;; Code:

(let ((dir (file-name-directory load-file-name)))
  (load-file (expand-file-name "early-init.el" dir))
  (load-file (expand-file-name "init.el" dir)))

;;; profiler.el ends here
