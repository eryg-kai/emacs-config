;;; audio.el --- Audio configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(pipewire))

(defun ec-play-toggle ()
  "Play or pause audio."
  (interactive)
  (ec-exec "playerctl play-pause"))

(defun ec-play-previous ()
  "Go to previous audio track."
  (interactive)
  (ec-exec "playerctl previous"))

(defun ec-play-next ()
  "Go to next audio track."
  (interactive)
  (ec-exec "playerctl next"))

;;; audio.el ends here
