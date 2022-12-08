;;; audio.el --- Audio configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(pipewire))

(setq pipewire-osd-enable nil)

(defalias 'ec-decrease-volume 'pipewire-decrease-volume)
(defalias 'ec-increase-volume 'pipewire-increase-volume)
(defalias 'ec-toggle-muted 'pipewire-toggle-mute)
(defalias 'ec-toggle-microphone 'pipewire-toggle-microphone)

(defun ec-play-pause ()
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
