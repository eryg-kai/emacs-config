;;; audio.el --- Audio configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(nconc package-selected-packages '(pulseaudio-control))

(setq pulseaudio-control-use-default-sink t)

(defalias 'ec-decrease-volume 'pulseaudio-control-decrease-volume)
(defalias 'ec-increase-volume 'pulseaudio-control-increase-volume)
(defalias 'ec-toggle-muted 'pulseaudio-control-toggle-current-sink-mute)
(defalias 'ec-toggle-microphone 'pulseaudio-control-toggle-current-source-mute)

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
