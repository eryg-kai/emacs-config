;;; audio.el --- Audio configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun ec-decrease-volume ()
  "Decrease current sink's volume."
  (interactive)
  (ec-exec "pactl set-sink-volume @DEFAULT_SINK@ -5%"))

(defun ec-increase-volume ()
  "Increase current sink's volume."
  (interactive)
  (ec-exec "pactl set-sink-volume @DEFAULT_SINK@ +5%"))

(defun ec-toggle-muted ()
  "Mute or unmute current sink."
  (interactive)
  (ec-exec "pactl set-sink-mute @DEFAULT_SINK@ toggle"))

(defun ec-toggle-microphone ()
  "Mute or unmute current source."
  (interactive)
  (ec-exec "pactl set-source-mute @DEFAULT_SOURCE@ toggle"))

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
