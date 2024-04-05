;;; audio.el --- Audio configuration. -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun ec-decrease-volume ()
  "Decrease current sink's volume."
  (interactive)
  (ec-exec-and-display "pactl set-sink-volume @DEFAULT_SINK@ -5%"
                       "&&" "pactl get-sink-volume @DEFAULT_SINK@"))

(defun ec-increase-volume ()
  "Increase current sink's volume."
  (interactive)
  (ec-exec-and-display "pactl set-sink-volume @DEFAULT_SINK@ +5%"
                       "&&" "pactl get-sink-volume @DEFAULT_SINK@"))

(defun ec-toggle-muted ()
  "Mute or unmute current sink."
  (interactive)
  (ec-exec-and-display "pactl set-sink-mute @DEFAULT_SINK@ toggle"
                       "&&" "pactl get-sink-mute @DEFAULT_SINK@"))

(defun ec-toggle-microphone ()
  "Mute or unmute current source."
  (interactive)
  (ec-exec-and-display "pactl set-source-mute @DEFAULT_SOURCE@ toggle"
                       "&&" "pactl get-source-mute @DEFAULT_SOURCE@"))

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
