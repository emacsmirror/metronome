;;; metronome.el --- A simple metronome for GNU Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jonathan Gregory

;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://gitlab.com/jagrg/metronome
;; Author: Jonathan Gregory <jgrg at autistici dot org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simple metronome for GNU Emacs. To install it from
;; source, add metronome.el to your load path and require it. Then
;; press `M-x metronome' to start the metronome. The first call will
;; prompt you for a tempo. Subsequent calls will either pause or
;; resume the metronome. To change tempo press C-u M-x metronome.

;;; Code:

(require 'cl-lib)

(defgroup metronome nil
  "Metronome utilities."
  :group 'metronome)

(defcustom metronome-high-click-sound (when load-file-name
					(concat (file-name-directory load-file-name) "high.wav"))
  "The filename of the high click sound."
  :type 'file)

(defcustom metronome-low-click-sound (when load-file-name
				       (concat (file-name-directory load-file-name) "low.wav"))
  "The filename of the low click sound."
  :type 'file)

(defvar metronome-timer nil
  "The timer of the metronome.")

(defvar metronome-tempo nil
  "The last tempo defined.")

(defvar metronome-paused-p nil
  "Whether the metronome is paused.")

(defun metronome-high-click (&optional sleep)
  "Play high click sound.
With optional argument SLEEP, wait for SLEEP seconds."
  (play-sound-file metronome-high-click-sound)
  (when sleep (sit-for sleep)))

(defun metronome-low-click (&optional sleep)
  "Play low tick sound.
With optional argument SLEEP, wait for SLEEP seconds."
  (play-sound-file metronome-low-click-sound)
  (when sleep (sit-for sleep)))

;; This metronome also includes the number of beats per bar (based on:
;; https://rosettacode.org/wiki/Metronome). Start the metronome with:
;; (while t (metronome-beat 120 4)) and stop it with C-g, or:
;; (setq metronome-timer
;;       (run-at-time t (/ 60.0 (float bpm)) 'metronome-beats bpm bpb))

(defun metronome-beat (bpm bpb)
  "Play BPB clicks at BPM beats per minute.
Use the first click to distinguish the cycle."
  (let ((counter 0)
	(sleep (/ 60.0 (float bpm))))
    (cl-loop for i from 1 to bpb
	     do (cl-incf counter)
	     if (= (% counter bpb) 1)
	     collect (metronome-high-click sleep)
	     else
	     collect (metronome-low-click sleep))))

(defun metronome-start (bpm)
  "Start metronome at BPM beats per minute."
  (setq metronome-timer
	(run-at-time t (/ 60.0 (float bpm)) 'metronome-low-click))
  (setq metronome-tempo bpm
	metronome-paused-p nil))

(defun metronome-stop ()
  "Stop the metronome."
  (when-let (timer metronome-timer)
    (cancel-timer timer)
    (setq metronome-timer nil
	  metronome-tempo nil
	  metronome-paused-p t)))

(defun metronome-pause ()
  "Pause the metronome."
  (when-let (timer metronome-timer)
    (cancel-timer timer)
    (setq metronome-paused-p t)))

(defun metronome-resume ()
  "Resume the metronome."
  (if-let (bpm metronome-tempo)
      (metronome-start bpm)
    (metronome-start (read-number "Tempo: ")))
  (setq metronome-paused-p nil))

;;;###autoload
(defun metronome (arg)
  "Start/pause/resume metronome.
With a prefix ARG, prompt for a new tempo."
  (interactive "P")
  (if (or arg (null metronome-timer))
      (progn (metronome-stop)
	     (metronome-start (read-number "Tempo: ")))
    (if metronome-paused-p
	(metronome-resume)
      (metronome-pause))))

(provide 'metronome)
;;; metronome.el ends here
