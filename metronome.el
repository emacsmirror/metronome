;;; metronome.el --- A simple metronome with accurate timing -*- lexical-binding: t; -*-

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

;; This is a metronome for GNU Emacs. To install it from source, add
;; metronome.el to your load path and require it. Then press M-x
;; metronome to start the metronome. The first call will prompt for
;; the tempo. Subsequent calls will either pause or resume the
;; metronome. To change tempo press C-u M-x metronome. There are two
;; ways to input the tempo. Either as an integer (the BPM) or as two
;; integers separated by space where the second integer is the number
;; of beats per bar.

;;; Code:

(require 'cl-lib)

(defgroup metronome nil
  "Metronome utilities."
  :group 'metronome)

(defcustom metronome-click
  (when load-file-name
    (concat (file-name-directory load-file-name) "sounds/low.wav"))
  "The filename of the low click sound."
  :type 'file)

(defcustom metronome-accent
  (when load-file-name
    (concat (file-name-directory load-file-name) "sounds/high.wav"))
  "The filename of the high click sound."
  :type 'file)

(defvar metronome-timer nil
  "The timer of the metronome.")

(defvar metronome-tempo nil
  "The last tempo defined.")

(defvar metronome-paused-p nil
  "Whether the metronome is paused.")

(defun metronome-start (bpm)
  "Start metronome at BPM beats per minute."
  (metronome-stop)
  (setq bpm (if (numberp bpm) bpm
	      (read-number "Tempo: ")))
  (setq metronome-timer
	(run-at-time t (/ 60.0 (float bpm)) 'metronome-play-click))
  (setq metronome-tempo bpm
	metronome-paused-p nil))
(defun metronome-play-click ()
  "Play low click sound."
  (play-sound `(sound :file ,metronome-click)))

(defun metronome-play-accent ()
  "Play high click sound."
  (play-sound `(sound :file ,metronome-accent)))


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
  (if-let (last-bpm metronome-tempo)
      (metronome-start last-bpm)
    (metronome-start 'prompt))
  (setq metronome-paused-p nil))

;;;###autoload
(defun metronome (arg)
  "Start/pause/resume metronome.
With a prefix ARG, prompt for a new tempo."
  (interactive "P")
  (if (or arg (null metronome-timer))
      (metronome-start 'prompt)
    (if metronome-paused-p
	(metronome-resume)
      (metronome-pause))))

(provide 'metronome)
;;; metronome.el ends here
