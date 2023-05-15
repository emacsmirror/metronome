;;; metronome.el --- The missing metronome for GNU Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023 Jonathan Gregory

;; Version: 0.2
;; Package-Requires: ((emacs "25.1"))
;; URL: https://git.sr.ht/~jagrg/metronome
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

;; This is a very simple metronome for GNU Emacs. To install it from
;; source, add metronome.el to your load path and require it. Then M-x
;; metronome to play/pause, and C-u M-x metronome to set a new tempo.

;; (require 'metronome)
;; (global-set-key (kbd "C-c C-m") 'metronome)

;;; Code:

(require 'timer)

(defgroup metronome nil
  "Metronome utilities."
  :group 'metronome)

(defcustom metronome-click
  (when load-file-name
    (concat (file-name-directory load-file-name) "sounds/low.wav"))
  "The filename of the click sound."
  :type 'file)

(defvar metronome-timer nil
  "The timer of the metronome.")

(defvar metronome-tempo nil
  "The last tempo defined.")

(defvar metronome-paused-p nil
  "Whether the metronome is paused.")


;;; Core functions

(defun metronome-play-click ()
  "Play a click sound."
  (play-sound `(sound :file ,metronome-click)))

(defun metronome-duration (bpm)
  "Calculate the duration in seconds between cycles."
  (/ 60 (float bpm)))

(defun metronome-cancel-timers ()
  "Cancel all metronome timers."
  (dolist (timer timer-list)
    (when-let (fn (aref timer 5))
      (when-let (fn (and (symbolp fn)
                         (symbol-name fn)))
	;; Only cancel timers running metronome functions
	(when (string-match "metronome-play-click" fn)
	  (cancel-timer timer))))))

(defun metronome-stop ()
  "Stop the metronome."
  (when metronome-timer
    (metronome-pause)
    (setq metronome-tempo nil)))

(defun metronome-start (bpm)
  "Start metronome at BPM beats per minute."
  (metronome-stop)
  (let* ((tempo (if (null (symbolp bpm)) bpm
		  (string-to-number
		   (read-from-minibuffer "Tempo: "))))
	 (tempo (if (= tempo 0) 120 tempo))
	 (interval (metronome-duration tempo)))
    (setq metronome-timer
	  (run-at-time nil interval #'metronome-play-click))
    (setq metronome-tempo tempo
	  metronome-paused-p nil)))

(defun metronome-pause ()
  "Pause the metronome."
  (when metronome-timer
    (metronome-cancel-timers)
    (setq metronome-paused-p t)))

(defun metronome-resume ()
  "Resume the metronome."
  (if-let (last-bpm metronome-tempo)
      (metronome-start last-bpm)
    (metronome-start 'prompt))
  (setq metronome-paused-p nil))


;;;###autoload
(defun metronome-set-tempo ()
  "Set a new tempo."
  (interactive)
  (metronome-start 'prompt)
  (metronome nil))

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
