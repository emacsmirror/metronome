;;; metronome.el --- A simple metronome with accurate timing -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jonathan Gregory

;; Version: 1.1
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

(defun metronome-play-click ()
  "Play low click sound."
  (play-sound `(sound :file ,metronome-click)))

(defun metronome-play-accent ()
  "Play high click sound."
  (play-sound `(sound :file ,metronome-accent)))

(defun metronome-delays (bpm &optional bpb)
  "Return a list of seconds ber beat relative to beat one.
For example, 4 BPB at 120 BPM yields (0.0 0.5 1.0 1.5)."
  (let* ((secs)
	 (delay (/ 60 (float bpm)))
	 (wait delay)
	 (now (string-to-number
	       (format-time-string "%S" (current-time)))))
    (dotimes (i (or bpb 1))
      (push (+ now (cl-incf wait delay)) secs))
    (setq secs (mapcar (lambda (sec)
			 (- (car secs) sec))
		       secs))))

(defun metronome-duration (bpm &optional bpb)
  "Calculate the duration in seconds between cycles.
The duration is the time span of one full cycle of BPB beats per
bar at BPM beats per minute."
  (let ((secs (metronome-delays bpm (or bpb 1)))
	(delay (/ 60 (float bpm))))
    (* delay (length secs))))

(defun metronome-play-pattern (bpm &optional bpb)
  "Play metronome pattern once at BPM beats per minute.
With optional argument BPB, play a different sound on the first
of BPB beats per bar."
  (let ((secs (metronome-delays bpm (or bpb 1))))
    (dolist (i secs)
      (if (and (= i (car secs))
	       (> bpb 1))
	  (run-with-timer i nil 'metronome-play-accent)
	(run-with-timer i nil 'metronome-play-click)))))

(defun metronome-stop ()
  "Stop the metronome."
  (when-let (timer metronome-timer)
    (cancel-timer timer)
    (setq metronome-timer nil
	  metronome-tempo nil
	  metronome-paused-p t)))

(defun metronome-maybe-round (bpm)
  "Round BPM up or down if outside 30-250 range."
  (cond ((< bpm 30) 30)
	((> bpm 250) 250)
	(t bpm)))

(defun metronome-start (bpm)
  "Start metronome at BPM beats per minute.
BPM can be a list of integers where the first element is the BPM
and the second element is the BPB. It can also be a symbol, in
which case prompt for a new input."
  (let ((bpb (or (car-safe (cdr-safe bpm)) 1)))
    ;; First stop timer if running
    (metronome-stop)
    ;; Prompt if BPM is set to 'prompt
    (setq bpm (if (symbolp bpm)
		  (let* ((it (read-from-minibuffer "Tempo: "))
			 (it (split-string it "\s"))
			 (it (mapcar #'string-to-number it)))
		    ;; Set the bpb if there is one
		    (setq bpb (car-safe (cdr-safe it)))
		    ;; as well as the bpm
		    (car-safe it))
		;; If BPM is not a symbol, then it's an integer
		(or (car-safe bpm) bpm)))
    (setq bpm (metronome-maybe-round bpm))
    ;; Now set the timer to run metronome-play-pattern for WAIT secs
    (setq metronome-timer
	  (let ((wait (metronome-duration bpm (or bpb 1)))
		(bpb (or bpb 1)))
	    (run-at-time nil wait 'metronome-play-pattern bpm bpb)))
    (setq metronome-tempo (list bpm (or bpb 1))
	  metronome-paused-p nil)))

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

;;; Tap Tempo

(defvar metronome-elapsed-time nil)
(defconst metronome-cached-time (current-time))

(defun metronome-find-difference (seq)
  "Find the difference between consecutive numbers in SEQ."
  (let* ((l1 (remove (butlast seq) seq))
	 (l2 (remove (car seq) seq)))
    (cl-mapcar (lambda (x y)
		 (- (- x y)))
	       l2 l1)))

;;;###autoload
(defun metronome-tap-tempo ()
  "Tap to set new tempo when called two or more times successively."
  (interactive)
  ;; First clear tempo cache
  (unless (eq last-command 'metronome-tap-tempo)
    (setq metronome-elapsed-time nil))
  (let ((message-log-max nil)
	(last-time (car metronome-elapsed-time))
	;; Collect elapsed time since cached time
	(time (string-to-number
	       (format "%.02f" (float-time (time-since metronome-cached-time))))))
    (setq metronome-elapsed-time
	  (list time (or last-time (truncate time))))
    ;; Find the difference between TIME and LAST-TIME
    (let* ((secs (metronome-find-difference metronome-elapsed-time))
    	   (bpm (metronome-maybe-round
		 (/ 60 (float (car secs))))))
      (metronome-play-click)
      (setq metronome-tempo bpm)
      (message (format "%d" bpm)))))

;;;###autoload
(defun metronome (arg)
  "Start/pause/resume metronome.
With a prefix ARG, prompt for a new tempo.

There are two ways of inputting the tempo. Either as an
integer (the BPM) or as two integers separated by space, where
the second integer is the number of beats per bar."
  (interactive "P")
  (if (or arg (null metronome-timer))
      (metronome-start 'prompt)
    (if metronome-paused-p
	(metronome-resume)
      (metronome-pause))))

(provide 'metronome)
;;; metronome.el ends here
