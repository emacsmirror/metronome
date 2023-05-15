[![MELPA](https://melpa.org/packages/metronome-badge.svg)](https://melpa.org/#/metronome)

# metronome.el

The missing metronome for GNU Emacs.


## Installation

This package is available in the MELPA repository and can be installed
from the Emacs package manager. To install it from source, add
`metronome.el` to your load path and require it:

```
(add-to-list 'load-path "/path/to/metronome.el")
(require 'metronome)
(global-set-key (kbd "C-c m") 'metronome)
```


## Usage

```
M-x metronome RET 120 RET
```

then `M-x metronome` to pause/resume, and `C-u M-x metronome` to set a
new tempo. See the
[wiki](https://gitlab.com/jagrg/metronome/-/wikis/pages) for more
information.


## Flashing the modeline on every beat

```lisp
(defun metronome-flash-mode-line ()
  (let ((visible-bell nil)
        (ring-bell-function
         (lambda ()
           (invert-face 'mode-line)
           (run-with-timer 0.1 nil #'invert-face 'mode-line))))
    (beep)))

(advice-add 'metronome-play-click :before #'metronome-flash-mode-line)
```


## Interacting with the metronome outside Emacs

```bash
#!/usr/bin/env bash

timer=$(emacsclient -e "metronome-timer");

if [ $# -eq 1 ] || [ "$timer" == nil ]
then
    tempo=$(zenity --entry --title "Metronome" --text "Tempo: ")
    emacsclient -e -a "" "(metronome-start $tempo)"
else
    emacsclient -e "(call-interactively #'metronome)"
fi
```


## A note on accuracy

The time intervals are good enough as a general reference, but perfect
timing is difficult on a multitasking system, so expect some limping
when the system is loaded. A better (but more advanced) option is to
use the JACK sound server daemon, or the
[cl-collider](https://github.com/byulparan/cl-collider) library to
connect to the SuperCollider server.

