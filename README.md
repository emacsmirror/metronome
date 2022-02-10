[![MELPA](https://melpa.org/packages/metronome-badge.svg)](https://melpa.org/#/metronome)

# metronome.el

A simple metronome for GNU Emacs.

## Installation

This package is available in the MELPA repository and can be installed
from the Emacs package manager. To install it from source, add
metronome.el to your load path and require it:

```
(add-to-list 'load-path "/path/to/metronome.el")
(require 'metronome)
(global-set-key (kbd "C-c C-m") 'metronome)
```

## Usage

```
M-x metronome RET 120 4 RET
                      ^
                      optional
```

then `M-x metronome` to pause/resume, and `C-u M-x metronome` to
set/play a new tempo. When prompted, enter the BPM and optional beats
per bar preceded by space. You can also set a new tempo by tapping two
or more times successively with the `metronome-tap-tempo` command, or
with the `metronome-(in/de)crement-tempo` commands.

For a visual reference of the tempo, beat and (optional) bar count,
use the `metronome-display` command. Press `SPC` to play/pause, `n/p`
to change tempo, `h/s` to tap/set a new tempo, and `q` to quit.

See the [wiki](https://gitlab.com/jagrg/metronome/-/wikis/pages) for
more information.

## A note on accuracy

The time intervals are good enough for general practising, but perfect
timing is difficult on a multitasking system, so expect some limping
when the system is loaded. A better (but more advanced) option is to
use the JACK sound server daemon, (see the `jack_metro` command, for
example), or the
[cl-collider](https://github.com/byulparan/cl-collider) library to
connect to the SuperCollider server. I would also suggest using the
[cl-patterns](https://github.com/defaultxr/cl-patterns) library to
create even more sophisticated patterns.

