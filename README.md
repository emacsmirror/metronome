[![MELPA](https://melpa.org/packages/metronome-badge.svg)](https://melpa.org/#/metronome)

# metronome

A simple metronome for GNU Emacs.

## Installation

This package is available in the MELPA repository and can be installed
from the Emacs package manager. To install it from source, add
metronome.el to your load path and require it:

```
(add-to-list 'load-path "/path/to/metronome.el")
(require 'metronome)
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

Perfect timing is difficult on a multitasking system, especially when
the system is loaded. If you know how to improve the timing please let
me know.
