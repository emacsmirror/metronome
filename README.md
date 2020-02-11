# metronome

A simple metronome for GNU Emacs.

## Installation

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
set/play a new tempo. You can also set a new tempo by tapping two or
more times successively with the `metronome-tap-tempo` command, or
with the `metronome-(in/de)crement-tempo` commands.

## Visual reference

The `metronome-display` command displays a visual reference of the
tempo, beat and (optional) bar count. Press `SPC` to play/pause, `n/p`
to change tempo, `h/s` to tap/set a new tempo, and `q` to quit.

See the [wiki](https://gitlab.com/jagrg/metronome/-/wikis/pages) for
more information.

## A note on accuracy

Perfect timing is difficult on a multitasking system, especially when
the system is loaded. If you know how to improve the timing please let
me know.
