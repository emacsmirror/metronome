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

See the [wiki](https://gitlab.com/jagrg/metronome/-/wikis/pages) for
more information.

## Visual reference and other features

I'm testing new features
[here](https://gitlab.com/jagrg/metronome/-/tree/display). The branch
includes a `metronome-display` command, which in addition to the
audible clicks also displays a visual reference of the tempo, beat
count, and optional bar count. Press `SPC` to play/pause, `n/p` to
increment/decrement the tempo, `s` to set a new tempo, and `q` to
quit.
