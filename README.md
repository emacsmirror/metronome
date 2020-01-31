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

See the [wiki](https://gitlab.com/jagrg/metronome/-/wikis/pages) for more information.
