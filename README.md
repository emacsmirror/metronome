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
M-x metronome RET 120 RET
```

then `M-x metronome` to pause/resume, and `C-u M-x metronome` to set a
new tempo. See the
[wiki](https://gitlab.com/jagrg/metronome/-/wikis/pages) for more
information.

## A note on accuracy

The time intervals are good enough for general practising, but perfect
timing is difficult on a multitasking system, so expect some limping
when the system is loaded. A better (but more advanced) option is to
use the JACK sound server daemon (see the `jack_metro` command, for
example), or the
[cl-collider](https://github.com/byulparan/cl-collider) library to
connect to the SuperCollider server (see also the
[cl-patterns](https://github.com/defaultxr/cl-patterns) library).

