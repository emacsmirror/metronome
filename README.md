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
more times successively with the `metronome-tap-tempo` command.

## emacsclient

You can use `emacsclient` to control the metronome outside Emacs with
some keyboard shortcut. Here's an example using `gnome-terminal`:

```shell
#!/bin/bash

tempo=0
prompt() {
    read -p "$1"": " value
    tempo=$value
}
prompt "Tempo"

bpm=$(echo $tempo | cut -d ' ' -f 1)

if echo $tempo | grep -E '[ "]' >/dev/null ; then
    bpb=$(echo $tempo | cut -d ' ' -f 2)
    emacsclient -e -a "" "(metronome-start '($bpm $bpb))"
else
    emacsclient -e -a "" "(metronome-start $bpm)"
fi
```

```shell
#!/bin/bash

timer=$(emacsclient -e "metronome-timer");

if [ $# -eq 1 ] || [ "$timer" == nil ] ; then
    gnome-terminal --geometry=40x3+580+200 \
		   --hide-menubar \
		   --title="Metronome" \
		   --wait \
		   --profile="metronome" \
		   -- metronome-prompt
else
    emacsclient -e "(call-interactively 'metronome)"
fi
```
