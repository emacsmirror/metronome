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
set/play a new tempo.

## emacsclient

You can also use `emacsclient` to control the metronome outside Emacs
with some keyboard shortcut. Here's an example using `gnome-terminal`:

```shell
#!/bin/sh

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
#!/bin/sh

if [ $# -eq 1 ] || [ $(emacsclient -e "metronome-timer") = nil ] ; then
    gnome-terminal --geometry=40x3+580+200 \
		   --hide-menubar \
		   --title="Metronome" \
		   --wait \
		   --profile="met" \
		   -- metronome-prompt
else
    emacsclient -e "(call-interactively 'metronome)"
fi
```
