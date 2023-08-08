#!/bin/sh

# mac emacs edit binary applescript
if [ "$1" = "-d" ]; then
    RANDOMFILE="applescript${RANDOM}.scpt"
    cat /dev/stdin >"/tmp/$RANDOMFILE"
    osadecompile "/tmp/$RANDOMFILE"
    rm "/tmp/$RANDOMFILE"
else
    RANDOMFILE="applescript${RANDOM}.scpt"
    osacompile -o "/tmp/$RANDOMFILE"
    cat "/tmp/$RANDOMFILE"
    rm "/tmp/$RANDOMFILE"
fi
