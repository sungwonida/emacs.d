#!/usr/bin/env bash

export GDK_SCALE=2
export GDK_DPI_SCALE=0.75
export XCURSOR_SIZE=32
exec ~/.local/emacs/bin/emacs-30.1 "$@"
