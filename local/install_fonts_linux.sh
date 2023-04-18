#!/bin/bash

if [ -z $1 ]; then
    echo "Font path must be given e.g. sh install_fonts_linux.sh fonts/linux/monaco"
    exit
fi

fonts=$1
mkdir -p ~/.fonts
cp -r $fonts/* ~/.fonts/
fc-cache -fv
