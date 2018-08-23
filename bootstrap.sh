#!/bin/bash

################################
# Author:   David Jung
# Email:    sungwonida@gmail.com
################################

# Check whether cask has been installed or not
cask=`which cask`
if [ -z $cask ]; then
    # If not found, install it
    echo "Installing Cask"
    curl -fsSkL https://raw.github.com/cask/cask/master/go | python

    # Add lines to export PATH if needed
    env_path=`echo $PATH`
    if [[ ! $env_path =~ .*\.cask\/bin.* ]]; then
        echo "" >> ~/.bashrc
        echo "# Cask" >> ~/.bashrc
        echo "export PATH=\"$HOME/.cask/bin:\$PATH\"" >> ~/.bashrc
    fi

    # Cask bootstrapping is done
fi

# Install the packages and their dependancies
echo "Installing packages & dependancies"
cask install

# Install the font if needed
kernel=`uname -s`
if [ "$kernel" == "Linux" ]; then
    monaco=`fc-list -f '%{file}\n' | grep -i "monaco"`
    if [ -z $monaco ]; then
        ./local/install_fonts_linux.sh ./local/fonts/linux
    fi
fi
