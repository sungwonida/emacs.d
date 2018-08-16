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
    echo "" >> ~/.bashrc
    echo "# Cask" >> ~/.bashrc
    echo "export PATH=\"$HOME/.cask/bin:\$PATH\"" >> ~/.bashrc
fi
# Cask bootstrapping is done

# Install the packages and their dependancies
echo "Installing packages & dependancies"
cask install

# Install the font if needed
kernel=`uname -s`
if [ "$kernel" == "Linux" ]; then
    monaco=`fc-list -f '%{file}\n' | grep -i "monaco"`
    if [ -z $monaco ]; then
        ./local/install_fonts_linux.sh ./local/fonts
    fi
fi
