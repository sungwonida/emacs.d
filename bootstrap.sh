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

# Investigate whether ~/.emacs.d/.cask/[VERSION]/bootstrap is
# already been existing or not
emacs_version=`emacs --version | head -n1 | cut -d' ' -f3`
elpa="$HOME/.emacs.d/.cask/$emacs_version/elpa"
if [ ! -d "$elpa" ]; then
    # If not found, install the packages specified in Cask
    # and their dependancies
    echo "Installing packages & dependancies"
    cask install
fi
