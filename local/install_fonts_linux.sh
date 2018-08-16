#!/bin/bash

fonts=$1
mkdir ~/.fonts
cp -r $fonts/* ~/.fonts/
fc-cache -fv
