#!/bin/bash

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

export PATH=$PATH:~/sw/$(uname -s)-$(uname -r | cut -d. -f 1,2)-$(uname -m)

if type zsh >& /dev/null; then
    exec zsh
fi
