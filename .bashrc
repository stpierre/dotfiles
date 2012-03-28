#!/bin/bash

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

if [[ -e /bin/zsh ]]; then
    exec /bin/zsh
fi
