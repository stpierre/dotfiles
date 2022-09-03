#!/bin/bash

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

complete -C /home/stpierre/bin/vault vault

complete -C /home/stpierre/bin/packer packer

#if type zsh >& /dev/null; then
#    exec zsh $*
#fi

