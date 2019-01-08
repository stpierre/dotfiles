# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

env > /tmp/bash.$$

if [[ -n $TERMINATOR_UUID ]] && type zsh >& /dev/null; then
    exec zsh $*
fi
