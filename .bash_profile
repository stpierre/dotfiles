# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

if type zsh >& /dev/null; then
    exec zsh $*
fi
