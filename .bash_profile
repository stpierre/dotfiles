# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

export PATH=$PATH:~/sw/$(uname -s)-$(uname -r | cut -d. -f 1,2)-$(uname -m)/bin

if type zsh >& /dev/null; then
    exec zsh
fi
