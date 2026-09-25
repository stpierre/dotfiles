# tab-complete hosts in .ssh/known_hosts
if [[ -e $HOME/.ssh/known_hosts ]]; then
    () {
        local -a myhosts
        myhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9|]*}%%\ *}%%,*} )
        zstyle ':completion:*' hosts $myhosts
    }
fi

alias tf=terraform
