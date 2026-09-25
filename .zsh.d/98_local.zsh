# include host-specific files: zshrc.<host> for the long hostname, the
# short hostname, and each of those with trailing digits removed
() {
    setopt local_options extended_glob
    local -aU hostnames=(
        $HOST
        ${HOST%%.*}
        ${HOST%%[0-9]#}
        ${${HOST%%.*}%%[0-9]#}
    )
    local host
    for host in $hostnames; do
        if [[ -e ~/.zsh.d/zshrc.$host ]]; then
            source ~/.zsh.d/zshrc."$host"
        fi
    done
}

if [[ -e ~/.zsh.d/aliases.local ]]; then
    source ~/.zsh.d/aliases.local
fi
