#! /bin/zsh

autoload -U compinit

zsh_cache=${HOME}/.zsh.d/cache
mkdir -p $zsh_cache

compinit -d $zsh_cache/zcomp-$HOST

for zshrc_snipplet in ~/.zsh.d/S[0-9][0-9]*; do
    if type _zshrc_log >& /dev/null; then
        _zshrc_log "Loading snipplet $zshrc_snipplet"
    fi
    source "$zshrc_snipplet"
done
