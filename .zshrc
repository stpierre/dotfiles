#! /bin/zsh

autoload -U compinit

zsh_cache=${HOME}/.zsh.d/cache
mkdir -p $zsh_cache

compinit -d $zsh_cache/zcomp-$HOST

for zshrc_snipplet in ~/.zsh.d/S[0-9][0-9]*; do
    source $zshrc_snipplet
done
