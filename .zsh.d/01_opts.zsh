umask 077

# change behavior of cd
setopt auto_cd
setopt auto_pushd
setopt pushd_silent
setopt pushd_to_home

# change behavior of history. HISTFILE is set in ~/.zshrc, and HISTSIZE
# and SAVEHIST come from oh-my-zsh
setopt share_history
setopt extended_history
setopt hist_ignore_dups
setopt hist_expire_dups_first
setopt hist_allow_clobber
setopt hist_reduce_blanks

# other misc. options
setopt noclobber
setopt extended_glob
# zsh's default WORDCHARS minus /._- (oh-my-zsh empties it)
WORDCHARS='*?[]~=&;!#$%^(){}<>'

# non-zsh-specific options
export CLICOLOR=1

# undo oh-my-zsh key bindings that clobber emacs-style ones
bindkey '^[w' copy-region-as-kill
bindkey '^[l' down-case-word
bindkey '^[[A' up-line-or-history
bindkey '^[[B' down-line-or-history
bindkey '^[OA' up-line-or-search
bindkey '^[OB' down-line-or-search
