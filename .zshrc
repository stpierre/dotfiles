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

# bun completions
[ -s "/Users/stpierre/.bun/_bun" ] && source "/Users/stpierre/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

alias claude-mem='/Users/stpierre/.bun/bin/bun "/Users/stpierre/.claude/plugins/cache/thedotmack/claude-mem/10.5.2/scripts/worker-service.cjs"'
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
export PATH="/opt/homebrew/sbin:$PATH"
