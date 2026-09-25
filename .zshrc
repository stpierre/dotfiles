# oh-my-zsh setup. Most of the config lives in ~/.zsh.d, which is
# $ZSH_CUSTOM: pre/*.zsh are sourced here before oh-my-zsh, and
# oh-my-zsh sources *.zsh itself, after its libs and plugins and before
# the theme. Run ~/bin/zsh-bootstrap to install oh-my-zsh.

ZSH=$HOME/.oh-my-zsh
ZSH_CUSTOM=$HOME/.zsh.d
ZSH_COMPDUMP=$ZSH_CUSTOM/cache/zcompdump-$HOST
ZSH_THEME=stpierre
DISABLE_AUTO_TITLE=true
HISTFILE=$HOME/.history

zstyle ':omz:update' mode auto
# gnubin puts GNU ls first, where -G means --no-group, not color
zstyle ':omz:lib:theme-and-appearance' gnu-ls yes

# zsh-syntax-highlighting must be last
plugins=(gh kubectl mise docker golang gcloud zsh-syntax-highlighting)

mkdir -p "$ZSH_CUSTOM"/cache

for zshrc_pre in "$ZSH_CUSTOM"/pre/*.zsh(N); do
    source "$zshrc_pre"
done
unset zshrc_pre

if [[ -r $ZSH/oh-my-zsh.sh ]]; then
    source "$ZSH"/oh-my-zsh.sh
else
    print -P "%F{red}oh-my-zsh is not installed; run ~/bin/zsh-bootstrap%f"
    autoload -Uz compinit add-zsh-hook
    compinit -d "$ZSH_COMPDUMP"
    for zshrc_snippet in "$ZSH_CUSTOM"/*.zsh(N) \
                         "$ZSH_CUSTOM"/themes/"$ZSH_THEME".zsh-theme(N); do
        source "$zshrc_snippet"
    done
    unset zshrc_snippet
fi
