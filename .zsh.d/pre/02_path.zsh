# Sourced by ~/.zshrc before oh-my-zsh, so that plugins can find their
# commands and compinit can see every completion directory.

typeset -U path fpath

# prepend (or append, with "after") a directory to PATH if it is not
# already there. kept for zshrc.<host> and path.local files.
pathmunge() {
    if [[ ${2:-} == "after" ]]; then
        path+=("$1")
    elif (( ! $path[(Ie)$1] )); then
        path=("$1" $path)
    fi
}

pathremove() {
    path=("${(@)path:#$1}")
}

() {
    local brew=/opt/homebrew
    local -a gnubins
    local dir
    local -A seen

    if [[ -d $brew ]]; then
        # gsed and gnu-sed (etc.) are the same keg, so dedupe on the
        # resolved path
        for dir in $brew/opt/*/libexec/gnubin(N/); do
            if [[ -z ${seen[${dir:A}]} ]]; then
                seen[${dir:A}]=1
                gnubins+=("$dir")
            fi
        done
        path=($gnubins $brew/bin $brew/sbin $path)
        fpath=($brew/share/zsh/site-functions(N/) $fpath)
    fi

    if [[ -d $HOME/go ]]; then
        export GOPATH=$HOME/go
    fi

    path=(
        $HOME/bin(N/)
        $HOME/.local/bin(N/)
        $path
        ${GOPATH:+$GOPATH/bin}
        ${KREW_ROOT:-$HOME/.krew}/bin(N/)
        $HOME/.bun/bin(N/)
        $HOME/.docker/bin(N/)
    )
}

if [[ -d $HOME/.bun ]]; then
    export BUN_INSTALL=$HOME/.bun
fi

if [[ -e $HOME/.zsh.d/path.local ]]; then
    source "$HOME"/.zsh.d/path.local
fi

export PATH
