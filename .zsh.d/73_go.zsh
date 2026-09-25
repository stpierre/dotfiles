# GOPATH and $GOPATH/bin are set up in pre/02_path.zsh

typeset -g _go_version=

# find the go directive in the nearest go.mod, stopping at the root of a
# git repo
_get_go_version() {
    local dir=$PWD
    local -a directive
    _go_version=
    while true; do
        if [[ -e $dir/go.mod ]]; then
            directive=(${(M)${(f)"$(<$dir/go.mod)"}:#go [0-9]*})
            _go_version=${${directive[1]#go }%%[[:space:]]*}
            return
        elif [[ -e $dir/.git || $dir == / ]]; then
            return
        fi
        dir=${dir:h}
    done
}

_get_go_prompt_info() {
    if [[ -n $_go_version ]]; then
        REPLY="go$_go_version"
    fi
}

if (( $+commands[go] )); then
    add_prompt_hook _get_go_prompt_info
    add-zsh-hook chpwd _get_go_version
    _get_go_version
fi
