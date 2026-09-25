(( $+commands[git] )) || return

typeset -g _current_git_repo _current_git_branch _current_git_unpushed
typeset -g _git_vars_stale=

# repo name is owner/name from the origin remote, falling back to the
# basename of the working tree
_parse_git_repo() {
    local toplevel url owner
    _current_git_repo=
    _current_git_branch=
    _current_git_unpushed=

    toplevel=$(git rev-parse --show-toplevel 2>/dev/null) || return
    url=$(git remote get-url origin 2>/dev/null)
    if [[ -n $url ]]; then
        url=${${url%.git}%/}
        # handles both host:owner/name and scheme://host/owner/name
        owner=${${url%/*}##*[:/]}
        _current_git_repo=$owner/${url##*/}
    else
        _current_git_repo=${toplevel:t}
    fi

    if [[ $_current_git_repo == (dotfiles|*/dotfiles) ]]; then
        _current_git_repo=
        return
    fi

    _current_git_branch=$(git symbolic-ref --short -q HEAD 2>/dev/null) ||
        _current_git_branch=$(git rev-parse --short HEAD 2>/dev/null)

    local unpushed
    if unpushed=$(git rev-list --count @{u}..HEAD 2>/dev/null) &&
            (( unpushed > 0 )); then
        _current_git_unpushed=$unpushed
    fi
}

# a command that may change git state marks the vars stale in preexec;
# they are refreshed in precmd, after the command has run
_zsh_mark_git_vars_stale() {
    case "$1" in
        *git*|g\ *|gs\ *)
            _git_vars_stale=1
            ;;
    esac
}

_zsh_update_git_vars() {
    if [[ -n $_git_vars_stale ]]; then
        _git_vars_stale=
        _parse_git_repo
    fi
}

_get_git_prompt_info() {
    if [[ -n $_current_git_repo && -n $_current_git_branch ]]; then
        REPLY="$_current_git_repo:$_current_git_branch"
        if [[ -n $_current_git_unpushed ]]; then
            REPLY+=" %B($_current_git_unpushed)%b"
        fi
    fi
}

add_prompt_hook _get_git_prompt_info

# we refresh the git vars on three occasions:
#
# 1.  When this file is first loaded;
# 2.  After a git command is run; and
# 3.  When cd is run
_parse_git_repo
add-zsh-hook preexec _zsh_mark_git_vars_stale
add-zsh-hook precmd _zsh_update_git_vars
add-zsh-hook chpwd _parse_git_repo

if (( $+commands[hub] )); then
    alias git=hub
fi

alias g=git
alias gg='git grep'

if (( $+commands[git-spice] )); then
    alias gs='git spice'
fi
