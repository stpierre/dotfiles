# Prompt hook framework. The stpierre theme (themes/stpierre.zsh-theme)
# calls _prompt_build from precmd and points PROMPT/RPROMPT at the
# variables it sets. This lives here rather than in the theme because
# oh-my-zsh sources the theme after the other files in ~/.zsh.d, and
# they register hooks.
#
# A hook is a function that sets REPLY to the text it wants shown (or
# leaves it empty to show nothing). Hooks run in the current shell, so
# they must not print anything.

typeset -ga _prompt_hooks _rprompt_hooks
typeset -g _prompt_left _prompt_right
_prompt_colors=(blue cyan green yellow magenta)
_prompt_sep=" %F{blue}|%f "
_prompt_nbsp=$' '

add_prompt_hook() {
    if [[ $1 == "--right" ]]; then
        _rprompt_hooks=(${_rprompt_hooks:#$2} "$2")
    else
        _prompt_hooks=(${_prompt_hooks:#$1} "$1")
    fi
}

# run each hook in the named array and join their output, cycling
# through _prompt_colors. $2 is the index of the first color to use.
_prompt_run_hooks() {
    local hook
    local -i i=$2
    local -a prompt_data
    for hook in ${(P)1}; do
        REPLY=
        "$hook"
        if [[ -n $REPLY ]]; then
            prompt_data+=("%F{${_prompt_colors[i]}}$REPLY%f")
            i=$(( (i % ${#_prompt_colors}) + 1 ))
        fi
    done
    REPLY=${(pj:$_prompt_sep:)prompt_data}
}

_prompt_build() {
    local REPLY
    local cwd="%F{${_prompt_colors[1]}}%~%f"
    local time="%B%F{yellow}%T%f%b"
    local retval="%(?..%F{red}[%?]%f)"

    _prompt_run_hooks _prompt_hooks 2
    # todo: switch to single-line prompt when there's not much data
    _prompt_left="$cwd${REPLY:+$_prompt_sep$REPLY}
$time $retval%#$_prompt_nbsp"

    _prompt_run_hooks _rprompt_hooks 1
    _prompt_right=$REPLY
}

# our prompt ends with a non-breaking space. this makes it clear the
# input buffer, so we can triple-click to copy and paste full
# lines. See #4 at
# http://chneukirchen.org/blog/archive/2013/03/10-fresh-zsh-tricks-you-may-not-know.html
bindkey -s "$_prompt_nbsp" '^u'
