# automatically reload the shell (with `omz reload`) when the zsh config
# changes. This is a precmd hook, not preexec, because `omz reload` execs
# a new shell, which would throw away the command about to run.

zmodload zsh/datetime
zmodload -F zsh/stat b:zstat

typeset -g _autoreload_started=$EPOCHSECONDS
typeset -g _autoreload_last_check=$EPOCHSECONDS

_autoreload() {
    local -i interval=${AUTORELOAD_INTERVAL:-3600}
    local -a files mtimes
    local newest

    # only check once an hour at most
    if (( EPOCHSECONDS - _autoreload_last_check < interval )); then
        return
    fi
    _autoreload_last_check=$EPOCHSECONDS

    files=(
        ~/.zshrc
        ~/.zsh.d/{,pre/}*.zsh(N)
        ~/.zsh.d/themes/*.zsh-theme(N)
        ~/.zsh.d/{zshrc.*,path.local,aliases.local}(N)
    )
    zstat -A mtimes +mtime -- $files 2>/dev/null
    newest=${${(On)mtimes}[1]}

    if (( newest > _autoreload_started )); then
        if (( ${#jobstates} )); then
            print -P "%F{yellow}zsh config changed; not reloading with" \
                  "background jobs running (run 'omz reload' manually)%f"
        else
            print -P "%F{yellow}zsh config changed; reloading%f"
            omz reload
        fi
    fi
}

add-zsh-hook precmd _autoreload
