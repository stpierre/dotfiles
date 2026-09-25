# once upon a time we had logic that would allow us to run multiple
# emacs servers. that's unused, but since we had the logic below, it
# makes sense to preserve that logic just in case we need it again
_emacs_server_name=server

__start_emacs_server() {
    local name=$_emacs_server_name
    local pid
    if [[ $__is_macos == true ]]; then
        pgrep -U "$USER" -i emacs
    else
        pgrep -U "$USER" -f "$__emacs_bin"
    fi | while read -r pid; do
        if lsof -F n -a -U -p "$pid" | grep -q "^n.*/$name"; then
            # already running
            return
        fi
    done
    "$__emacs_bin" --daemon="$name"
}

if (( $+commands[emacs] )); then
    # $commands ignores functions, so this finds the real binary even
    # after the emacs() function below has been defined
    __emacs_bin=$commands[emacs]
    if [[ -z $__emacs_version ]]; then
        __emacs_version=$("$__emacs_bin" --version | head -n 1 | \
            awk '{ split($NF, v, "."); print v[1] "." v[2]; }')
    fi

    if (( $+commands[emacsclient] && __emacs_version >= 23.1 )); then
        emacs() {
            __start_emacs_server
            local -a args=(-s "$_emacs_server_name")
            local gui_frames
            # check to see if any of -nw, -t, or --tty were provided
            # as arguments. if they were, we respect them; if not, we
            # figure out whether to open a new frame (and how)
            if [[ ${argv[(Ie)-nw]} == 0 && ${argv[(Ie)-t]} == 0 && \
                      ${argv[(Ie)--tty]} == 0 ]]; then
                if [[ $__is_macos == true ]]; then
                    args+=(-n)
                    # on Mac OS, the frame behavior is broken and it
                    # won't reuse a frame if `-r` is given, but it
                    # also won't open a new frame *unless* `-c` is
                    # given. see
                    # https://emacs.stackexchange.com/questions/79292/why-is-emacsclient-not-reusing-the-existing-frame
                    gui_frames=$(emacsclient -e -s "$_emacs_server_name" \
                                             '(visible-frame-list)' | \
                                     sed 's/^(#<frame //; s/)$//; s/ 0x[0-9a-f]*>//g; s/ #<frame /\n/;' | \
                                     grep -v '^F[0-9]*$')
                    if [[ -z $gui_frames ]]; then
                        args+=(-c)
                    fi
                elif [[ -n $DISPLAY ]]; then
                    # on linux, if we have a GUI, `-c` Just Works
                    # (although it seems like maybe we should use `-r`
                    # here instead?)
                    args+=(-n -c)
                else
                    # no GUI, run in the terminal. we have to omit the
                    # `-n` flag, which causes emacsclient to exit
                    # immediately; that's great for GUI emacs, but not
                    # so much for the terminal.
                    args+=(-t)
                fi
            fi
            emacsclient "${args[@]}" "$@"
        }

        emacs-kill() {
            emacsclient -e -s "$_emacs_server_name" '(kill-emacs)'
        }

        export ALTERNATIVE_EDITOR=""
    fi

    export EDITOR="emacs -nw"
    alias emcas=emacs
    alias vi=$EDITOR
elif (( $+commands[vim] )); then
    export EDITOR=vim
    alias emcas=$EDITOR
else
    export EDITOR=vi
    alias emcas=$EDITOR
fi
