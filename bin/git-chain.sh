#!/bin/bash
#
# chain together a bunch of git commands.  e.g.:
#
# git chain 'co master' upstream 'co -' 'merge master'
#
# ...is equivalent to:
#
# git co master && git upstream && git co - && git merge master

for gitcmd in "$@"; do
    cmd="git $gitcmd"
    echo $cmd
    if ! $cmd; then
        rv=$?
        echo "$cmd failed"
        exit $rv
    fi
    echo
done
