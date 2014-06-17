#!/bin/zsh

usage() {
    echo "Usage: $(basename $0) [-c <commit | change-id>] [-r <remote>] <user> [<user> ...]"
    exit 1
}

cid=
remote=origin
while getopts ":c:r:h" opt; do
    case $opt in
        c)
            cid=$OPTARG
            ;;
        r)
            remote=$OPTARG
            ;;
        h)
            usage
            ;;
        \?)
            echo "Invalid option: -$OPTARG"
            usage
            ;;
    esac
done

shift $((OPTIND - 1))

if [[ -z $1 ]]; then
    usage
fi

if [[ -z $cid ]]; then
    cid=$(git log --pretty=format:%b HEAD~1..HEAD | \
        sed -n 's/[[:space:]]*Change-Id:[[:space:]]*//p')
    if [[ -z $cid ]]; then
        echo "Unable to determine Change-Id"
        usage
    fi
fi

users="$@"

set -- $(git remote -v | \
    sed -n 's#^'"$remote"'[[:space:]]*ssh://\([^:/]*\):\([0-9]*\).*push)#\1 \2#p')
server=$1
port=$2

args=
for user in ${=users}; do
    args="$args --add $user"
done
ssh -p $port $server gerrit set-reviewers $args $cid
