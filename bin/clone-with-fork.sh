#!/bin/zsh

upstream=$(echo $1 | sed -n 's#.*github.com/\([^/]*\)/.*#\1#p')
name=$(echo $1 | sed -n 's#.*/\(.*\)\.git#\1#p')
dest=${2:-$name}

git clone https://github.com/$upstream/$name.git $dest
cd $dest
git remote rm origin
git remote add origin https://github.com/stpierre/$name.git
git remote add upstream https://github.com/$upstream/$name.git
git fetch --all
