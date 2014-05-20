#!/bin/bash -xe

dest=${DOTFILES_HOME:-$HOME}

clone=$(mktemp -d)
git clone https://github.com/stpierre/dotfiles.git $clone
rsync -av $clone/ $dest/
rm -rf $clone
cd $dest
git status
