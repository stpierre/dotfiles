#!/bin/bash -xe

clone=$(mktemp -d)
git clone https://github.com/stpierre/dotfiles.git $clone
rsync -av $clone/ $HOME/
rm -rf $clone
cd $HOME
git status
