==========
 dotfiles
==========

This is my HOME.  There are many like it, but this one is mine.

Technique
=========

My technique for managing this stuff is based on rtomayko's, which is
outlined at `https://github.com/rtomayko/dotfiles`_.  Basically, my
HOME is itself a git work tree, with repo data in ``~/.git``.  To
prevent ``git status`` from being overwhelmingly noisy, I put ``*` in
``~/.gitignore``.  This is the only difference between my approach and
rtomayko's; with my approach, ``.gitignore`` is itself versioned,
whereas his approach relies on the (unversioned) ``.git/info/exclude``
file.

Similarly, a number of my larger files (``.zshrc``, ``.emacs``) source
other stuff that is not tracked, which is where I can put
site-specific or sensitive things that I do not want to be tracked in
git.

zsh loader
==========

The ``.zshrc`` in particular is fairly complex, and I'm pretty proud
of it, so it deserves special mention.  My ``.zshrc`` just loads, in
alphabetical order, every file in ``~/.zsh.d/S*``.  (Basically, it
imitates init scripts.)  Several of the files in there load their own
``.local`` files, and ``S98_local`` also loads
``~/.zsh.d/zshrc.$(hostname)``, ``~/.zsh.d/zshrc.$(hostname -s)``, and
both of those with any trailing digits stripped off.  This makes it
trivial to customize my zsh environment for different sites or hosts
while still retaining the same core of zsh settings.
