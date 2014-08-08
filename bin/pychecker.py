#!/usr/bin/env python
"""pylint and flake8 runner for emacs flymake.

epylint doesn't accept --rcfile -- WTF?  Ours automagically finds the
correct pylintrc and .flake8.  This also lets me disable and enable
some rules on a global basis, and run flake8.
"""

import argparse
import os
import subprocess
import sys
import warnings

from pylint import lint
from pylint.reporters.text import TextReporter, ParseableTextReporter

try:
    from flake8.engine import get_style_guide
except ImportError:
    from pep8 import StyleGuide as get_style_guide

try:
    import simplejson as json
except ImportError:
    import json


class RCFinder(object):
    """config file finder

    finds a config file with any of a set of given names within the
    git project that contains a filename.  caches results in
    ~/.pychecker.
    """
    def __init__(self, cachefile="~/.pychecker"):
        self._cachefile = os.path.expanduser(cachefile)
        if os.path.exists(self._cachefile):
            self._cache = json.load(open(self._cachefile))
        else:
            self._cache = dict()
        self._gitroots = dict()

    def _get_cached(self, gitroot, names):
        """get a cached config file."""
        cachekey = "-".join([gitroot] + names)
        retval = self._cache.get(cachekey)
        if retval is not None and not os.path.exists(retval):
            self._set_cache(gitroot, names, None)
            retval = None
        return retval

    def _set_cache(self, gitroot, names, value):
        """set the cache for a set of config file names within a git root."""
        cachekey = "-".join([gitroot] + names)
        self._cache[cachekey] = value
        json.dump(self._cache, open(self._cachefile, "w"))

    def _find_gitroot(self, filename):
        """find top directory of the git project that contains ``filename``."""
        return self._gitroots.get(
            filename,
            subprocess.check_output(
                "cd %s && git rev-parse --show-toplevel" %
                os.path.dirname(os.path.abspath(filename)),
                shell=True).strip())

    def find(self, filename, names):
        """find a config file whose name is in ``names`` for ``filename``."""
        gitroot = self._find_gitroot(filename)
        if gitroot == os.path.expanduser("~"):
            return None
        retval = self._get_cached(gitroot, names)
        if retval is not None:
            return retval
        else:
            for root, _, files in os.walk(gitroot):
                matches = set(files) & set(names)
                if matches:
                    retval = os.path.join(root, matches.pop())
                    break
        self._set_cache(gitroot, names, retval)
        return retval


def run_pylint(filename, rcfile=None):
    """run pylint check."""
    args = [
        "-r", "n", "--persistent=n",
        "-d", "W0142,W1201,I0011,R0801,R0901,R0902,R0903,R0904,R0921,R0922",
        "-e", "W0511"]
    if rcfile:
        args.append("--rcfile=%s" % rcfile)

    kwargs = dict(exit=False)
    with warnings.catch_warnings():
        warnings.simplefilter("error", UserWarning)
        try:
            kwargs['reporter'] = ParseableTextReporter(sys.stdout)
            args += ["-f", "parseable", "-i", "y"]
        except UserWarning:
            kwargs['reporter'] = TextReporter(sys.stdout)
            args += ["--msg-template",
                     "{path}:{line}: [{symbol}, {obj}] {msg}"]

    lint.Run(args + [filename], **kwargs)


def run_flake8(filename, rcfile=None):
    """run flake8 check."""
    kwargs = dict(format="pylint", ignore=["E501"])
    if rcfile:
        kwargs['config'] = rcfile
    flake8 = get_style_guide(**kwargs)
    flake8.input_file(filename)


def main():
    """run checks."""
    parser = argparse.ArgumentParser()
    parser.add_argument("filename")
    options = parser.parse_args()

    rcfinder = RCFinder()

    run_pylint(options.filename,
               rcfinder.find(options.filename,
                             ["pylintrc", "pylintrc.conf", ".pylintrc"]))
    run_flake8(options.filename, rcfinder.find(options.filename, [".flake8"]))


if __name__ == "__main__":
    sys.exit(main())
