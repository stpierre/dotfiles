#!/usr/bin/env python
"""pylint and flake8 runner for emacs flymake.

epylint doesn't accept --rcfile -- WTF?  Ours automagically finds the
correct pylintrc and .flake8.  This also lets me disable and enable
some rules on a global basis, and run flake8.
"""
from __future__ import print_function

import argparse
import os
import subprocess
import sys
import warnings
from xml.etree import ElementTree

from pylint import lint
from pylint.reporters import text

try:
    from flake8.engine import get_style_guide  # noqa
except ImportError:
    from pep8 import StyleGuide as get_style_guide  # noqa

try:
    import simplejson as json  # pylint: disable=import-error
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
        return value

    def find_gitroot(self, file_or_dir):
        """find top directory of the git project that contains ``filename``."""
        if os.path.isdir(file_or_dir):
            dirname = os.path.abspath(file_or_dir)
        else:
            dirname = os.path.dirname(os.path.abspath(file_or_dir))
        cached = self._gitroots.get(dirname)
        if cached:
            return cached
        else:
            try:
                retval = subprocess.check_output(
                    "cd %s && git rev-parse --show-toplevel" % dirname,
                    shell=True).strip()
            except subprocess.CalledProcessError:
                retval = dirname
        self._gitroots[dirname] = retval
        return retval

    def find(self, file_or_dir, names):
        """find a config file whose name is in ``names`` for ``filename``."""
        retval = self._get_cached(file_or_dir, names)
        if retval is not None:
            return retval

        if os.path.isdir(file_or_dir):
            for name in names:
                candidate = os.path.join(file_or_dir, name)
                if os.path.exists(candidate):
                    return self._set_cache(file_or_dir, names, candidate)

        # nothing found
        gitroot = self.find_gitroot(file_or_dir)
        if file_or_dir == gitroot:
            return None
        else:
            retval = self.find(os.path.dirname(file_or_dir), names)
            if retval is not None:
                self._set_cache(file_or_dir, names, retval)
            return retval


def run_pylint(filename, rcfile=None):
    """run pylint check."""
    disable = ["E1103",  # maybe-no-member
               "W0142",  # star-args
               "W1201",  # logging-not-lazy
               "I0011",  # locally-disabled
               "I0012",  # locally-enabled
               "R0801",  # duplicate-code
               "R0901",  # too-many-ancestors
               "R0902",  # too-many-instance-attributes
               "R0903",  # too-few-public-methods
               "R0904",  # too-many-public-methods
               "R0921",  # abstract-class-not-used
               "R0922"]  # abstract-class-little-used
    enable = ["W0511"]  # fixme
    args = [
        "-r", "n", "--persistent=n",
        "-d", ",".join(disable),
        "-e", ",".join(enable)]
    if rcfile:
        args.append("--rcfile=%s" % rcfile)

    kwargs = dict(exit=False)
    with warnings.catch_warnings():
        warnings.simplefilter("error", UserWarning)
        try:
            kwargs['reporter'] = text.ParseableTextReporter(sys.stdout)
            args += ["-f", "parseable", "-i", "y"]
        except UserWarning:
            kwargs['reporter'] = text.TextReporter(sys.stdout)
            args += ["--msg-template",
                     "{path}:{line}: [{msg_id}({symbol}), {obj}] {msg}"]

    lint.Run(args + [filename], **kwargs)


def run_flake8(filename, rcfile=None):
    """run flake8 check."""
    kwargs = dict(format="pylint", ignore=["E501"])
    if rcfile:
        kwargs['config'] = rcfile
    flake8 = get_style_guide(**kwargs)
    flake8.input_file(filename)


def run_coverage(filename, coverage_xml):
    """flag lines without test coverage."""
    if coverage_xml is None:
        return
    cover_data = ElementTree.parse(coverage_xml)
    for cls in cover_data.findall(".//class"):
        if not filename.endswith(cls.get("filename")):
            continue
        for line in cls.findall("lines/line[@hits='0']"):
            print("%s:%s: No test coverage" % (filename, line.get("number")))


def main():
    """run checks."""
    parser = argparse.ArgumentParser()
    parser.add_argument("--original")
    parser.add_argument("filename")
    options = parser.parse_args()

    rcfinder = RCFinder()

    filename = os.path.abspath(options.filename)

    run_pylint(filename,
               rcfinder.find(filename,
                             ["pylintrc", "pylintrc.conf", ".pylintrc"]))
    run_flake8(filename, rcfinder.find(filename, [".flake8"]))
    run_coverage(options.original or filename,
                 rcfinder.find(filename, ["coverage.xml"]))


if __name__ == "__main__":
    sys.exit(main())
