#!/usr/bin/env python
"""pylint and flake8 runner for emacs flymake.

epylint doesn't accept --rcfile -- WTF?  Ours automagically finds the
correct pylintrc and .flake8.  This also lets me disable and enable
some rules on a global basis, and run flake8.
"""
from __future__ import print_function

import argparse
import json
import logging
import os
import subprocess
import sys

try:
    from xml import etree
    etree.ElementTree
    HAS_COVERAGE = True
except (AttributeError, ImportError):
    HAS_COVERAGE = False

try:
    from pylint import lint
    from pylint.reporters import text
    HAS_PYLINT = True
except ImportError:
    HAS_PYLINT = False

try:
    from flake8.engine import get_style_guide  # noqa
    HAS_FLAKE8 = True
except ImportError:
    try:
        from pep8 import StyleGuide as get_style_guide  # noqa
        HAS_FLAKE8 = True
    except ImportError:
        HAS_FLAKE8 = False

try:
    import mypy.api
    HAS_MYPY = True
except ImportError:
    HAS_MYPY = False

SCRIPT_NAME = os.path.basename(__file__)
LOG = logging.getLogger(SCRIPT_NAME if __name__ == "__main__" else __name__)
DEBUG = True


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

        retval = self.find(os.path.dirname(file_or_dir), names)
        if retval is not None:
            self._set_cache(file_or_dir, names, retval)
        return retval


def run_pylint(filename, rcfile=None):
    """run pylint check."""
    args = [
        "-r",
        "n",
        "--persistent=n",
    ]
    if rcfile:
        args.append("--rcfile=%s" % rcfile)
    else:
        disable = [
            "I0011",  # locally-disabled
            "I0012",  # locally-enabled
            "R0801",  # duplicate-code
        ]
        enable = ["W0511"]  # fixme
        args.extend([
            '-d',
            ",".join(disable),
            "-e",
            ",".join(enable),
        ])

    kwargs = {}
    try:
        kwargs['reporter'] = text.TextReporter(sys.stdout)
        kwargs['reporter'].line_format  # pylint: disable=pointless-statement
        args += [
            "--msg-template",
            "{path}:{line}: [{msg_id}({symbol}), {obj}] {msg}"
        ]
    except AttributeError:
        kwargs['reporter'] = text.ParseableTextReporter(sys.stdout)
        args += ["-f", "parseable", "-i", "y"]

    LOG.info("Running pylint: %s %s", args, filename)

    lint.Run(args + [filename], **kwargs)


def run_flake8(filename, rcfile=None):
    """run flake8 check."""
    kwargs = {'format': "pylint"}
    if rcfile:
        kwargs['config'] = rcfile
    flake8 = get_style_guide(**kwargs)
    LOG.info("Running flake8 with style guide: %s", kwargs)
    flake8.input_file(filename)


def run_mypy(filename, rcfile=None):
    """run mypy check."""
    args = []
    if rcfile:
        args.extend(['--config-file', rcfile])
    LOG.info("Running mypy: %s %s", args, filename)

    normal_report, error_report, status = mypy.api.run(args)
    if status != 0:
        LOG.error('Mypy encountered errors (exit code: {status})')
    if error_report:
        LOG.warning(error_report)
    print(normal_report)


def run_coverage(filename, coverage_xml):
    """flag lines without test coverage."""
    if coverage_xml is None:
        return
    cover_data = etree.ElementTree.parse(coverage_xml)
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

    if DEBUG:
        handler = logging.FileHandler(
            os.path.join("/tmp", "%s.%s" % (SCRIPT_NAME, os.getpid())))
        handler.setFormatter(logging.Formatter("%(levelname)s %(message)s"))
        LOG.addHandler(handler)
        LOG.setLevel(logging.DEBUG)

    LOG.debug("Invocation: %s", sys.argv)
    LOG.debug("Working directory: %s", os.getcwd())
    LOG.debug("Environment: %s", os.environ)

    rcfinder = RCFinder()

    filename = os.path.abspath(options.filename)

    if HAS_PYLINT:
        run_pylint(
            filename,
            rcfinder.find(filename,
                          ["pylintrc", "pylintrc.conf", ".pylintrc"]))

    if HAS_FLAKE8:
        run_flake8(filename, rcfinder.find(filename, [".flake8"]))

    if HAS_COVERAGE:
        run_coverage(options.original or filename,
                     rcfinder.find(filename, ["coverage.xml"]))

    if HAS_MYPY:
        run_mypy(filename, rcfinder.find(filename, ["mypy.ini"]))


if __name__ == "__main__":
    sys.exit(main())
