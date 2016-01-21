"""
zfec -- fast forward error correction library with Python interface

maintainer web site: U{http://tahoe-lafs.org/source/zfec}

zfec web site: U{http://tahoe-lafs.org/source/zfec}
"""

__version__ = "unknown"
try:
    from _version import __version__
except ImportError:
    # We're running in a tree that hasn't run darcsver, and didn't come with a
    # _version.py, so we don't know what our version is. This should not happen
    # very often.
    pass

from _fec import Encoder, Decoder, Error
import easyfec, filefec, cmdline_zfec, cmdline_zunfec

quiet_pyflakes=[__version__, Error, Encoder, Decoder, cmdline_zunfec, filefec, cmdline_zfec, easyfec]

# zfec -- fast forward error correction library with Python interface
#
# Copyright (C) 2007-2010 Allmydata, Inc.
# Author: Zooko Wilcox-O'Hearn
# mailto:zooko@zooko.com
#
# This file is part of zfec.
#
# See README.rst for licensing information.
