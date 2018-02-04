"""
zfec -- fast forward error correction library with Python interface

maintainer web site: U{http://tahoe-lafs.org/source/zfec}

zfec web site: U{http://tahoe-lafs.org/source/zfec}
"""

from ._version import get_versions
__version__ = get_versions()['version']
del get_versions

from ._fec import Encoder, Decoder, Error
from . import easyfec, filefec, cmdline_zfec, cmdline_zunfec

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
