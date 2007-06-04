"""
zfec -- fast forward error correction library with Python interface

maintainer web site: U{http://allmydata.com/source/zfec}

zfec web site: U{http://allmydata.com/source/zfec}
"""

__version__ = "unknown"
try:
    from _version import __version__
except ImportError:
    # we're running in a tree that hasn't run make-version.py, so we don't
    # know what our version is. This should not happen very often.
    pass

from _fec import Encoder, Decoder, Error
import filefec, cmdline_zfec, cmdline_zunfec

# zfec -- fast forward error correction library with Python interface
# 
# Copyright (C) 2007 Allmydata, Inc.
# Author: Zooko Wilcox-O'Hearn
# mailto:zooko@zooko.com
# 
# This file is part of zfec.
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your option)
# any later version, with the added permission that, if you become obligated
# to release a derived work under this licence (as per section 2.b), you may
# delay the fulfillment of this obligation for up to 12 months.  See the
# COPYING file for details.
