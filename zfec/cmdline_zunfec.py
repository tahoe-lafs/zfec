#!/usr/bin/env python

# zfec -- a fast C implementation of Reed-Solomon erasure coding with
# command-line, C, and Python interfaces

from __future__ import print_function

import os, sys, argparse

from zfec import filefec

from zfec import __version__ as libversion
__version__ = libversion

def main():
    if '-V' in sys.argv or '--version' in sys.argv:
        print("zfec library version: ", libversion)
        print("zunfec command-line tool version: ", __version__)
        return 0

    parser = argparse.ArgumentParser(description="Decode data from share files.")

    parser.add_argument('-o', '--outputfile', required=True, help='file to write the resulting data to, or "-" for stdout', type=str, metavar='OUTF')
    parser.add_argument('sharefiles', nargs='*', help='shares file to read the encoded data from', type=str, metavar='SHAREFILE')
    parser.add_argument('-v', '--verbose', help='print out messages about progress', action='store_true')
    parser.add_argument('-f', '--force', help='overwrite any file which already in place of the output file', action='store_true')
    parser.add_argument('-V', '--version', help='print out version number and exit', action='store_true')
    args = parser.parse_args()

    if len(args.sharefiles) < 2:
        print("At least two sharefiles are required.")
        return 1

    if args.force:
        outf = open(args.outputfile, 'wb')
    else:
        try:
            flags = os.O_WRONLY|os.O_CREAT|os.O_EXCL | (hasattr(os, 'O_BINARY') and os.O_BINARY)
            outfd = os.open(args.outputfile, flags)
        except OSError:
            print("There is already a file named %r -- aborting.  Use --force to overwrite." % (args.outputfile,))
            return 2
        outf = os.fdopen(outfd, "wb")

    sharefs = []
    # This sort() actually matters for performance (shares with numbers < k
    # are much faster to use than the others), as well as being important for
    # reproducibility.
    args.sharefiles.sort()
    for fn in args.sharefiles:
        sharefs.append(open(fn, 'rb'))
    try:
        filefec.decode_from_files(outf, sharefs, args.verbose)
    except filefec.InsufficientShareFilesError as e:
        print(str(e))
        return 3
    finally:
        outf.close()
        for f in sharefs:
            f.close()

    return 0

# zfec -- fast forward error correction library with Python interface
# 
# Copyright (C) 2007 Allmydata, Inc.
# Author: Zooko Wilcox-O'Hearn
# 
# This file is part of zfec.
#
# See README.rst for licensing information.
