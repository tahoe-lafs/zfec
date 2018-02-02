#!/usr/bin/env python

# zfec -- a fast C implementation of Reed-Solomon erasure coding with
# command-line, C, and Python interfaces

from __future__ import print_function

import sys, argparse

from zfec import filefec

from zfec import __version__ as libversion
__version__ = libversion

DEFAULT_K=3
DEFAULT_M=8

def main():

    if '-V' in sys.argv or '--version' in sys.argv:
        print("zfec library version: ", libversion)
        print("zfec command-line tool version: ", __version__)
        sys.exit(0)

    parser = argparse.ArgumentParser(description="Encode a file into a set of share files, a subset of which can later be used to recover the original file.")

    parser.add_argument('inputfile', help='file to encode or "-" for stdin', type=argparse.FileType('rb'), metavar='INF')
    parser.add_argument('-d', '--output-dir', help='directory in which share file names will be created (default ".")', default='.', metavar='D')
    parser.add_argument('-p', '--prefix', help='prefix for share file names; If omitted, the name of the input file will be used.', metavar='P')
    parser.add_argument('-s', '--suffix', help='suffix for share file names (default ".fec")', default='.fec', metavar='S')
    parser.add_argument('-m', '--totalshares', help='the total number of share files created (default %d)' % DEFAULT_M, default=DEFAULT_M, type=int, metavar='M')
    parser.add_argument('-k', '--requiredshares', help='the number of share files required to reconstruct (default %d)' % DEFAULT_K, default=DEFAULT_K, type=int, metavar='K')
    parser.add_argument('-f', '--force', help='overwrite any file which already in place an output file (share file)', action='store_true')
    parser.add_argument('-v', '--verbose', help='print out messages about progress', action='store_true')
    parser.add_argument('-q', '--quiet', help='quiet progress indications and warnings about silly choices of K and M', action='store_true')
    parser.add_argument('-V', '--version', help='print out version number and exit', action='store_true')
    args = parser.parse_args()

    is_infile_stdin = False
    if args.prefix is None:
        args.prefix = args.inputfile.name
        if args.prefix == "<stdin>":
            args.prefix = ""
            is_infile_stdin = True

    if args.verbose and args.quiet:
        print("Please choose only one of --verbose and --quiet.")
        sys.exit(1)

    if args.totalshares > 256 or args.totalshares < 1:
        print("Invalid parameters, totalshares is required to be <= 256 and >= 1\nPlease see the accompanying documentation.")
        sys.exit(1)
    if args.requiredshares > args.totalshares or args.requiredshares < 1:
        print("Invalid parameters, requiredshares is required to be <= totalshares and >= 1\nPlease see the accompanying documentation.")
        sys.exit(1)

    if not args.quiet:
        if args.requiredshares == 1:
            print("warning: silly parameters: requiredshares == 1, which means that every share will be a complete copy of the file.  You could use \"cp\" for the same effect.  But proceeding to do it anyway...")
        if args.requiredshares == args.totalshares:
            print("warning: silly parameters: requiredshares == totalshares, which means that all shares will be required in order to reconstruct the file.  You could use \"split\" for the same effect.  But proceeding to do it anyway...")

    in_file = args.inputfile
    try:
        args.inputfile.seek(0, 2)
        fsize = args.inputfile.tell()
        args.inputfile.seek(0, 0)
    except IOError:
        if is_infile_stdin:
            contents = args.inputfile.read()
            fsize = len(contents)
        else:
            raise Exception("zfec - needs a real (Seekable) file handle to"
                            " measure file size upfront.")

    try:
        return filefec.encode_to_files(in_file, fsize, args.output_dir,
                                    args.prefix, args.requiredshares,
                                    args.totalshares, args.suffix,
                                    args.force, args.verbose)
    finally:
        args.inputfile.close()

# zfec -- fast forward error correction library with Python interface
#
# Copyright (C) 2007 Allmydata, Inc.
# Author: Zooko Wilcox-O'Hearn
#
# This file is part of zfec.
#
# See README.rst for licensing information.
