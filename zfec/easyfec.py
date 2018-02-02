# zfec -- a fast C implementation of Reed-Solomon erasure coding with
# command-line, C, and Python interfaces

import zfec

# div_ceil() was copied from the pyutil library.
def div_ceil(n, d):
    """
    The smallest integer k such that k*d >= n.
    """
    return (n//d) + (n%d != 0)

from base64 import b32encode
def ab(x): # debuggery
    if len(x) >= 3:
        return "%s:%s" % (len(x), b32encode(x[-3:]),)
    elif len(x) == 2:
        return "%s:%s" % (len(x), b32encode(x[-2:]),)
    elif len(x) == 1:
        return "%s:%s" % (len(x), b32encode(x[-1:]),)
    elif len(x) == 0:
        return "%s:%s" % (len(x), "--empty--",)

class Encoder(object):
    def __init__(self, k, m):
        self.fec = zfec.Encoder(k, m)

    def encode(self, data):
        """
        @param data: string

        @return: a sequence of m blocks -- any k of which suffice to
            reconstruct the input data
        """
        chunksize = div_ceil(len(data), self.fec.k)
        l = [ data[i*chunksize:(i+1)*chunksize] + b"\x00" * min(chunksize, (((i+1)*chunksize)-len(data))) for i in range(self.fec.k) ]
        assert len(l) == self.fec.k, (len(l), self.fec.k,)
        assert (not l) or (not [ x for x in l if len(x) != len(l[0]) ], (len(l), [ ab(x) for x in l ], chunksize, self.fec.k, len(data),))
        return self.fec.encode(l)
        
class Decoder(object):
    def __init__(self, k, m):
        self.fec = zfec.Decoder(k, m)

    def decode(self, blocks, sharenums, padlen):
        """
        @param padlen: the number of bytes of padding to strip off;  Note that
            the padlen is always equal to (blocksize times k) minus the length
            of data.  (Therefore, padlen can be 0.)
        """
        data = b''.join(self.fec.decode(blocks, sharenums))
        if padlen:
            return data[:-padlen]
        else:
            return data

# zfec -- fast forward error correction library with Python interface
# 
# Copyright (C) 2007 Allmydata, Inc.
# Author: Zooko Wilcox-O'Hearn
# 
# This file is part of zfec.
# 
# See README.rst for licensing information.
