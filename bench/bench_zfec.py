from zfec import easyfec, Encoder, filefec, Decoder
from pyutil import mathutil

import os, sys
from time import time

FNAME="benchrandom.data"

def _make_new_rand_file(size):
    open(FNAME, "wb").write(os.urandom(size))

def donothing(results, reslenthing):
    pass

K=3
M=10

d = b""
ds = []
easyfecenc = None
fecenc = None
def _make_new_rand_data(size, k, m):
    global d, easyfecenc, fecenc, K, M
    K = k
    M = m
    d = os.urandom(size)
    del ds[:]
    ds.extend([None]*k)
    blocksize = mathutil.div_ceil(size, k)
    for i in range(k):
        ds[i] = d[i*blocksize:(i+1)*blocksize]
    ds[-1] = ds[-1] + b"\x00" * (len(ds[-2]) - len(ds[-1]))
    easyfecenc = easyfec.Encoder(k, m)
    fecenc = Encoder(k, m)

from hashlib import sha256
hashers = [ sha256() for i in range(M) ]
def hashem(results, reslenthing):
    for i, result in enumerate(results):
        hashers[i].update(result)

def _encode_file(N):
    filefec.encode_file(open(FNAME, "rb"), donothing, K, M)

def _encode_file_stringy(N):
    filefec.encode_file_stringy(open(FNAME, "rb"), donothing, K, M)

def _encode_file_stringy_easyfec(N):
    filefec.encode_file_stringy_easyfec(open(FNAME, "rb"), donothing, K, M)

def _encode_file_not_really(N):
    filefec.encode_file_not_really(open(FNAME, "rb"), donothing, K, M)

def _encode_file_not_really_and_hash(N):
    filefec.encode_file_not_really_and_hash(open(FNAME, "rb"), donothing, K, M)

def _encode_file_and_hash(N):
    filefec.encode_file(open(FNAME, "rb"), hashem, K, M)

def _encode_data_not_really(N):
    # This function is to see how long it takes to run the Python code
    # that does this benchmarking and accounting and so on but not
    # actually do any erasure-coding, in order to get an idea of how
    # much overhead there is in using Python.  This exercises the
    # basic behavior of allocating buffers to hold the secondary
    # shares.
    sz = N // K
    for i in range(M-K):
        x = '\x00' * sz

def _encode_data_easyfec(N):
    easyfecenc.encode(d)

def _encode_data_fec(N):
    fecenc.encode(ds)

def bench(k, m):
    SIZE = 10**6
    MAXREPS = 1000
    # for f in [_encode_file_stringy_easyfec, _encode_file_stringy, _encode_file, _encode_file_not_really,]:
    # for f in [_encode_file,]:
    # for f in [_encode_file_not_really, _encode_file_not_really_and_hash, _encode_file, _encode_file_and_hash,]:
    # for f in [_encode_data_not_really, _encode_data_easyfec, _encode_data_fec,]:
    print("measuring encoding of data with K=%d, M=%d, encoding %d bytes %d times in a row..." % (k, m, SIZE, MAXREPS))
    # for f in [_encode_data_fec, _encode_data_not_really]:
    for f in [_encode_data_fec]:
        def _init_func(size):
            return _make_new_rand_data(size, k, m)
        for BSIZE in [SIZE]:
            _init_func(BSIZE)
            start = time()
            for _ in range(MAXREPS):
                f(BSIZE)
            elapsed = (time() - start) / MAXREPS
            print("Average MB/s:", (BSIZE / (1024 * 1024)) / elapsed)

    print("measuring decoding of primary-only data with K=%d, M=%d, %d times in a row..." % (k, m, MAXREPS))
    blocks = fecenc.encode(ds)
    sharenums = list(range(len(blocks)))
    decer = Decoder(k, m)
    start = time()
    for _ in range(MAXREPS):
        decer.decode(blocks[:k], sharenums[:k])
    assert b"".join(decer.decode(blocks[:k], sharenums[:k]))[:SIZE] == b"".join(ds)[:SIZE]
    elapsed = (time() - start) / MAXREPS
    print("Average MB/s:", (sum(len(b) for b in blocks) / (1024 * 1024)) / elapsed)

    print("measuring decoding of secondary-only data with K=%d, M=%d, %d times in a row..." % (k, m, MAXREPS))
    blocks = fecenc.encode(ds)
    sharenums = list(range(len(blocks)))
    decer = Decoder(k, m)
    start = time()
    for _ in range(MAXREPS):
        decer.decode(blocks[k:k+k], sharenums[k:k+k])
    assert b"".join(decer.decode(blocks[k:k+k], sharenums[k:k+k]))[:SIZE] == b"".join(ds)[:SIZE]
    elapsed = (time() - start) / MAXREPS
    print("Average MB/s:", (sum(len(b) for b in blocks) / (1024 * 1024)) / elapsed)


k = K
m = M
for arg in sys.argv:
    if arg.startswith('--k='):
        k = int(arg[len('--k='):])
    if arg.startswith('--m='):
        m = int(arg[len('--m='):])

bench(k, m)
