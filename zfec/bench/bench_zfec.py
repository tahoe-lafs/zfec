from zfec import easyfec, Encoder, filefec
from pyutil import mathutil

import os

from pyutil import benchutil

FNAME="benchrandom.data"

def _make_new_rand_file(size):
    open(FNAME, "wb").write(os.urandom(size))

def donothing(results, reslenthing):
    pass

K=3
M=10

d = ""
ds = []
easyfecenc = None
fecenc = None
def _make_new_rand_data(size):
    global d, easyfecenc, fecenc
    d = os.urandom(size)
    del ds[:]
    ds.extend([None]*K)
    blocksize = mathutil.div_ceil(size, K)
    for i in range(K):
        ds[i] = d[i*blocksize:(i+1)*blocksize]
    ds[-1] = ds[-1] + "\x00" * (len(ds[-2]) - len(ds[-1]))
    easyfecenc = easyfec.Encoder(K,M)
    fecenc = Encoder(K,M)

import sha
hashers = [ sha.new() for i in range(M) ]
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
    i = 0
    for c in d:
        i += 1
    assert len(d) == N == i
    pass

def _encode_data_easyfec(N):
    easyfecenc.encode(d)

def _encode_data_fec(N):
    fecenc.encode(ds)

def bench():
    # for f in [_encode_file_stringy_easyfec, _encode_file_stringy, _encode_file, _encode_file_not_really,]:
    # for f in [_encode_file,]:
    # for f in [_encode_file_not_really, _encode_file_not_really_and_hash, _encode_file, _encode_file_and_hash,]:
    # for f in [_encode_data_not_really, _encode_data_easyfec, _encode_data_fec,]:
    for f in [_encode_data_fec,]:
        for BSIZE in [2**22]:
            benchutil.rep_bench(f, n=BSIZE, initfunc=_make_new_rand_data, MAXREPS=64, MAXTIME=None)

bench()
