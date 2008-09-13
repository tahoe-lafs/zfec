#!/usr/bin/env python

import bisect, random, os, re

from pyutil import fileutil

assert not os.path.exists("benchresults")

os.mkdir("benchresults")

MIN=512
MAX=1024

results = {}

R=re.compile("ave rate: ([1-9][0-9]*)")

def measure(stride):
    fileutil.rm_dir("build")
    fileutil.rm_dir("instdir")
    fileutil.remove_if_possible(os.path.join("zfec", "_fec.so"))
    fileutil.make_dirs("instdir")
    fname = os.path.join("benchresults", "comp_0-stride_%d"%stride)
    os.system("PYTHONPATH=instdir ./setup.py develop --install-dir=instdir --stride=%d >/dev/null" % stride)
    os.system("PYTHONPATH=instdir python -OO ./bench/bench_zfec.py >> %s" % fname)
    inf = open(fname, "rU")
    for l in inf:
        m = R.search(l)
        if m:
            result = int(m.group(1))
            if results.has_key(stride):
                print "stride: %d, results: %d (dup %d)" % (stride, result, results[stride])
            else:
                print "stride: %d, results: %d" % (stride, result)
            results[stride] = result
            break

measure(MIN)
measure(MAX)

while True:
    stride = random.randrange(MIN, MAX+1)
    measure(stride)
