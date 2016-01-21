#!/bin/bash

/bin/rm -rf ./benchresults
mkdir benchresults
STRIDE=32
while [ $(( $STRIDE < 32769 )) ] ; do
    /bin/rm -rf build
    rm zfec/_fec.so
    /bin/rm -rf instdir
    mkdir instdir
    PYTHONPATH=instdir ./setup.py develop --install-dir=instdir --stride=${STRIDE} >/dev/null
    echo $STRIDE
    PYTHONPATH=instdir python -OO ./bench/bench_zfec.py >> benchresults/comp_0-stride_$STRIDE
    tail -1 benchresults/comp_0-stride_$STRIDE
    STRIDE=$(( $STRIDE + 32 ))
done
