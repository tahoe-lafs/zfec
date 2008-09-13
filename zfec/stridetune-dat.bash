#!/bin/bash

DAT=stridetune.dat
rm -f $DAT
for F in benchresults/comp_0-stride_*; do
    NUM=${F:27}
    for M in `grep "^N: " benchresults/comp_0-stride_$NUM | cut -d':' -f10-` ; do
        echo "$NUM $M" >> $DAT
    done
done
