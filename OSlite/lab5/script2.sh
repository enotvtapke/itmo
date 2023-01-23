#!/bin/bash

K=30
N=1750000
#1800000 Not working
for ((i=0;i<$K;i++)); do
    `./newmem.bash $N` &
    sleep 1
done
