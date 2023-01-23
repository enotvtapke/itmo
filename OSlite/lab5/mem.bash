#!/bin/bash

out='report.log'
arr=()
i=0
> $out

while true; do
    arr+=( 1 2 3 4 5 6 7 8 9 10 )
    if [ $(($i % 100000)) == 0 ]; then
        echo $i >> $out
    fi
    i=$(($i + 1))
done
