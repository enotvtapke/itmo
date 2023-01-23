#!/bin/bash

N=$1
arr=()
i=0

while [ $(($i)) -lt $N ]; do
    arr+=(1 2 3 4 5 6 7 8 9 10)
    i=$(($i + 10))
done
