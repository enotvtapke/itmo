#!/bin/bash

trashpath=~/.trash
filename=$1
mkdir -p $trashpath
linkname=$(( `ls $trashpath | sort -n | tail -n 1` + 1 ))
if [ -f ./"$filename" ]; then
  ln ./"$filename" ~/.trash/$linkname
  echo -e $linkname//$PWD/"$filename" >> ~/.trash.log
  rm ./"$filename"
fi
