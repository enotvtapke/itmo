#!/bin/bash

trashpath=~/.trash
lineindex=1
while read line; do
  filename=`echo $line | awk -F'/' '{print $NF}'`
 if [[ "$filename" = "$1" ]]; then
  index=`echo $line | awk -F'//' '{print $1}'`
  path=`echo $line | awk -F'//' '{print $2}'`
  echo "Untrash file $path?"
  read input < /dev/tty
  case $input in
    yes)
      dir=`echo $path | grep -o -e '.*/'`
      if ! [ -d $dir ]; then
        echo "Directory $dir do not exists. Untrashing file in $HOME"
        dir=$HOME
        path=$dir/$filename
      fi
      while [ -f "$path" ]; do
        echo "File $path already exists. Enter new name for untrashed file:"
        read name < /dev/tty
        path=$dir/$name
      done
      ln $trashpath/$index "$path"
      sed -i "${lineindex}d" ~/.trash.log
      rm $trashpath/$index
      ;;
    *)
      echo "skipped"
      ;;
  esac
 fi
  let lineindex=$lineindex+1
done < ~/.trash.log
