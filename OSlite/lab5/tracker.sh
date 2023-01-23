#!/bin/bash

#pid=`cat .pid`
name='mem.bash'
report='report.log'
if [ $1 ]; then
    name=$1
fi
if [ $2 ]; then
    report=$2
fi
out="$name.log"
pid=`pgrep $name`
while ! [ $pid ]; do
    pid=`pgrep $name`
    sleep 0.2
done
echo "Process $name with pid=$pid found. Begin writing to $out."
> $out
echo $name $pid >> $out
echo >> $out

while true; do
    mem=`top -b -n 1 -o %MEM | sed '4q;d'`
    swap=`top -b -n 1 -o %MEM | sed '5q;d'`
    echo $mem >> $out
    echo $swap >> $out
    list=`top -b -n 1 -o %MEM | head -n 12 | tail -n 5 | awk '{print $12}'`
    echo $list >> $out
    currentProc=`top -b -n 1 | grep $pid`
    echo $currentProc >> $out
    if ! [ "$currentProc" ]; then
        echo `dmesg | grep "$name" | tail -n 2` >> $out
        echo `cat "$report" | tail -n 1` >> $out
        echo "Process $pid stoped"
        exit
    fi
    echo >> $out
    sleep 1
done
