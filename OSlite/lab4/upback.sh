#!/bin/bash

prevBackupName=`ls ~ | grep -e 'Backup-' | sort | tail -n 1`
prevBackup=~/$prevBackupName
mkdir -p ~/restore
rm ~/restore/*
cp $prevBackup/* ~/restore
for file in `ls ~/restore`; do
  prefix=`echo $file | sed -E 's/\.[0-9]{4}-[0-9]{2}-[0-9]{2}$//g'`
  count=`ls ~/restore | grep -c "$prefix"`
  if [ `echo $file | grep -E "\.[0-9]{4}-[0-9]{2}-[0-9]{2}$"` ] && [ $count -gt 1 ]; then
    rm ~/restore/$file
  fi
done
