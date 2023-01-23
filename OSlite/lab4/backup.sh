#!/bin/bash

curDate=`date +"%Y-%m-%d"`
prevBackupName=`ls ~ | grep -e 'Backup-' | sort | tail -n 1`
prevDate=`echo $prevBackupName | cut -c 7- | sed 's/-//g'`
differ=$(( (`date +"%Y%m%d"` - prevDate)/86400 ))
if [ $differ -gt 6 ]; then
  newFile=Backup-`date +"%Y-%m-%d"`
  mkdir -p ~/$newFile
  cp ~/source/* ~/$newFile
  echo "$newFile $curDate" >> ~/backup-report
  ls ~/source >> ~/backup-report
else
  prevBackup=~/$prevBackupName
  echo "$prevBackupName $curDate" >> ~/backup-report
  for file in `ls ~/source`; do
    if ! [ -f $prevBackup/$file ]; then
        cp ~/source/$file $prevBackup/$file
        echo "$file $curDate" >> ~/backup-report
    fi
  done 
  for file in `ls ~/source`; do
    if [ -f $prevBackup/$file ] && [ `wc -c $prevBackup/$file | awk '{print $1}'` != `wc -c ~/source/$file | awk '{print $1}'` ]; then
        mv $prevBackup/$file $prevBackup/$file.$curDate
        cp ~/source/$file $prevBackup/$file
        echo "$file $file.$curDate" >> ~/backup-report
    fi
  done
fi
