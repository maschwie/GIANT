#!/bin/bash

for i in `find -name "*.adb" -or -name "*.ads"`; do
  if [ "`grep '^--  academic purposes.' $i`" != "" ]
  then 
	echo "replacing header for $i"
	cp $i $i.bak
	cat $i | sed -e '1,24d' > /tmp/empty
	cat `pwd`/COPYING /tmp/empty > $i
  fi
done
