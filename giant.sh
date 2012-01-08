#!/bin/sh

cd `dirname $0`
system=`uname -s`
if [ "$system" = "Linux" ]; then
	./giant-Linux $*
elif [ "$system" = "SunOS" ]; then
	LD_LIBRARY_PATH=src/iml_browser_030825:$LD_LIBRARY_PATH ./giant-SunOS $*
else
	echo "$system is unsupported."
	exit 1
fi
