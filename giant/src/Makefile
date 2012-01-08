all:
	make -C ../ all

#temporary rule to compile a single .adb file
%.ali : %.adb
	make -C ../ MAIN="src/"$< all
