generated :
	ayacc 'File=>giant-parser.y Summary=>On'
	aflex -E giant-scanner.aflex
	# giant-parser.adb
	sed '13,15s/Giant.Parser/Parser/g'\
	giant-parser.adb > giant-parser.adb~
	mv giant-parser.adb~ giant-parser.adb
	# giant-scanner.ads
	sed 's/giant-scanner/Giant.Scanner/g'\
	giant-scanner.ads > giant-scanner.ads~
	mv giant-scanner.ads~ giant-scanner.ads
	# giant-scanner.adb
	sed 's/giant-scanner/Giant.Scanner/g'\
	giant-scanner.adb > giant-scanner.adb~
	mv giant-scanner.adb~ giant-scanner.adb
	sed '4,5s/Giant.Scanner/Scanner/g'\
	giant-scanner.adb > giant-scanner.adb~
	mv giant-scanner.adb~ giant-scanner.adb
	# giant-scanner-dfa.ads
	sed 's/giant-scanner/Giant.Scanner/g'\
	giant-scanner-dfa.ads > giant-scanner-dfa.ads~
	mv giant-scanner-dfa.ads~ giant-scanner-dfa.ads
	# giant-scanner-dfa.adb
	sed 's/giant-scanner/Giant.Scanner/g'\
	giant-scanner-dfa.adb > giant-scanner-dfa.adb~
	mv giant-scanner-dfa.adb~ giant-scanner-dfa.adb
	# giant-scanner-io.ads
	sed 's/giant-scanner/Giant.Scanner/g'\
	giant-scanner-io.ads > giant-scanner-io.ads~
	mv giant-scanner-io.ads~ giant-scanner-io.ads
	# giant-scanner-io.adb
	sed 's/giant-scanner/Giant.Scanner/g'\
	giant-scanner-io.adb > giant-scanner-io.adb~
	mv giant-scanner-io.adb~ giant-scanner-io.adb
