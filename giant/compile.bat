set IML_PATH=/giant/iml/iml_browser
set GIANT_PATH=../
set GTKADA_PATH=../../GtkAda-2.2.0
set XMLADA_PATH=../../src/xmlada-0.7.1

mkdir obj-Win32
cd obj-Win32
gnatmake -v -m -g -gnatX -gnata -gnatf -gnatwl -gnato -fstack-check -I%GIANT_PATH%/src -I%GIANT_PATH%/src/config -I%GIANT_PATH%/src/control -I%GIANT_PATH%/src/graph_lib -I%GIANT_PATH%/src/gsl -I%GIANT_PATH%/src/gsl/generated -I%GIANT_PATH%/src/gui -I%GIANT_PATH%/src/project -I%GIANT_PATH%/src/reuse -I%GIANT_PATH%/src/utils -I%GIANT_PATH%/src/vis -I%IML_PATH%/IML.generated -I%IML_PATH%/IML.src -I%IML_PATH%/Reuse.src -I%IML_PATH%/usr.local.reuse -I../aunit-1.01/aunit/framework -I../aunit-1.01/aunit/text_reporter -I%XMLADA_PATH%/dom -I%XMLADA_PATH%/input_sources -I%XMLADA_PATH%/sax -I%XMLADA_PATH%/unicode -I%GTKADA_PATH%/include/gtkada %GIANT_PATH%/src/giant-main.adb -o %GIANT_PATH%/giant.exe -bargs -static -E
cd ..

set PATH=%PATH%;..\GtkAda-2.2.0\bin
