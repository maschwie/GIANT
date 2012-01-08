PICS = $(shell grep includegraphics *.tex \
	         | sed "s/.*includegraphics.*{\(.*\)}.*/\1.eps/")

default: view

include ../Makefile.incl

#$(shell ./create_MakefilePics.sh)
#include Makefile.pics

ifeq ($(shell test -r ../Makefile.local && echo "true"),true)
  include ../Makefile.local
endif

pics: $(PICS)

view: ps
	gv handbuch.ps

handbuch.pdf: base_pdf handbuch-pdf.pdf

handbuch-pdf.pdf: step1
	pdflatex $*
	pdflatex $*
	makeindex $*
	pdflatex $*
	thumbpdf $*
	pdflatex $*
	mv handbuch-pdf.pdf handbuch.pdf

ps: base_ps handbuch.ps
pdf: handbuch-pdf.pdf

step1: *.tex
	rm -f handbuch.dvi handbuch.ps handbuch.pdf handbuch-pdf.pdf

base_ps: step1 pics handbuch.ind
base_pdf: step1

clean:
	rm -f *~ *.aux *.dvi *.eps *.ilg *.ind *.dix *.log *.toc handbuch.pdf handbuch-pdf.pdf

