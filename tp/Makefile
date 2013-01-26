DIRNAME=`dirname $(PWD)`
BASEDIR=`basename $(PWD)`
DATE=`date '+%a%d%b%y-%H%M'`
FILE=$(BASEDIR)-$(DATE).tar.bz2

all: pdf

pdf: tp1

tp1: tp1.tex
	pdflatex tp1.tex
	pdflatex tp1.tex
	pdflatex tp1.tex

clean: cleantex

cleantex:
	rm -f *.aux *.log *.toc *.pdf *.bbl *.blg *.dvi *.ps *~ *.bak *.bmt \
		*.out *.lof *.lot *.mlf? *.mlt? *.mtc *.mtc?

dist: clean
	cd $(DIRNAME) ; tar -jcv --exclude CVS -f $(FILE) $(BASEDIR)
