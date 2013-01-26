DIRNAME=`dirname $(PWD)`
BASEDIR=`basename $(PWD)`
DATE=`date '+%a%d%b%y-%H%M'`
FILE=$(BASEDIR)-$(DATE).tar.bz2

all: image dvi pdf

image:
	(cd figures-src && make && make install)

dvi:
	(cd figures-src && make ps && make installps)
	latex -src presentation.tex
	latex -src presentation.tex
	latex -src presentation.tex

ps: dvi
	dvips presentation.dvi

pdf:
	(cd figures-src && make pdf && make installpdf)
	pdflatex presentation.tex
	pdflatex presentation.tex
	pdflatex presentation.tex
	(cd tp && make pdf)

clean: cleantex
	(cd figures-src && make clean)
	(cd figures && make clean)

cleantex:
	rm -f *.aux *.log *.toc *.pdf *.bbl *.blg *.dvi *.ps *~ *.bak *.bmt \
		*.out *.lof *.lot *.mlf? *.mlt? *.mtc *.mtc? \
		presentation.idx presentation.ilg presentation.ind \
		presentation.nav presentation.snm presentation.vrb

dist: clean
	cd $(DIRNAME) ; tar -jcv --exclude CVS -f $(FILE) $(BASEDIR)
