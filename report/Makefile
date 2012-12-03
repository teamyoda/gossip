# Example Makefile for Latex
FILE = report
PDFLATEX = pdflatex
BIBTEX = bibtex
PDF = pdf
RM =rm -f

all: paper tidy

tidy:
	$(RM) *.log
	$(RM) *.aux
	$(RM) *.bbl
	$(RM) *.blg
	$(RM) *.dvi
	$(RM) *.out

clean: tidy
	$(RM) $(PDF)/*.pdf
	$(RM) *.toc
	$(RM) *.lof
	$(RM) *.lot

show: clean all
	gnome-open $(PDF)/$(FILE).pdf


paper: $(FILE).tex
	$(PDFLATEX) $(FILE)
	$(PDFLATEX) $(FILE)
	mv $(FILE).pdf $(PDF)

