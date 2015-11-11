R_PROG = R
RNWFILES = $(wildcard *.rnw)
TEXFILES = $(wildcard *.tex)
DEPENDS = $(patsubst %.rnw,%.tex,$(RNWFILES)) $(TEXFILES) $(RFILES)
MASTER = $(RNWFILES:rnw=pdf)

all: $(MASTER)

$(MASTER): $(DEPENDS)

%.tex: %.rnw
	sh ./vc
	${R_PROG} --no-save --no-restore -e "require(knitr); knit('$*.rnw'); purl('$*.rnw', documentation=0)" $<

%.pdf: %.tex
	${R_PROG} CMD texi2pdf $<

clean:
	$(RM) *.aux *.log *.toc *.nav *.snm *.vrb *.dvi *.bbl *.blg *.out *.idx *.ind *.ilg

veryclean:
	$(RM) *.aux *.log *.toc *.nav *.snm *.vrb *.dvi *.bbl *.blg *.out *.idx *.ind *.ilg *.tex *.pdf 
