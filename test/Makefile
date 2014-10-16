MLCOMP ?= mlkit
SOURCES=../draw2d.mlb ../draw.sml ../draw.sig Makefile tdiagram.sml tdiagram.mlb simple.sml simple.mlb draw_house.sml draw_house.mlb frac.sml frac.mlb

.PHONY: all
all: house.pdf ground.pdf bath.pdf simple.pdf frac.pdf tdiagram.pdf
	@echo "Wrote files test/{$^}"

.PHONY: simple
simple: simple.pdf

.PHONY: firstfloor
firstfloor: firstfloor.pdf

%.run: $(SOURCES)
	$(MLCOMP) -output $*.run $*.mlb

%.pdf: %.tex
	pdflatex $<
	@echo "Wrote file $@"

house.tex ground.tex bath.tex firstfloor.tex: draw_house.run
	./draw_house.run

simple.tex: simple.run
	./simple.run

frac.tex: frac.run
	./frac.run

tdiagram.tex: tdiagram.run
	./tdiagram.run

.PHONY: clean
clean:
	rm -rf *.aux *.tex *~ *.pdf draw_test draw2_test draw_house *.log *.xml MLB *.run
