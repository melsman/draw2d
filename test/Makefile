MLCOMP ?= mlkit
SOURCES=../draw2d.mlb ../draw.sml ../draw.sig Makefile tdiagram.sml tdiagram.mlb simple.sml simple.mlb draw_house.sml draw_house.mlb draw_house2.sml draw_house2.mlb frac.sml frac.mlb

.PHONY: all
all: house.pdf ground.pdf bath.pdf simple.pdf frac.pdf tdiagram.pdf firstfloor.pdf
	@echo "Wrote files test/{$^}"

.PHONY: all2
all2: house2.pdf ground2.pdf bath2.pdf house2a.pdf groundfloor2.pdf
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

house2.tex ground2.tex bath2.tex firstfloor2.tex house2a.tex groundfloor2.tex: draw_house2.run
	./draw_house2.run

simple.tex: simple.run
	./simple.run

frac.tex: frac.run
	./frac.run

tdiagram.tex: tdiagram.run
	./tdiagram.run

.PHONY: clean
clean:
	rm -rf *.aux *.tex *~ *.pdf draw_test draw2_test draw_house draw_house2 *.log *.xml MLB *.run
