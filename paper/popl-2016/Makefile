PAPER=paper

all: paper.pdf

compiled/paper_scrbl.zo: *.rkt *.scrbl
	raco make -v $(PAPER).scrbl

paper.pdf: pkg compiled/paper_scrbl.zo module-graphs/*.tex texstyle.tex scripts/*.rkt
	scribble ++extra fig-adaptor.tex ++extra aec-badge-popl.pdf ++extra lnm ++extra module-graphs ++style texstyle.tex --pdf $(PAPER).scrbl

paper.tex: pkg compiled/paper_scrbl.zo texstyle.tex
	scribble ++style texstyle.tex --latex $(PAPER).scrbl

pkg:
	raco pkg install --skip-installed glob
	raco pkg install --skip-installed unstable

one: compiled/paper_scrbl.zo texstyle.tex
	sed -f onecolumn.sed $(PAPER).scrbl > $(PAPER)1.scrbl
	sed -f onecolumn-2.sed texstyle.tex > texstyle1.tex
	scribble ++style texstyle1.tex --pdf $(PAPER)1.scrbl
	mv $(PAPER)1.pdf $(PAPER).pdf

# Check style:
proof:
	echo "weasel words: "
	sh scripts/weasel.sh *.scrbl
	echo
	echo "passive voice: "
	sh scripts/passive.sh *.scrbl
	echo
	echo "misc: "
	sh scripts/misc.sh *.scrbl
	echo
	echo "duplicates: "
	perl scripts/duplicates.pl *.scrbl

clean:
	rm -r compiled
