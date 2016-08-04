PAPER=paper
RACO=raco
SCRIBBLE=scribble

all: paper.pdf

compiled/paper_scrbl.zo: *.rkt *.scrbl
	${RACO} make -v $(PAPER).scrbl

paper.pdf: pkg compiled/paper_scrbl.zo module-graphs/*.tex texstyle.tex
	${SCRIBBLE} ++extra fig-adaptor.tex ++extra fig-fsm-example.tex ++extra fig-tr-example.tex ++extra fig-ts-example.tex ++extra module-graphs ++style texstyle.tex --pdf $(PAPER).scrbl

paper.tex: pkg compiled/paper_scrbl.zo texstyle.tex
	${SCRIBBLE} ++style texstyle.tex --latex $(PAPER).scrbl

pkg:
	${RACO} pkg install --auto --skip-installed glob syntax-sloc with-cache
	${RACO} pkg install --auto --skip-installed ../../tools/benchmark-util
	${RACO} pkg install --auto --skip-installed ../../tools/summarize

one: compiled/paper_scrbl.zo texstyle.tex
	sed -f onecolumn.sed $(PAPER).scrbl > $(PAPER)1.scrbl
	sed -f onecolumn-2.sed texstyle.tex > texstyle1.tex
	${SCRIBBLE} ++style texstyle1.tex --pdf $(PAPER)1.scrbl
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

test:
	${RACO} make -v jfp-test.scrbl
	${SCRIBBLE} ++style texstyle.tex --pdf jfp-test.scrbl

clean:
	rm -r compiled
