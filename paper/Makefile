PAPER=paper

all: paper.pdf

compiled/paper_scrbl.zo: *.rkt *.scrbl
	raco make -v $(PAPER).scrbl


paper.pdf: compiled/paper_scrbl.zo texstyle.tex
	scribble ++style texstyle.tex --pdf $(PAPER).scrbl

paper.tex: compiled/paper_scrbl.zo texstyle.tex
	scribble ++style texstyle.tex --latex $(PAPER).scrbl

asumu:
	raco make -v $(PAPER).scrbl
	scribble --latex ++style texstyle.tex $(PAPER).scrbl
	pdflatex $(PAPER).tex

mf: compiled/paper_scrbl.zo texstyle.tex
	sed -f onecolumn.sed $(PAPER).scrbl > $(PAPER)1.scrbl
	sed -f onecolumn-2.sed texstyle.tex > texstyle1.tex
	cp sigplanconf.cls ~/plt/pkgs/scribble-pkgs/scribble-lib/scribble/sigplan/sigplanconf.cls 
	scribble ++style texstyle1.tex --pdf $(PAPER)1.scrbl
	mv $(PAPER)1.pdf $(PAPER).pdf

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
