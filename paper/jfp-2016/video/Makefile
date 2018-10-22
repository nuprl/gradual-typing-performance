TALK=jfp.ss
RACO=raco

all: preview

compile:
	@${RACO} make -v ${TALK}

preview: compile test
	@${RACO} slideshow --right-half-screen ${TALK}

pict: compile
	@${RACO} pict ${TALK}

test: compile
	@${RACO} test ${TALK}

show: compile
	@${RACO} slideshow ${TALK}

pdf: compile
	@${RACO} slideshow --condense --pdf ${TALK}

