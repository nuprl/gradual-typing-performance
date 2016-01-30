all:
	git archive --prefix=popl-2016-artifact/ --remote=../ -o popl-2016-artifact.tar popl-2016-artifact
	packer build artifact.json

32-bit:
	git archive --prefix=popl-2016-artifact/ --remote=../ -o popl-2016-artifact.tar popl-2016-artifact
	packer build artifact-32.json

readme:
	scribble +m --htmls README.scrbl
