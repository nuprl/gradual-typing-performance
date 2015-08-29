all:
	git archive --prefix=paper-archive/ --remote=../ -o paper-archive.tar HEAD
	packer build artifact.json
