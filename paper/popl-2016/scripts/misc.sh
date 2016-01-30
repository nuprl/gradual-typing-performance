#!/usr/bin/env bash

contractions="n't|don't|can't|couldn't|wouldnt'|shouldn't|won't"

echo "future cases: "
ack-grep --color "will|would" $*
echo "contractions: "
ack-grep --color "$contractions" $*
