#!/usr/bin/env bash

if command -v ack-grep >/dev/null 2>&1; then
  ACK=ack-grep
else
  ACK=ack
fi

contractions="n't|don't|can't|couldn't|wouldnt'|shouldn't|won't"

echo "future cases: "
${ACK} --color "will|would" $*
echo "contractions: "
${ACK} --color "$contractions" $*
