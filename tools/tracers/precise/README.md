precise tracing
===

(No more counting contract applications, just track identifiers directly)


1. Build a modulegraph, annotate edges with exported identifiers
  - DONE, see `boundaries` from `tools/summarize/modulegraph.rkt`
2. Run the program, instrument `#%app` to count
3. Print totals, suggest migration path
