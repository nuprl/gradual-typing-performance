data
===

For presentation

- `main.rkt` for making graphs; run `main.rktd TAG VERSION` i.e. `main.rktd take5py py`
- `py` data from reticulated
- `module-graphs` LaTeX encoded modulegraphs


To make a figure for the python data

1. Copy the dataset to the `py/` directory
2. Make sure dataset has unique name -- different from any Racket dataset
3. Make sure data has `2**N` rows for some `N` (this has been an issue)
4. Make a modulegraph `module-graphs/DATA.tex`
5. Run `raco gtp-lnm DATA py` and admire the new `.png` file
