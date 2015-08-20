Gradual Typing Performance
==========================

Data & code for our POPL 2015 submission.

- To compile the paper, run `cd paper; make`.
- To run a benchmark, run `./run benchmarks/NAME`, where `NAME` is a folder in the `benchmarks/` directory.
  Results will be saved to `./benchmarks/NAME.rktd`


Reproducing our Results
-----------------------
1. Execute the `./run` script for each folder under `benchmarks/`.
   (Disclaimer: this may take over a month)
2. Copy all resulting `.rktd` files to the `paper/data` folder.
3. Edit the filepaths in `paper/data.rkt` to reference the new `.rktd` files.
4. Run `make clean all`.


Sub-Folders
-----------
- `benchmarks` Source code for our benchmark projects.
- `paper` Source for our paper.
- `tools` Utilities for generating benchmark configurations and running experiments.
