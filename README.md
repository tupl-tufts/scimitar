[![DOI](https://zenodo.org/badge/838439044.svg)](https://zenodo.org/doi/10.5281/zenodo.13625531)

# Scimitar

Mixed integer linear programming is a powerful and widely used approach to solving optimization problems, but its expressiveness is limited.

The optimization-aided language Scimitar encodes optimization problems using an expressive functional language, with a compiler that targets a mixed integer linear program solver.

Scimitar provides easy access to encoding techniques that normally require expert knowledge, enabling solve-time conditional constraints, inlining, loop unrolling, and many other high-level language constructs.

# Using

Install `racket` on whatever platform you're using.  E.g. on Ubuntu run `apt-get install racket -y`, on Arch run `pacman -S racket`.

Install a solver.  There are two options: COIN-OR Cbc and Gurobi.

Of the two, COIN-OR Cbc is certainly simpler to install via `apt` or `pacman`, etc., but it is much slower and some benchmarks do not behave correctly due to numerical errors.

To install Gurobi, navigate to the [Gurobi website](https://www.gurobi.com) and sign up for an academic license, and follow the instructions for downloading and installing.

# Configuring

If you set up Gurobi, it is the default solver, so you're done.

If you are using Cbc, you need to do one of two things.

To run the benchmarks, open the file `scimitar/solver.rkt` using your favorite text editor and change the line `(current-solver gurobi)` to `(current-solver cbc)`.

To write your own Scimitar program you don't need to change this file, but you do need to add `(require scimitar/solver) (current-solver cbc)` to the top of your program.


# Examples

To demonstrate Scimitar, we give benchmarks including classic optimization domains and more complex problems.

Running all of these benchmarks is as easy as running `make test`.  

To run a specific benchmark, you can select it, e.g., `make -C examples/pipes test`.

# Paper

For more details on our work, see our paper, **Scimitar: Functional Programs as Optimization Problems** (DOI 10.1145/3689492.3690051) in the proceedings of *Onward! 2024*.
