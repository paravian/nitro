# nitro

This package provides integration for the maximum parsimony phylogenetic
software TNT (Goloboff et al. 2008, Goloboff and Catalano 2016, Goloboff and
Morales 2023), in R. It interfaces with a locally installed copy of the TNT
command line executable and provides functions for performing branch swapping,
parsimony ratchet and driven (i.e., "New Technology") searches. Each tree search
function automatically returns a set of trees for visualisation or use with
other R packages.

## Installation

`nitro` can be installed directly from this Github repository using `devtools`:

```
devtools::install_github("paravian/nitro")
```

## Basic usage

`nitro` requires the command line version (not the menu-driven graphical
version) of TNT. Windows, macOS and Linux versions of TNT are available at
[http://www.lillo.org.ar/phylogeny/tnt/](http://www.lillo.org.ar/phylogeny/tnt/). Once
the files have been extracted from the `.zip` file, TNT can be prepared for use
in R by creating a new `TNTProcess` [R6](https://r6.r-lib.org/) object and
specifying the path of the executable:

```
library(nitro)

tnt_path <- "Downloads/tnt-linux/binaries/tnt"
TNTProcess$new(path = tnt_path)
```

If this is the first time that you have run TNT, you will be prompted to accept
TNT's licence agreement. Read and follow the on-screen instructions until you
receive the following notification:

```
✔ TNT executable verified and ready.
```

The following example shows the generation of a set of most parsimonious trees
using TNT's branch swapping method and the calculation and
visualisation of a strict consensus tree. Like the `TNTProcess` object, options
for tree searches, weighting strategies, resampling, branch support and
topological constraints are defined with R6 classes in `nitro`. These classes
provide a simple yet flexible framework for quickly constructing and modifying
the components of a phylogenetic analysis.

`nitro` provides the `DiscreteMatrix` R6 class for including data from discrete character-taxon matrices. `DiscreteMatrix` accepts `phyDat` matrix objects which
can be created easily using
[`TreeTools`](https://cran.r-project.org/package=TreeTools),
either directly from Nexus or TNT files using `ReadAsPhyDat` or
`ReadTntAsPhyDat` respectively, or directly from a `matrix` object using
`MatrixToPhyDat`:

```
library(TreeTools)
mtx <- DiscreteMatrix$new(matrix = ReadAsPhyDat("matrix.nex"))
```

`DiscreteMatrix` also contains methods for setting the ordering and activity
status of characters.

Individual analyses are configured using the `TreeAnalysis` R6 object. A
`TreeAnalysis` object specifies the matrix and the tree analysis method to use,
as well as other properties such as taxon activity, outgroup taxon,
zero-length branch rule, topological constraints and character weighting
strategy. Once created, a `TreeAnalysis` object can be executed with its `run`
method:

```
branch_swap <- BranchSwappingOptions$new(replications = 1000, hold_rep = 10)
tree_search <- TreeAnalysis$new(discrete_matrix = matrix, method = branch_swap, hold = 100)
res <- tree_search$run()
```
  
Results from `TreeAnalysis` runs are `TreeAnalysisResults` R6 objects. The
`trees` attribute of a `TreeAnalysisResults` object is a list of 
[`treedata`](https://yulab-smu.top/treedata-book/chapter2.html) trees.
`TreeAnalysis` automatically calculates the consistency, retention and
rescaled consistency indices for all trees, and can additionally annotate trees
with data such as resampling and branch supports, when those analysis methods
are used. `treedata` trees can be visualised as publication quality figures with
[`ggtree`](https://yulab-smu.top/treedata-book/chapter4.html). However, trees
from a `TreeAnalysisResults` object can be easily converted into the standard
`phylo` objects used by [`ape`](https://cran.r-project.org/package=ape) using
the generic `as.multiPhylo` function from `TreeTools`. This allows the 
use of any function that accepts `phylo` objects to quickly process and
visualise the results of tree analyses:

```
phys <- as.multiPhylo(res)
cons <- consensus(phys, rooted = TRUE)
plot(ladderize(cons), no.margin = TRUE)
```

# Features

`nitro` currently has R6 configuration option classes for the following tree
analysis methods implemented by TNT:

* Tree searches, including 'traditional' searches (`BranchSwappingOptions`),
ratcheting (`RatchetOptions`), branch breaking (`BranchBreakingOptions`),
sectorial searches (`ConstrainedSectorialSearchOptions`, 
`ExclusiveSectorialSearchOptions`, `RandomSectorialSearchOptions`), drifting 
(`TreeDriftingOptions`), fusing (`TreeFusingOptions`), hybridizing 
(`TreeHybridizingOptions`) and driven (i.e., "new technology") searches 
(`DrivenSearchOptions`);
* Resampling support calculations (`BootstrapOptions`, `JackknifeOptions`, 
`SymmetricResamplingOptions`)
* Branch (i.e., Bremer) supports (`BranchSupportOptions`)
* (Extended) implied weighting (`ImpliedWeightingOptions`)
* Topological constraints (`MonophylyConstraintOptions`)

## Citing

If you have used `nitro` in a publication and have found it useful, please
consider citing it alongside TNT as follows:

Birch, S. 2023. nitro: Integration of TNT in R. GitHub repository:
https://github.com/paravian/nitro.

## References

Goloboff, P.A., Farris, J.S., Nixon, K.C., 2008. TNT, a free program for
phylogenetic analysis. Cladistics 24, 774–786.
https://doi.org/10.1111/j.1096-0031.2008.00217.x

Goloboff, P.A., Catalano, S.A., 2016. TNT version 1.5, including a full
implementation of phylogenetic morphometrics. Cladistics 32, 221–238.
https://doi.org/10.1111/cla.12160

Goloboff, P.A., Morales, M.E., 2023. TNT version 1.6, with a graphical interface
for MacOS and Linux, including new routines in parallel. Cladistics 39, 144–153. https://doi.org/10.1111/cla.12524


TNT is made available thanks to a subsidy from the
[Willi Hennig Society](https://cladistics.org/).
