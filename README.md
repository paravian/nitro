# nitro

This package provides integration for the maximum parsimony phylogenetic
software TNT (Goloboff et al. 2008, Goloboff and Catalano 2016), in R. It
interfaces with a locally installed copy of the TNT command line executable and
provides functions for performing branch swapping, parsimony ratchet and driven
(i.e., "New Technology") searches. Each tree search function automatically
returns a set of trees for visualisation or use with other R packages.

## Installation

`nitro` can be installed directly from this Github repository using `devtools`:

```
devtools::install_github("paravian/nitro")
```

## Basic usage

`nitro` requires the command line version (not the menu-driven graphical
version) of TNT to be installed. Windows, macOS and Linux versions of TNT are
available at
[http://www.lillo.org.ar/phylogeny/tnt/](http://www.lillo.org.ar/phylogeny/tnt/).
If you have not used TNT before, you must also agree to the license conditions
for TNT by running it once and following the instructions before you can start
using `nitro`.

The following example shows the generation of a set of most parsimonious trees
using TNT's traditional branch swapping method and the calculation and
visualisation of a strict consensus tree. All tree search commands expect
`phyDat` representations of phylogenetic matrices. These can be created easily
using [`TreeTools`](https://cran.r-project.org/package=TreeTools) functions,
either directly from Nexus or TNT files using `ReadAsPhyDat` or
`ReadTntAsPhyDat`, or directly from a `matrix` object using `MatrixToPhyDat`.

```
# The location of the TNT command line executable file
tnt_path <- "~/tnt64/tnt

matrix <- TreeTools::ReadAsPhyDat("matrix.nex")
branch_swap <- nitro::NitroBranchSwap(replications = 100, hold_rep = 10)
tree_search <- nitro::newTreeSearch(matrix, branch_swap, hold = 100)
analysis <- nitro::tnt(tree_search, tnt_path)
cons <- ape::consensus(analysis@results@trees)
plot(cons)
```
  
Alternatively, `nitro` functions support pipelining as implemented in packages
such as [`magrittr`](https://magrittr.tidyverse.org/) which eliminates temporary
variables and results in more readable code.

```
library(magrittr)

analysis <- TreeTools::ReadAsPhyDat("matrix.nex") %>%
  nitro::newTreeSearch(NitroBranchSwap(replications = 100, hold_rep = 10), hold = 100) %>%
  nitro::tnt(tnt_path)

ape::consensus(analysis@results@trees) %>%
  plot()
```

Presently, `nitro` supports performing branch swapping, parsimony ratchet and
"new technology" searches (using the commands `NitroImplicitEnum`,
`NitroBranchSwap`, `NitroRatchet`, and `NitroDriven` respectively). A subset of
the most relevant options for each method are exposed for their respective
functions, in addition to defining zero-length branch collapsing rules,
outgroup selection, inactive characters and taxa and character ordering.
See the documentation of each command for more details.

Each tree search command returns an list containing the parameters and TNT tree
search command(s) used and a `multiPhylo` object containing all trees found.
Additionally, `nitro` automatically calculates the consistency, retention and
rescaled consistency indices for all trees.

## Planned features

These features are currently not present in `nitro` but are planned for future
inclusion:

* ~~Exclusion of taxa/characters~~
* ~~Specifying additive (ordered) characters~~
* ~~Running (extended) implied weighting analyses~~
* Calculation of support statistics (e.g., bootstrap, jackknife, symmetric
  resampling, Bremer support)
* ~~Specifying constraints on monophyly~~

## Citing

If you have used `nitro` in a publication and have found it useful, please
consider citing it alongside TNT as follows:

Brougham, T. 2020. nitro: Integration of TNT in R. GitHub repository:
https://github.com/paravian/nitro.

## References

Goloboff, P.A., Farris, J.S., Nixon, K.C., 2008. TNT, a free program for
phylogenetic analysis. Cladistics 24, 774–786.
https://doi.org/10.1111/j.1096-0031.2008.00217.x

Goloboff, P.A., Catalano, S.A., 2016. TNT version 1.5, including a full
implementation of phylogenetic morphometrics. Cladistics 32, 221–238.
https://doi.org/10.1111/cla.12160

TNT is made available thanks to a subsidy from the
[Willi Hennig Society](https://cladistics.org/).
