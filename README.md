# nitro

This package provides integration for the maximum parsimony phylogenetic software TNT in R. It interfaces with a locally installed copy of the TNT command line executable and provides functions for performing branch swapping, parsimony ratchet and driven (i.e., "New Technology") searches. Each tree search function automatically returns a set of trees for visualisation or use with other R packages.

## Installation

`nitro` can be installed directly from this Github repository using `devtools`:

```
devtools::install_github("paravian/nitro")
```

## Basic usage

`nitro` requires the command line version (not the menu-driven graphical version) of TNT to be installed. Windows, macOS and Linux versions of TNT are available at [http://www.lillo.org.ar/phylogeny/tnt/](http://www.lillo.org.ar/phylogeny/tnt/). If you have not used TNT before, you must also agree to the license conditions for TNT by running it once and following the instructions before you can start using `nitro`.

The following example shows the generation of a set of most parsimonious trees using TNT's traditional branch swapping method and the calculation and visualisation of a strict consensus tree. All tree search commands expect `phyDat` representations of phylogenetic matrices. These can be created created either from Nexus or TNT files, using `ReadAsPhyDat` or `ReadTntAsPhyDat` respectively from `TreeTools`, or directly from a `data.frame` or `matrix` object using `as.phyDat` from `phangorn`.

```
# The location of the TNT command line executable file
tnt.path <- "~/tnt64/tnt

matrix <- TreeTools::ReadAsPhyDat("matrix.nex")
branchswap.params <- nitro::branchswap()
mpts <- nitro::tnt(branchswap.params, tnt.path)
consensus <- ape::consensus(mpts$trees)
plot(cons)
```
  
Alternatively, `nitro` functions support pipelining as implemented in packages such as [`magrittr`](https://magrittr.tidyverse.org/) which eliminates temporary variables and results in more readable code.

```
library(magrittr)

TreeTools::ReadAsPhyDat("matrix.nex") %>%
  nitro::branchswap(matrix) %>%
  tnt(tnt.path) %$%
  consensus(trees) %>%
  plot()
```

Presently, `nitro` supports performing branch swapping, parsimony ratchet and "new technology" searches (using the commands `branchswap`, `ratchet`, and `driven` respectively). A subset of the most relevant options for each method are exposed for their respective functions; see the documentation of each command for more details.

Each tree search command returns an list containing the parameters and TNT tree search command(s)
used and a `multiPhylo` object containing all trees found. Additionally, `nitro` automatically calculates the consistency, retention and rescaled consistency indices for all trees.

## Planned features

These features are currently not present in `nitro` but are planned for future inclusion:

* ~~Exclusion of taxa/characters~~
* ~~Specifying additive (ordered) characters~~
* ~~Running (extended) implied weighting analyses~~
* Calculation of support statistics (e.g., bootstrap, jackknife, symmetric resampling, Bremer support)
* Specifying constraints on monophyly
