# nitro

This package provides integration for the maximum parsimony phylogenetic software TNT in R. It interfaces with a locally installed copy of the TNT command line executable and provides functions for performing branch swapping, parsimony ratchet and driven (i.e., "New Technology") searches. Each tree search function automatically returns a set of trees for visualisation or use with other R packages.

## Installation

`nitro` can be installed directly from this Github repository using `devtools`:

```
devtools::install_github("paravian/nitro")
```

## Basic usage

`nitro` requires the command line version (not the menu-driven graphical version) of TNT to be installed. Windows, macOS and Linux versions of TNT are available at [http://www.lillo.org.ar/phylogeny/tnt/](http://www.lillo.org.ar/phylogeny/tnt/). If you have not used TNT before, you must also agree to the license conditions for TNT by running it once and following the instructions before you can start using `nitro`.

The following example shows the generation of a set of most parsimonious trees using TNT's traditional branch swapping method and the calculation and visualisation of a strict consensus tree. `nitro` accepts matrices as `phyDat` objects and can be created either directly from Nexus files, as in the example below with `ReadAsPhyDat` in the `TreeSearch` package, or from dataframes or matrices using `as.phyDat` in `phangorn`.

```
library(nitro)
library(ape)
library(TreeSearch)

# The location of the TNT command line executable file
tnt.path <- "~/tnt64/tnt

matrix <- ReadAsPhyDat("matrix.nex")

mpts <- branchswap(tnt.path, matrix)

cons <- consensus(mpts)
plot(cons)
```

Presently, `nitro` supports performing branch swapping, parsimony ratchet and "new technology" searches (using the commands `branchswap`, `ratchet`, and `driven` respectively). A subset of the most relevant options for each method are exposed for their respective functions; see the documentation of each command for more details.

Each tree search command returns a `multiPhylo` object containing all trees found. Additionally, `nitro` automatically calculates the consistency, retention and rescaled consistency indices for all trees.

## Planned features

These features are currently not present in `nitro` but are planned for future inclusion:

* Exclusion of taxa/characters
* Specifying additive (ordered) characters
* Running (extended) implied weighting analyses
* Calculation of support statistics (e.g., bootstrap, jackknife, symmetric resampling, Bremer support)
* Specifying constraints on monophyly
