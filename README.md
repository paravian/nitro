# nitro

This package provides integration for the maximum parsimony phylogenetic
software TNT (Goloboff et al. 2008, Goloboff and Catalano 2016, Goloboff and
Morales 2023), in R. It interfaces with a locally installed copy of the TNT
command line executable and provides functions for performing branch swapping,
parsimony ratchet and driven (i.e., "New Technology") searches. Each tree search
function automatically returns a set of trees for visualisation or use with
other R packages.

## Installation
 

`nitro` is not currently available on CRAN and must be installed directly from
this repository. Firstly, install the `treeio` package, which is made available
via [Bioconductor](https://bioconductor.org) and which can be installed using
the R package `BiocManager`:

```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("treeio")
```

Then install `nitro` from this repository using `devtools`:

```
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("paravian/nitro")
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
