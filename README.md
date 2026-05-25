# nitro <img src="man/figures/logo.png" align="right" height="139" alt="" />

`nitro` provides an R interface to
[TNT](http://www.lillo.org.ar/phylogeny/tnt/), a program for phylogenetic
analysis under maximum parsimony (Goloboff et al., 2008; Goloboff &
Catalano, 2016; Goloboff & Morales, 2023). It interfaces with a locally
installed copy of the TNT command line executable and provides a clean,
pipeable R API for configuring and running parsimony analyses, retrieving
results as standard R phylogenetic objects, and integrating with the
broader R phylogenetics ecosystem.

## Installation

`nitro` is not currently available on CRAN. Install the development
version from GitHub using `remotes`:

```r
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")

remotes::install_github("paravian/nitro")
```

You will also need a local installation of the TNT command line
executable, available for Windows, macOS, and Linux at
[http://www.lillo.org.ar/phylogeny/tnt/](http://www.lillo.org.ar/phylogeny/tnt/).

## Getting started

Attach the TNT executable to your R session using `create_interface()`, then
load a character matrix, configure an analysis, and run it:

```r
library(nitro)
library(TreeTools)

# Attach TNT
interface <- create_interface("~/Downloads/tnt-linux/TNT-bin/tnt")

# Load a discrete character matrix from a Nexus file
nex_path <- system.file("extdata", "canale_2022.nex", package = "nitro")
dm <- ReadAsPhyDat(nex_path) |> create_matrix()

# Configure and run a branch swapping analysis
ta <- make_tree_analysis(dm)
ta <- set_tree_search(ta, name = "branch_swapping", replications = 1000)

results <- execute_analysis(interface, ta, hold = 10000)

# Compute and plot the strict consensus
library(ape)
cons <- as.phylo(results) |> consensus(rooted = TRUE)
plot(ladderize(cons), no.margin = TRUE)
```

See the [Getting Started](articles/nitro.html) article for a full walkthrough.

## Features

### Tree searches

`nitro` supports all tree search strategies implemented by TNT:

| Method | Class | TNT command |
|---|---|---|
| Branch swapping | `BranchSwappingCommand` | `mult` |
| Branch breaking | `BranchBreakingCommand` | `bbreak` |
| Extra ("new technology") searches | `ExtraSearchMethodsCommand` | `xmult` |
| — Random sectorial search | `RandomSectorialSearchCommand` | `sectsch rss` |
| — Exclusive sectorial search | `ExclusiveSectorialSearchCommand` | `sectsch xss` |
| — Constrained sectorial search | `ConstrainedSectorialSearchCommand` | `sectsch css` |
| — Tree drifting | `TreeDriftingCommand` | `drift` |
| — Tree fusing | `TreeFusingCommand` | `tfuse` |
| — Parsimony ratchet | `RatchetCommand` | `ratchet` |

All search strategies are configured via `set_tree_search()` and can be
combined in a single `TreeAnalysis`.

### Character weighting

| Method | Class | TNT command |
|---|---|---|
| Implied weighting | `ImpliedWeightingCommand` | `piwe` |
| Extended implied weighting | `ExtendedImpliedWeightingCommand` | `xpiwe` |

Weighting is configured via `set_weighting()`. See the
[Implied Weighting](articles/implied-weighting.html) article for details
and a comparison with equal weights results.

### Resampling support

| Method | Class | TNT command |
|---|---|---|
| Bootstrap | `BootstrapCommand` | `resample boot` |
| Jackknife | `JackknifeCommand` | `resample jak` |
| Symmetric resampling | `SymmetricResamplingCommand` | `resample sym` |

Resampling is configured via `set_support()`. Analyses can be run with or
without a target tree. See the
[Resampling Analyses](articles/resampling-analyses.html) article for
details.

### Combined discrete and continuous characters

`nitro` supports combined analyses of discrete morphological characters and
continuous measurements. Both matrix types are created via `create_matrix()` and
combined with `c()`. See the
[Continuous Characters](articles/continuous-characters.html) article for
details.

## Output and interoperability

`execute_analysis()` returns a `TreeAnalysisResults` object. Trees are stored as
[`treedata`](https://yulab-smu.top/treedata-book/chapter2.html) objects, which
combine the standard `ape` `phylo` representation with a table of node-level
annotations (support values, etc.).

Results can be converted to standard R objects for use with any package in the
`ape` ecosystem:

```r
# Single tree or multiPhylo
trees <- as.phylo(results)

# Per-tree statistics (length, CI, RI, RC)
results$statistics

# Visualise with ggtree (Bioconductor)
library(ggtree)
ggtree(results$trees[[1]]) + geom_tiplab()
```

## Citing

If you use `nitro` in a publication, please cite it alongside TNT:

> Birch, S. (2026). *nitro*: Integration of TNT in R. GitHub repository:
> https://github.com/paravian/nitro.

TNT is made available thanks to a subsidy from the
[Willi Hennig Society](https://cladistics.org/). Please also cite the
relevant TNT publications:

> Goloboff, P. A., Farris, J. S., & Nixon, K. C. (2008). TNT, a free
> program for phylogenetic analysis. *Cladistics*, 24(5), 774–786.
> https://doi.org/10.1111/j.1096-0031.2008.00217.x

> Goloboff, P. A., & Catalano, S. A. (2016). TNT version 1.5, including
> a full implementation of phylogenetic morphometrics. *Cladistics*,
> 32(3), 221–238. https://doi.org/10.1111/cla.12160

> Goloboff, P. A., & Morales, M. E. (2023). TNT version 1.6, with a
> graphical interface for MacOS and Linux, including new routines in
> parallel. *Cladistics*, 39(2), 144–153.
> https://doi.org/10.1111/cla.12524
