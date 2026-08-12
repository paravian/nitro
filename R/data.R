#' Canale et al. (2022) Discrete Character Taxon Matrix
#'
#' @description
#' A discrete morphological character taxon matrix for abelisaurid theropod
#' dinosaurs, originally published by Canale et al. (2022). Provided as
#' a Nexus file.
#'
#' @details
#' ## File
#' `canale_2022.nex` — a Nexus-format file readable by
#' [TreeTools::ReadAsPhyDat()].
#'
#' ## Contents
#' * **Taxa:** 20 abelisaurid and outgroup theropod taxa.
#' * **Characters:** 175 discrete morphological characters.
#'
#' ## Usage
#' ```r
#' nex_path <- system.file("extdata", "canale_2022.nex", package = "nitro")
#' dm <- TreeTools::ReadPhyDat(nex_path) |> create_matrix()
#' ```
#'
#' @format A Nexus file containing a discrete morphological character
#'   matrix with taxa as rows and characters as columns.
#'
#' @source Canale, J. I., Apesteguía, S., Gallina, P. A., Mitchell, J.,
#'   Smith, N. D., Cullen, T. M., Shinya, A., Haluza, A., Gianechini,
#'   F. A., & Makovicky, P. J. (2022). New giant carnivorous dinosaur
#'   reveals convergent evolutionary trends in theropod arm reduction.
#'   *Current Biology*, 32(14), 3195--3202.
#'   \doi{10.1016/j.cub.2022.05.057}
#'
#' @seealso
#' * [create_matrix()] — converts the parsed data into a [DiscreteMatrix].
#' * [make_tree_analysis()] — uses a [DiscreteMatrix] to configure an
#'   analysis.
#' * [`raven_2017`] — the combined discrete/continuous example dataset.
#'
#' @name canale_2022
#' @aliases canale_2022.nex
NULL

#' Raven et al. (2017) Combined Discrete and Continuous Character Matrix
#'
#' @description
#' A combined discrete morphological and continuous character matrix for
#' stegosaurian dinosaurs, originally published by Raven et al. (2017). Provided
#' as a pair of files: a Nexus file for discrete characters and a CSV file for
#' continuous characters.
#'
#' @details
#' ## Files
#' * `raven_2017.nex` — a Nexus-format file readable by
#'   [TreeTools::ReadAsPhyDat()].
#' * `raven_2017.csv` — a comma-separated values file readable by
#'   [read.table()].
#'
#' ## Contents
#' * **Taxa:** shared across both files.
#' * **Discrete characters:** morphological characters in the Nexus file.
#' * **Continuous characters:** measurements in the CSV file.
#'
#' ## Usage
#' ```r
#' nex_path <- system.file("extdata", "raven_2017.nex", package = "nitro")
#' csv_path <- system.file("extdata", "raven_2017.csv", package = "nitro")
#'
#' dm <- TreeTools::ReadPhyDat(nex_path) |> create_matrix()
#' cm <- read.table(csv_path, sep = ",", header = TRUE) |> create_matrix()
#'
#' combined <- c(dm, cm)
#' ```
#'
#' @format Two files:
#' \describe{
#'   \item{`raven_2017.nex`}{A Nexus file containing a discrete
#'     morphological character matrix with 91 characters.}
#'   \item{`raven_2017.csv`}{A CSV file containing a continuous character
#'     matrix with 24 characters, with taxa as rows and characters as columns.}
#' }
#'
#' @source Raven, T. J., & Maidment, S. C. R. (2017). A new phylogeny of
#'   Stegosauria (Dinosauria, Ornithischia). *Papers in Palaeontology*,
#'   3(1), 1--16. \doi{10.1002/spp2.1081}
#'
#' @seealso
#' * [create_matrix()] — converts the parsed data into a [DiscreteMatrix]
#'   or [ContinuousMatrix].
#' * [make_tree_analysis()] — uses the combined matrix to configure an
#'   analysis.
#' * [`canale_2022`] — the discrete-only example dataset.
#'
#' @name raven_2017
#' @aliases raven_2017.nex raven_2017.csv
NULL

#' Lee (2013) Discrete Character-Taxon Matrix
#'
#' @description
#' A discrete morphological character-taxon matrix for diapsid reptiles,
#' originally published by Lee (2013) as the Diapsid 189 matrix. Provided
#' as a Nexus file. This dataset is used in \pkg{nitro} to demonstrate
#' backbone constraint functionality.
#'
#' @details
#' ## File
#' `lee_2013.nex` — a Nexus-format file readable by
#' [TreeTools::ReadAsPhyDat()].
#'
#' ## Contents
#' * **Taxa:** 27 diapsid and 11 outgroup amniote taxa.
#' * **Characters:** 189 discrete morphological characters.
#'
#' ## Usage
#' ```r
#' nex_path <- system.file("extdata", "lee_2013.nex", package = "nitro")
#' dm <- TreeTools::ReadAsPhyDat(nex_path) |> create_matrix()
#' ```
#'
#' @format A Nexus file containing a discrete morphological character matrix
#'   with taxa as rows and 189 characters as columns.
#'
#' @source Lee, M. S. Y. (2013). Turtle origins: insights from phylogenetic
#'   retrofitting and molecular scaffolds. *Journal of Evolutionary
#'   Biology*, 26(12), 2729--2738. \doi{10.1111/jeb.12268}
#'
#' @seealso
#' * [create_matrix()] — converts the parsed data into a [DiscreteMatrix].
#' * [make_tree_analysis()] — uses a [DiscreteMatrix] to configure an
#'   analysis.
#' * [set_constraint()] — attaches topological constraints to a
#'   [TreeAnalysis].
#' * [BackboneConstraint] — the constraint class used to enforce a
#'   reference topology.
#' * [`canale_2022`] — the discrete-only example dataset.
#' * [`raven_2017`] — the combined discrete and continuous example dataset.
#'
#' @name lee_2013
#' @aliases lee_2013.nex
NULL

#' Gatesy et al. (1997) Bovid Mitochondrial Ribosomal DNA Sequences
#'
#' @description
#' Mitochondrial 12S and 16S ribosomal DNA sequences for bovid and outgroup
#' taxa, originally published by Gatesy et al. (1997). Provided as two Nexus
#' files, one per gene region.
#'
#' @details
#' ## Files
#' * `gatesy_1997_12s.nex` — a Nexus-format file containing aligned 12S rDNA
#'   sequences.
#' * `gatesy_1997_16s.nex` — a Nexus-format file containing aligned 16S rDNA
#'   sequences.
#'
#' ## Contents
#' * **Taxa:** 57 bovid and outgroup artiodactyl taxa, comprising 26 taxa for
#'   which new sequences were generated by Gatesy et al. (1997) and additional
#'   taxa for which sequences were available in GenBank.
#' * **12S rDNA:** Sequences of approximately 250 base pairs per taxon,
#'   corresponding to positions 923--1163 of the *Bos taurus* mitochondrial
#'   genome.
#' * **16S rDNA:** Sequences of approximately 350 base pairs per taxon,
#'   corresponding to positions 2298--2739 of the *Bos taurus* mitochondrial
#'   genome.
#'
#' ## Loading
#' Molecular sequence data must be loaded differently from morphological
#' matrices. `ReadAsPhyDat()` is not appropriate for DNA sequences because it
#' creates a custom character mapping from the states present in the file and
#' sets an incorrect `type` flag incompatible with `DiscreteMatrix`. Instead,
#' use `read.nexus.data()` followed by conversion to a `phyDat` object with
#'  `type = "DNA"`:
#'
#' ```r
#' library(phangorn)
#'
#' nex_12s <- system.file("extdata", "gatesy_1997_12s.nex",
#'                        package = "nitro")
#' nex_16s <- system.file("extdata", "gatesy_1997_16s.nex",
#'                        package = "nitro")
#'
#' dm_12s <- read.nexus.data(nex_12s) |>
#'   as.alignment() |>
#'   as.phyDat(type = "DNA") |>
#'   create_matrix()
#'
#' dm_16s <- read.nexus.data(nex_16s) |>
#'   as.alignment() |>
#'   as.phyDat(type = "DNA") |>
#'   create_matrix()
#' ```
#'
#' @format Two Nexus files:
#' \describe{
#'   \item{`gatesy_1997_12s.nex`}{Aligned 12S mitochondrial rDNA sequences for
#'     57 bovid and outgroup taxa, spanning approximately 250 base pairs per
#'     taxon.}
#'   \item{`gatesy_1997_16s.nex`}{Aligned 16S mitochondrial rDNA sequences for
#'     57 bovid and outgroup taxa, spanning approximately 350 base pairs per
#'     taxon.}
#' }
#'
#' @source Gatesy, J., Amato, G., Vrba, E., Schaller, G., & DeSalle, R.
#'   (1997). A cladistic analysis of mitochondrial ribosomal DNA from the
#'   Bovidae. *Molecular Phylogenetics and Evolution*, 7(3), 303--319.
#'   \doi{10.1006/mpev.1997.0402}
#'
#' @seealso
#' * [create_matrix()] — converts the parsed data into a [DiscreteMatrix].
#' * [`gentry_1992`] — the morphological matrix combined with these sequences in
#'   Gatesy et al. (1997).
#' * [Molecular and Morphological Analysis](molecular-morphology.html) — a
#'   worked example of the complete combined analysis workflow.
#'
#' @name gatesy_1997
#' @aliases gatesy_1997_12s.nex gatesy_1997_16s.nex
NULL

#' Gentry (1992) Bovid Skeletal Character Matrix
#'
#' @description
#' A discrete morphological character matrix of 112 skeletal characters scored
#' for 27 extant bovid species, from the tribal and subfamilial classification
#' of Gentry (1992). Provided as a Nexus file.
#'
#' @details
#' ## File
#' `gentry_1992.nex` — a Nexus-format file readable by
#' [TreeTools::ReadAsPhyDat()].
#'
#' ## Contents
#' * **Taxa:** 27 extant bovid species, with one species included from most
#'   tribes as a typical or unspecialised representative.
#' * **Characters:** 112 discrete skeletal characters covering both cranial and
#'   postcranial characters. Seventeen characters have an intermediate state in
#'   addition to the primitive and derived conditions.
#'
#' ## Ordered characters
#' The following characters are ordered as prescribed by Gentry (1992) and
#' should be passed to the `ordered` argument of `create_matrix()` when loading
#' this dataset for combined analysis:
#'
#' Characters 1, 5, 10, 11, 18, 21, 23, 25, 26, 29, 32, 43, 45, 49,
#' 55, 68, 73.
#'
#' ## Loading
#' ```r
#' library(TreeTools)
#'
#' nex_path <- system.file("extdata", "gentry_1992.nex",
#'                         package = "nitro")
#' morph <- ReadAsPhyDat(nex_path) |>
#'   create_matrix(
#'     ordered = c(1, 5, 10, 11, 18, 21, 23, 25, 26, 29, 32, 43, 45,
#'                 49, 55, 68, 73)
#'   )
#' ```
#'
#' @format A Nexus file containing a discrete morphological character matrix
#'   with 27 taxa as rows and 112 skeletal characters as columns. Character
#'   states are coded as integers; multistate characters are ordered as
#'   prescribed by Gentry (1992).
#'
#' @source Gentry, A. W. (1992). The subfamilies and tribes of the
#'   family Bovidae. *Mammal Review*, 22(1), 1--32.
#'   \doi{10.1111/j.1365-2907.1992.tb00116.x}
#'
#' @seealso
#' * [create_matrix()] — converts the parsed data into a [DiscreteMatrix].
#' * [`gatesy_1997`] — the molecular partitions combined with this matrix.
#' * [Molecular and Morphological Analysis](molecular-morphology.html) — a
#'   worked example of the complete combined analysis workflow.
#'
#' @name gentry_1992
#' @aliases gentry_1992.nex
NULL
