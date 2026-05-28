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
