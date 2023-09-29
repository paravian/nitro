#' Canale et al. (2022) matrix
#'
#' A phylogenetic matrix of carcharodontosaurid theropod dinosaurs.
#'
#' @format A phyDat object containing 20 taxa and 175 characters.
#' @source Downloaded from the Supplemental Information section of
#'   Canale et al. (2022) <https://doi.org/10.1016/j.cub.2022.05.057>,
#'   converted into a Nexus file in Mesquite 3.61, and read with ReadAsPhydat
#'   from TreeTools.
"canale_2022"

#' Raven and Maidment (2017) matrix
#'
#' A phylogenetic matrix of stegosaurian dinosaurs.
#'
#' @format A list containing the following objects:
#'   \itemize{
#'     \item \code{continuous}: A data frame containing 24 continuous character
#'       traits; and
#'     \item \code{discrete}: A phyDat object containing 91 characters.
#'   }
#'   Both objects score character data for the same 23 taxa.
#' @source Downloaded from the Dryad Digital repository of Raven and
#'   Maidment (2017) <https://doi.org/10.5061/dryad.ds543>. The TNT file was
#'   converted into both a CSV file for the continuous data, and a Nexus file in
#'   Mesquite 3.61 for the discrete character data. The CSV file was read with
#'   `read.table` and the Nexus file was read with ReadAsPhydat from TreeTools.
"raven_2017"
