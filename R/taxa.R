#' Define inactive taxa
#'
#' @param matrix a \code{phyDat} object of USER type.
#' @param taxa a character vector of taxa to be set as inactive.
#' @return a \code{phyDat} object.
#' @export
inactiveTaxa <- function (matrix, taxa) {
  if (class(matrix) != "phyDat") {
    stop("'matrix' must be a phyDat object")
  }
  if (is.character(taxa) == FALSE) {
    stop("'characters' must be a character")
  }
  if (!all(taxa %in% names(matrix))) {
    stop(paste("Specified inactive taxa not present in matrix: ", taxa[!taxa %in% names(matrix)]))
  }

  attr(matrix, "inactive.taxa") <- names(matrix) %in% taxa
  return(matrix)
}
