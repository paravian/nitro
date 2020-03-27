#' Run an analysus using implied weighting
#'
#' @param tnt.path the location of the TNT command-line binary.
#' @param analysis the parameters for an analysis as specified from the output
#'   of the \code{implicit.enum}, \code{branchswap}, \code{ratchet}, or
#'   \code{driven} commands.
#' @param run.now a logical value indicating whether to perform a phylogenetic
#'   analysis straight away or save the parameters commands for use in other
#'   methods.
#' @param k an integer value indicating the concavity constant to apply.
#' @param multi.k a logical value indicating whether each character will be
#'   given an independent concavity constant based on the value of \code{k}.
#' @export
impliedWeights <- function(tnt.path, analysis, run.now=TRUE, k=3,
                           multi.k=FALSE) {
  if (is.numeric(k) == FALSE | length(k) != 1) {
    stop("'k' must be an integer")
  } else if (k <= 0 | k > 1000) {
    stop("'k' must be an integer between 0 and 1000")
  }
  if (is.logical(multi.k) == FALSE) {
    stop("'multi.k' must be a logical")
  }

  analysis$tnt.params$k <- k
  analysis$tnt.params$multi.k <- multi.k

  analysis$tnt.params$iw.cmd <- paste0("piwe =", analysis$tnt.params$k, ";")
  if (analysis$tnt.params$multi.k) {
    analysis$tnt.params$eiw.cmd <- "xpiwe (*;"
  }

  if (run.now) {
    output <- runTnt(tnt.path, analysis)
    analysis$trees <- tntTreeParse(output, names(analysis$matrix))
  }
  return(analysis)
}
