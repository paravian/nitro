#' Run an analysus using implied weighting
#'
#' @param tnt.path The location of the TNT command-line binary.
#' @param tnt.params The parameters for an analysis as specified from the output
#'   of the \code{branchswap}, \code{ratchet}, or \code{driven} commands.
#' @param run.now Logical; perform a phylogenetic analysis straight away or save
#'   commands for use in other methods
#' @param k The concavity constant to apply..
#' @param extended Use extensions to implied weighting
impliedWeights <- function(tnt.path, tnt.params, run.now=TRUE, k=3,
                           multi.k=FALSE) {
  if (is.numeric(k) == FALSE | length(k) != 1) {
    stop("'k' must be an integer")
  } else if (k <= 0 | k > 1000) {
    stop("'k' must be an integer between 0 and 1000")
  }
  if (is.logical(multi.k) == FALSE) {
    stop("'multi.k' must be a logical")
  }

  tnt.params$k <- k
  tnt.params$multi.k <- multi.k

  tnt.params$iw.cmd <- paste0("piwe =", tnt.params$k, ";")
  if (tnt.params$multi.k) {
    tnt.params$eiw.cmd <- "xpiwe (*;"
  }

  if (run.now) {
    output <- runTnt(tnt.path, matrix, tnt.params)
    trees <- tntTreeParse(output, names(matrix))
    return(list(tnt.params = tnt.params, trees = trees))
  } else {
    return(list(tnt.params = tnt.params))
  }
}
