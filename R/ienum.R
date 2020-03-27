#' Phylogenetic analysis using implicit enumeration
#'
#' @importFrom utils file_test tail
#' @param tnt.path the location of the TNT command-line binary.
#' @param matrix a \code{phyDat} object of the matrix.
#' @param run.now a logical value indicating whether to perform a phylogenetic
#'   analysis straight away or save the parameters commands for use in other
#'   methods.
#' @param collapse an integer indicating the rule for collapsing of zero length
#'   branches. The options are:
#'   \itemize{
#'   \item \code{1}: collapse an interior branch of the maximum possible length
#'     of the branch is zero
#'   \item \code{2}: keep zero length branches if ancestor and descendent states
#'     differ
#'   \item \code{3}: collapse an interior branch if the minimum possible length
#'     of the branch is zero (the default)
#'   \item \code{4}: discard all trees that must contain a zero length branch
#'   }
#' @param hold an integer value indicating the maximum number of trees to allow
#'   TNT to hold in memory.
#' @param outgroup the name of the taxon to set as the outgroup for the
#'   phylogenetic analysis. By default, the first taxon in the matrix is the
#'   outgroup.
#' @return a list containing the search parameters and TNT command string, a
#'   \code{phyDat} object containing the phylogenetic matrix analysed and, if
#'   \code{run.now} is \code{TRUE}, a \code{multiPhylo} object of trees found
#'   from the search commands.
#' @export
implicit.enum <- function (tnt.path, matrix, run.now=TRUE, hold=100, collapse=3, outgroup=NULL) {
  # Validate command arguments
  if (file_test("-f", tnt.path) == FALSE) {
    stop("'tnt.path' does not exist")
  }
  if (!inherits(matrix, "phyDat")) {
    stop("'matrix' must be a phyDat object")
  }
  if (is.logical(run.now) == FALSE) {
    stop("'run.now' must be a logical")
  }
  if (is.numeric(hold) == FALSE | length(hold) != 1) {
    stop("'hold' must be an integer")
  } else if (hold %% 1 != 0 | hold <= 0) {
    stop("'hold' must be an integer > 0")
  }
  if (is.numeric(collapse) == FALSE | length(collapse) != 1) {
    stop("'collapse' must be an integer")
  } else {
    if (!collapse %in% 1:4) {
      stop("'collapse' must be an integer between 1 and 4")
    }
  }
  if ((is.null(outgroup) == TRUE | is.character(outgroup) == TRUE) == FALSE &
      length(outgroup) != 1) {
    stop("'outgroup' must be a character")
  }

  if (length(matrix) > 25) {
    warning("Matrix contains more than 25 OTUs, analysis may not complete in a
            reasonable timeframe")
  }

  tnt.params <- list(collapse = collapse, hold = hold, outgroup = outgroup,
                     cmd = "ienum;")

  analysis <- list(tnt.params = tnt.params, matrix = matrix)
  if (run.now) {
    output <- runTnt(tnt.path, analysis)
    analysis$trees <- tntTreeParse(output, names(matrix))
  }
  return(analysis)
}
