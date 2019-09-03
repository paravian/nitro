#' Phylogenetic analysis using the parsimony ratchet
#'
#' @param tnt.path The location of the TNT command-line binary.
#' @param matrix A \code{phyDat} object of the matrix.
#' @param run.now Logical; perform a phylogenetic analysis straight away or save
#'   commands for use in other methods
#' @param collapse Set rule for collapsing of zero length branches. The options
#'   are:
#'   \itemize{
#'   \item \code{1}: collapse an interior branch of the maximum possible length of the
#'   branch is zero
#'   \item \code{2}: keep zero length branches if ancestor and descendent states
#'   differ
#'   \item \code{3}: collapse an interior branch if the minimum possible length of the
#'   branch is zero
#'   \item \code{4}: discard all trees that must contain a zero length branch
#'   }
#' @param hold The maximum number of trees to allow TNT to hold in memory
#'   (typically \code{replications} * \code{hold.rep}).
#' @param outgroup The outgroup taxon for the phylogenetic analysis. By default,
#'   the first taxon in the matrix is considered the outgroup.
#' @param iterations The number of iterations.
#' @param replacements The number of replacements (i.e., accepted tree
#'   rearrangements) to perform in each perturbation phase.
#' @param prob.up The probability of upweighting a character.
#' @param prob.down The probability of downweighting a character.
#' @return A list containing the search parameters and TNT command string and,
#'   if \code{run.now} is \code{TRUE}, a \code{multiPhylo} object of trees
#'   found from the search commands.
#' @export
ratchet <- function(tnt.path, matrix, run.now = TRUE, hold=100, collapse=3,
                    outgroup=NULL, iterations=50, replacements=40, prob.up=4,
                    prob.down=4) {
  if (file_test("-f", tnt.path) == FALSE) {
    stop("'tnt.path' does not exist")
  }
  if (class(matrix) != "phyDat") {
    stop("'matrix' must be a phyDat object")
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
  if ((is.null(outgroup) == TRUE | is.integer(outgroup) == TRUE) == FALSE &
      length(outgroup) != 1) {
    stop("'outgroup' must be a character")
  }
  if (is.numeric(iterations) == FALSE | length(iterations) != 1) {
    stop("'iterations' must be an integer")
  } else if (iterations %% 1 != 0 | iterations <= 0) {
    stop("'iterations' must be an integer > 0")
  }
  if (is.numeric(hold) == FALSE | length(hold) != 1) {
    stop("'hold' must be an integer")
  } else if (hold %% 1 != 0 | hold <= 0) {
    stop("'hold' must be an integer > 0")
  }

  tnt.params <- list(collapse = collapse, hold = hold, outgroup = outgroup,
                     replications = iterations)

  tnt.params$cmd <- c(paste("mult= wagner replic 10;"),
                      paste("ratchet= iter", iterations, "numsubs",
                            replacements, "upfactor", prob.up,
                            "downfact", prob.down, ";"))

  analysis <- list(tnt.params = tnt.params, matrix = matrix)
  if (run.now) {
    output <- runTnt(tnt.path, analysis)
    analysis$trees <- tntTreeParse(output, names(matrix))
  }
  return(analysis)
}
