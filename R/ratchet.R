#' Phylogenetic analysis using the parsimony ratchet
#'
#' @param tnt.path The location of the TNT command-line binary.
#' @param matrix A \code{phyDat} object of the matrix.
#' @param run.now Logical; perform a phylogenetic analysis straight away or save
#'   commands for use in other methods
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
ratchet <- function(tnt.path, matrix, run.now = TRUE, hold=100, outgroup=NULL,
                    iterations=50, replacements=40, prob.up=4, prob.down=4) {
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

  if (run.now) {
    output <- runTnt(tnt.path, matrix, tnt.params)
    trees <- tntTreeParse(output, names(matrix))
    return(list(tnt.params = tnt.params, trees = trees))
  } else {
    return(list(tnt.params = tnt.params))
  }
}
