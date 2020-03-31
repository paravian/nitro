#' Phylogenetic analysis using the parsimony ratchet
#'
#' @importFrom utils tail
#' @importFrom progress progress_bar
#' @param matrix a \code{phyDat} object of the matrix.
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
#' @param iterations an integer value indicating the number of iterations.
#' @param replacements an integer value indicating the number of replacements
#'   (i.e., accepted tree rearrangements) to perform in each perturbation
#'   phase.
#' @param prob.up an integer value indicating the probability of upweighting a
#'   character.
#' @param prob.down an integer value indicating the probability of
#'   downweighting a character.
#' @return a list containing the search parameters and TNT command string, a
#'   \code{phyDat} object containing the phylogenetic matrix analysed and, if
#'   \code{run.now} is \code{TRUE}, a \code{multiPhylo} object of trees found
#'   from the search commands.
#' @export
ratchet <- function(matrix, hold=100, collapse=3, outgroup=NULL, iterations=50,
                    replacements=40, prob.up=4, prob.down=4) {
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

  # Set up progress bar and define function for determining progress
  progress <- list(
    bar = progress_bar$new(format = "Ratcheting: [:bar] :current/:total iters | Best score: :score", total = iterations),
    value = function (out) {
      ratchet.re <- regexec("\\r([0-9]+) +[A-Z]+ +[0-9]+ of [0-9]+ +([0-9\\.]+|-+) +([0-9\\.]+|-+) +([0-9:]+) +([0-9,]+)", out)
      prog.value <- NULL
      if(length(attr(ratchet.re[[1]], "match.length")) != 1) {
        ratchet.m <- tail(regmatches(out, ratchet.re)[[1]], -1)
        return(list(ratio = as.numeric(ratchet.m[1]) / iterations,
                    tokens = list(score = ratchet.m[3])))
      }
    })
  analysis <- list(tnt.params = tnt.params, matrix = matrix,
                   progress = progress)
  return(analysis)
}
