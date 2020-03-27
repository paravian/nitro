#' Phylogenetic analysis using branch swapping
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
#' @param replications an integer value indicating the number of replications.
#' @param hold.rep an integer value inficating the maximum number of trees to
#'   retain during each replication.
#' @param keepall a logical value indicating whether to retain all generated
#'   trees from each replication regardless of length.
#' @return a list containing the search parameters and TNT command string, a
#'   \code{phyDat} object containing the phylogenetic matrix analysed and, if
#'   \code{run.now} is \code{TRUE}, a \code{multiPhylo} object of trees found
#'   from the search commands.
#' @export
branchswap <- function (tnt.path, matrix, run.now = TRUE, collapse=3, hold=100,
                        outgroup=NULL, replications=10, hold.rep=10,
                        keepall=FALSE) {
  # Validate command arguments
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
  if ((is.null(outgroup) == TRUE | is.character(outgroup) == TRUE) == FALSE &
      length(outgroup) != 1) {
    stop("'outgroup' must be a character")
  }
  if (is.numeric(replications) == FALSE | length(replications) != 1) {
    stop("'replications' must be an integer")
  } else if (replications %% 1 != 0 | replications <= 0) {
    stop("'replications' must be an integer > 0")
  }
  if (is.numeric(hold.rep) == FALSE | length(hold.rep) != 1) {
    stop("'hold.rep' must be an integer")
  } else if (hold.rep %% 1 != 0 | hold.rep <= 0) {
    stop("'hold.rep' must be an integer > 0")
  }
  if (is.logical(keepall) == FALSE & length(keepall) != 1) {
    stop("'keepall' must be logical")
  }

  tnt.params <- list(collapse = collapse, hold = hold, outgroup = outgroup,
                     replications = replications)

  tnt.params$cmd <- c(paste("mult= replic", replications, "hold", hold.rep,
                            ifelse(keepall, "keepall", "nokeepall"), ";"))

  analysis <- list(tnt.params = tnt.params, matrix = matrix)
  if (run.now) {
    output <- runTnt(tnt.path, analysis)
    analysis$trees <- tntTreeParse(output, names(matrix))
  }
  return(analysis)
}
