#' Phylogenetic analysis using branch swapping
#'
#' @param tnt.path The location of the TNT command-line binary.
#' @param matrix A \code{phyDat} object of the matrix.
#' @param hold The maximum number of trees to allow TNT to hold in memory (typically \code{replications} * \code{hold.rep}).
#' @param outgroup The outgroup taxon for the phylogenetic analysis. By default, the first taxon in the matrix is considered the outgroup.
#' @param replications The number of branch swapping replications.
#' @param hold.rep The maximum number of trees to retain during each replication.
#' @param keepall Retain all generated trees from each replication regardless of length.
#' @return A \code{multiPhylo} object.
#' @export
branchswap <- function (tnt.path, matrix, hold=100, outgroup=NULL, replications=10, hold.rep=10, keepall=FALSE) {
  tnt.cmds <- c(paste("hold", hold, ";"))

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
  if ((is.null(outgroup) == TRUE | is.character(outgroup) == TRUE) == FALSE & length(outgroup) != 1) {
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

  if (!is.null(outgroup)) {
    tnt.cmds <- c(tnt.cmds, paste("outgroup", outgroup, ";"))
  }

  tnt.cmds <- c(tnt.cmds, c(paste("mult= replic", replications, "hold", hold, paste0(ifelse(keepall, "", "no"), "keepall"), ";")))
  output <- runTnt(tnt.path, matrix, tnt.cmds)
  trees <- tntTeeParse(output, rownames(matrix))
  return(trees)
}
