#' Phylogenetic analysis using driven (or "New Technology") search
#'
#' @param tnt.path The location of the TNT command-line binary.
#' @param matrix A \code{phyDat} object of the matrix.
#' @param run.now Logical; perform a phylogenetic analysis straight away or save
#'   commands for use in other methods
#' @param hold The maximum number of trees to allow TNT to hold in memory.
#' @param outgroup The outgroup taxon for the phylogenetic analysis. By default,
#'   the first taxon in the matrix is considered the outgroup.
#' @param hits The number of times to reach the shortest tree for each
#'   replication.
#' @param replications The number of replications.
#' @param rss,css,xss Use random, constraint or exclusive sectorial searches.
#' @param ratchet.cycles The number of cycles of the parsimony ratchet.
#' @param drifting.cycles The number of cycles of tree drifting.
#' @param fusing.rounds The number of rounds of fusing.
#' @param consense.times Number of times to consense until the consensus is
#'   stablilised.
#' @param multiply After hitting target score, find additional trees by fusing
#'   suboptimal with optimal trees
#' @param hold.rep The maximum number of trees to retain during each
#'   replication.
#' @param keepall Retain all generated trees from each replication regardless of
#'   length. This has a different meaning when \code{hits} = 1 and when
#'   \code{hits} > 1. When \code{hits} = 1, it is trees from each of the RAS +
#'   TBR +  SS or DFT or RAT, in addition to the trees resulting from fusing
#'   those.  When \code{hits} > 1, then it means the trees resulting from fusing
#'   the initial starting trees for each of starting points.
#' @return A list containing the search parameters and TNT command string and,
#'   if \code{run.now} is \code{TRUE}, a \code{multiPhylo} object of trees
#'   found from the search commands.
#' @export
driven <- function(tnt.path, matrix, run.now=TRUE, hold=100, outgroup=NULL,
                   hits=1, replications=4, hold.rep=1, autoconstrain="wagner",
                   rss=TRUE, css=FALSE, xss=FALSE, ratchet.cycles=0,
                   drifting.cycles=30, hybridization=FALSE, fusing.rounds=0,
                   consense.times=0, multiply=FALSE, keepall=FALSE) {
  if (file_test("-f", tnt.path) == FALSE) {
    stop("'tnt.path' does not exist")
  }
  if (class(matrix) != "phyDat") {
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
  if ((is.null(outgroup) == TRUE | is.integer(outgroup) == TRUE) == FALSE &
      length(outgroup) != 1) {
    stop("'outgroup' must be a character")
  }
  if(is.numeric(hits) == FALSE | length(hits) != 1) {
    stop("'hits' must be an integer")
  } else if (hits %% 1 != 0 | hits <= 0) {
    stop("'hits' must be an integer > 0")
  }

  if (any(replications %% 1 != 0) & length(replications) != 1) {
    stop("'replications' must be an integer")
  } else if (replications <= 0) {
    stop("'hold' must be > 0")
  }
  if (is.logical(rss) == FALSE & length(rss) != 1) {
    stop("'rss' must be a logical")
  }
  if (is.logical(css) == FALSE & length(css) != 1) {
    stop("'css' must be a logical")
  }
  if (is.logical(xss) == FALSE & length(xss) != 1) {
    stop("'xss' must be a logical")
  }
  if (is.numeric(ratchet.cycles) == FALSE & length(ratchet.cycles) != 1) {
    stop("'ratchet.cycles' must be an integer")
  }  else if (ratchet.cycles %% 1 != 0 | ratchet.cycles < 0) {
    stop("'ratchet.cycles' must be an integer >= 0")
  }
  if (is.numeric(drifting.cycles) == FALSE & length(ratchet.cycles) != 1) {
    stop("'drifting.cycles' must be an integer")
  } else if (drifting.cycles %% 1 != 0 | drifting.cycles < 0) {
    stop("'drifting.cycles' must be an integer >= 0")
  }
  if (is.numeric(fusing.rounds) == FALSE & length(fusing.rounds) != 1) {
    stop("'fusing.rounds' must be an integer")
  } else if (fusing.rounds < 0) {
    stop("'fusing.rounds' must be >= 0")
  }
  if (is.numeric(consense.times) == FALSE & length(consense.times) != 1) {
    stop("'consense.times' must be an integer")
  }  else if (consense.times < 0) {
    stop("'consense.times' must be >= 0")
  }
  if (is.logical(hybridization) == FALSE & length(hybridization) != 1) {
    stop("'hybridizaation' must be a logical")
  }
  if (is.logical(multiply) == FALSE & length(multiply) != 1) {
    stop("'multiply' must be a logical")
  }
  if (is.logical(keepall) == FALSE & length(keepall) != 1) {
    stop("'keepall' must be a logical")
  }

  tnt.params <- list(hold = hold, outgroup = outgroup, hits = hits,
                     replications = replications)

  tnt.params$cmd <- c(paste("xmult=", "hits", hits, "replications",
                            replications, "hold", hold.rep,
                            ifelse(rss, "rss", "norss"),
                            ifelse(css, "css", "nocss"),
                            ifelse(xss, "xss", "noxss"),
                            ifelse(fusing.rounds == 0, "nofuse",
                                   paste("fuse", fusing.rounds)),
                            ifelse(ratchet.cycles == 0, "noratchet",
                                   paste("ratchet", ratchet.cycles)),
                            ifelse(drifting.cycles == 0, "nodrift",
                                   paste("drift", drifting.cycles)),
                            ifelse(consense.times == 0, "noconsense",
                                   paste("consense", consense.times)),
                            ifelse(multiply, "multiply", "nomultiply"),
                            ifelse(hybridization, "hybrid", "nohybrid"),
                            ifelse(keepall, "keepall", "nokeepall"),
                            ";"))

  if (run.now) {
    output <- runTnt(tnt.path, matrix, tnt.params)
    trees <- tntTreeParse(output, names(matrix))
    return(list(tnt.params = tnt.params, trees = trees))
  } else {
    return(list(tnt.params = tnt.params))
  }
}
