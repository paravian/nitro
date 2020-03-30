#' Phylogenetic analysis using driven (or "New Technology") search
#'
#' @importFrom utils file_test tail
#' @param tnt.path the location of the TNT command-line binary.
#' @importFrom progress progress_bar
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
#' @param hits an integer value indicating the number of times the shortest
#'   tree must be found on consecutive re-runs of the analysis before stopping.
#' @param replications an integer value indicating the number of replications.
#' @param hold.rep an integer value inficating the maximum number of trees to
#'   retain during each replication.
#' @param rss,css,xss a logival value indicating whether to use random,
#'   constraint or exclusive sectorial searches.
#' @param ratchet.cycles an integer value indicating the number of cycles of
#'   the parsimony ratchet to perform per replication.
#' @param drifting.cycles an integer value indicating the number of cycles of
#'   tree drifting to perform per replication.
#' @param fusing.rounds an integer value indicating the number of rounds of
#'   fusing per replication.
#' @param consense.times an integer value indicating the number of times to
#'   consense until the consensus is stablilised.
#' @param multiply a logical value indicating whether to find additional trees
#'   by fusing suboptimal with optimal trees after hitting the target score.
#' @param keepall a logical value indicating whether to retain all generated
#'   trees from each replication regardless of length. This has a different
#'   meaning when \code{hits} = 1 and when \code{hits} > 1. When
#'   \code{hits} = 1, it is trees from each of the RAS + TBR +  SS or DFT or
#'   RAT, in addition to the trees resulting from fusing those. When
#'   \code{hits} > 1, then it means the trees resulting from fusing the
#'   initial starting trees for each of starting points.
#' @return a list containing the search parameters and TNT command string, a
#'   \code{phyDat} object containing the phylogenetic matrix analysed and, if
#'   \code{run.now} is \code{TRUE}, a \code{multiPhylo} object of trees found
#'   from the search commands.
#' @export
driven <- function(tnt.path, matrix, run.now=TRUE, collapse=3, hold=100,
                   outgroup=NULL, hits=1, replications=4, hold.rep=1,
                   rss=TRUE, css=FALSE, xss=FALSE, ratchet.cycles=0,
                   drifting.cycles=30, fusing.rounds=0, consense.times=0,
                   multiply=FALSE, keepall=FALSE) {
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
  if (is.logical(multiply) == FALSE & length(multiply) != 1) {
    stop("'multiply' must be a logical")
  }
  if (is.logical(keepall) == FALSE & length(keepall) != 1) {
    stop("'keepall' must be a logical")
  }

  tnt.params <- list(collapse = collapse, hold = hold, outgroup = outgroup,
                     hits = hits, replications = replications)

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
                            ifelse(keepall, "keepall", "nokeepall"),
                            ";"))

  analysis <- list(tnt.params = tnt.params, matrix = matrix)
  if (run.now) {
    # Set up progress bar and define function for determining progress
    progress <- list(
      bar = progress_bar$new(format = "Driven search: [:bar] run :run | :current/:total reps | Best score: :score", total = replications),
      value = function (out) {
        driven.re <- regexec("\\r([0-9]+) +[A-Z]+ +([0-9]+) +([0-9\\.]+|-+) +([0-9\\.]+|-+) +([0-9:]+) +([0-9,]+)", out)
        prog.value <- NULL
        if(length(attr(driven.re[[1]], "match.length")) != 1) {
          driven.m <- tail(regmatches(out, driven.re)[[1]], -1)
          return(list(ratio = (as.numeric(driven.m[2]) / replications) - 0.1,
                      tokens = list(score = driven.m[4],
                                    run = ifelse(hits > 1, as.numeric(driven.m[1]) + 1, 1))))
        }
      })
    output <- tnt(tnt.path, analysis, progress)
    analysis$trees <- tntTreeParse(output, names(matrix))
  }
  return(analysis)
}
