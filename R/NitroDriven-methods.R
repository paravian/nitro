#' Construct a branch swapping analysis
#'
#' @importFrom methods new
#' @importFrom TreeTools PhyDatToMatrix
#' @param matrix an object of class \code{phyDat}.
#' @param replications an integer value indicating the number of replications.
#' @param hits an integer value indicating the number of times the shortest
#'   tree must be found on consecutive re-runs of the analysis before stopping.
#' @param replications an integer value indicating the number of replications.
#' @param rss,css,xss a logical value indicating whether to use random,
#'   constraint or exclusive sectorial searches.
#' @param ratchet_cycles an integer value indicating the number of cycles of
#'   the parsimony ratchet to perform per replication.
#' @param drifting_cycles an integer value indicating the number of cycles of
#'   tree drifting to perform per replication.
#' @param fusing_rounds an integer value indicating the number of rounds of
#'   fusing per replication.
#' @param consense_times an integer value indicating the number of times to
#'   consense until the consensus is stablilised.
#' @param keep_all a logical value indicating whether to retain all generated
#'   trees from each replication regardless of length. This has a different
#'   meaning when \code{hits} = 1 and when \code{hits} > 1. When
#'   \code{hits} = 1, it is trees from each of the RAS + TBR +  SS or DFT or
#'   RAT, in addition to the trees resulting from fusing those. When
#'   \code{hits} > 1, then it means the trees resulting from fusing the
#'   initial starting trees for each of starting points.
#' @templateVar isgeneric FALSE
#' @template ordered_characters-template
#' @template inactive_taxa-template
#' @template inactive_characters-template
#' @template outgroup-template
#' @template collapse-template
#' @template weighting-template
#' @template k-template
#' @template multi_k-template
#' @export
NitroDriven <- function (matrix, replications, hits = 1, rss = TRUE,
                         css = FALSE, xss = FALSE, ratchet_cycles = 0,
                         drifting_cycles = 30, fusing_rounds = 0,
                         consense_times = 0, keep_all = FALSE,
                         ordered_characters = numeric(),
                         inactive_taxa = character(),
                         inactive_characters = numeric(), outgroup = NULL,
                         collapse = 3, weighting = c("equal", "implied"),
                         k = 3, multi_k = FALSE) {
  weighting <- match.arg(weighting)
  tree_search <- new("NitroDriven", replications, hits, rss, css, xss,
                     ratchet_cycles, drifting_cycles, fusing_rounds,
                     consense_times, keep_all)
  if (weighting == "equal") {
    obj <- new("NitroEqualWeights", matrix, tree_search, ordered_characters,
               inactive_taxa, inactive_characters, collapse, outgroup)
  } else {
    obj <- new("NitroImpliedWeights", matrix, tree_search, ordered_characters,
               inactive_taxa, inactive_characters, collapse, outgroup, k,
               multi_k)
  }
  obj
}

#' @importFrom methods callNextMethod
setMethod("initialize", "NitroDriven",
  function (.Object, replications, hits, rss, css, xss, ratchet_cycles,
            drifting_cycles, fusing_rounds, consense_times, keep_all) {
    if (class(hits) == "numeric") {
      hits <- as.integer(hits)
    }
    if (class(replications) == "numeric") {
      replications <- as.integer(replications)
    }
    if (class(ratchet_cycles) == "numeric") {
      ratchet_cycles <- as.integer(ratchet_cycles)
    }
    if (class(drifting_cycles) == "numeric") {
      drifting_cycles <- as.integer(drifting_cycles)
    }
    if (class(fusing_rounds) == "numeric") {
      fusing_rounds <- as.integer(fusing_rounds)
    }
    if (class(consense_times) == "numeric") {
      consense_times <- as.integer(consense_times)
    }
    .Object <- callNextMethod(.Object, hits = hits,
      replications = replications, rss = rss, css = css, xss = xss,
      ratchet_cycles = ratchet_cycles, drifting_cycles = drifting_cycles,
      fusing_rounds = fusing_rounds, consense_times = consense_times,
      keep_all = keep_all)
    .Object
  })

setMethod("show", "NitroDriven", function (object) {
  cat("Parameters for driven analysis:\n\n")
  cat(paste("Replications:               ", object@replications, "\n"))
  cat(paste("Hits:                       ", object@hits, "\n"))
  ss <- c("Random", "Constraint", "Exclusive")
  if (any(object@rss, object@css, object@xss)) {
    cat(paste("Sectorial searches:         ",
              paste(ss[c(object@rss, object@css, object@xss)],
                    collapse = ", "), "\n"))
  }
  if (object@ratchet_cycles > 0) {
    cat(paste("Ratchet cycles:             ", object@ratchet_cycles, "\n"))
  }
  if (object@drifting_cycles > 0) {
    cat(paste("Drifting cycles:            ", object@drifting_cycles, "\n"))
  }
  if (object@fusing_rounds > 0) {
    cat(paste("Fusing rounds:              ", object@fusing_rounds, "\n"))
  }
  if (object@consense_times > 0) {
    cat(paste("Consense times:             ", object@consense_times, "\n"))
  }
  cat(paste("Keep all trees:             ", object@keep_all, "\n"))
})

#' @rdname replications
setMethod("replications", signature("NitroDriven"), function (n) n@replications)

#' @rdname replications
setMethod("replications<-", signature("NitroDriven", "numeric"), .replications_body)

#' Hits
#'
#' Function that returns or sets the number of times that shortest tree(s) of
#' the same length must found on consecutive re-runs of the analysis before
#' stopping.
#' @param n an object of class \code{NitroDriven}.
#' @return an integer value indicating the number of hits.
#' @export
#' @rdname hits
setGeneric("hits", function (n) standardGeneric("hits"))

#' @rdname hits
setMethod("hits", signature("NitroDriven"), function (n) n@hits)

#' @param value an integer indicating the number of times the shortest tree
#' must be found on consecutive re-runs of the analysis before stopping.
#' @export
#' @rdname hits
setGeneric("hits<-", function (n, value) standardGeneric("hits<-"))

.hits_body <- function (n, value) {
  if (inherits(n, "NitroImpliedWeights")) {
    n@nitro_obj@hits <- as.integer(value)
  } else if (inherits(n, "NitroBase")) {
    n@hits <- as.integer(value)
  }
  validObject(n)
  n
}

#' @rdname hits
setMethod("hits<-", signature("NitroDriven", "numeric"), .hits_body)

#' @rdname tnt_cmd
setMethod("tnt_cmd", "NitroDriven", function (n) {
  return(
    paste0("xmult= hits ", n@hits,
      " replications ", n@replications,
      ifelse(n@rss, " rss", " norss"),
      ifelse(n@css, " css", " nocss"),
      ifelse(n@xss, " xss", " noxss"),
      ifelse(n@fusing_rounds == 0, " nofuse",
             paste(" fuse ", n@fusing_rounds)),
      ifelse(n@ratchet_cycles == 0, " noratchet",
             paste(" ratchet ", n@ratchet_cycles)),
      ifelse(n@drifting_cycles == 0, " nodrift",
             paste(" drift", n@drifting_cycles)),
      ifelse(n@consense_times == 0, " noconsense",
             paste(" consense ", n@consense_times)),
      ifelse(n@keep_all, " keepall", " nokeepall"),";")
  )
})
