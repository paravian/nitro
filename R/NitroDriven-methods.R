#' Definte parameters for a driven analysis
#'
#' @importFrom methods new
#' @param replications an integer value indicating the number of replications.
#' @param hits an integer value indicating the number of times the shortest
#'   tree must be found on consecutive re-runs of the analysis before stopping.
#' @param consense_times an integer value indicating the number of times to
#'   consense until the consensus is stablilised.
#' @param keep_all a logical value indicating whether to retain all generated
#'   trees from each replication regardless of length. This has a different
#'   meaning when \code{hits} = 1 and when \code{hits} > 1. When
#'   \code{hits} = 1, it is trees from each of the RAS + TBR +  SS or DFT or
#'   RAT, in addition to the trees resulting from fusing those. When
#'   \code{hits} > 1, then it means the trees resulting from fusing the
#'   initial starting trees for each of starting points.
#' @param multiply a logical value indicating whether to find additional trees
#'   by fusing suboptimal trees with optimal trees.
#' @param sectorial_search a list of objects of inheriting
#'   \code{"\linkS4class{NitroSectorialSearch}"}.
#' @param tree_fuse an object of class \code{"\linkS4class{NitroTreeFuse}"}.
#' @param tree_hybridize an object of class \code{"\linkS4class{NitroTreeHybridize}"}.
#' @param tree_drift an object of class \code{"\linkS4class{NitroTreeDrift}"}.
#' @param ratchet an object of class \code{"\linkS4class{NitroRatchet}"}.
#' @include NitroSectorialSearch-class.R
#' @include NitroTreeFuse-class.R
#' @include NitroTreeHybridize-class.R
#' @include NitroTreeDrift-class.R
#' @include NitroRatchet-class.R
#' @export
NitroDriven <- function (replications, hits = 1, consense_times = 0,
                         keep_all = FALSE, multiply = TRUE,
                         sectorial_search = NULL,  tree_fuse = NULL,
                         tree_hybridize = NULL, tree_drift = NULL,
                         ratchet = NULL) {
  if (is.null(sectorial_search)) {
    sectorial_search <- c(NitroRandomSectorialSearch(),
                          NitroConstraintSectorialSearch())
  }
  if (is.null(tree_fuse)) {
    tree_fuse <- NitroTreeFuse()
  }
  if (is.null(tree_hybridize)) {
    tree_hybridize <- NitroTreeHybridize(rounds = 0)
  }
  if (is.null(tree_drift)) {
    tree_drift <- NitroTreeDrift(iterations = 5)
  }
  if (is.null(ratchet)) {
    ratchet <- NitroRatchet(iterations = 0)
  }
  new("NitroDriven", replications = replications, hits = hits,
    consense_times = consense_times, keep_all = keep_all, multiply = multiply,
    sectorial_search = sectorial_search, tree_fuse = tree_fuse,
    tree_hybridize = tree_hybridize, tree_drift = tree_drift,
    ratchet = ratchet)
}

#' @importFrom methods callNextMethod
setMethod("initialize", "NitroDriven",
  function (.Object, replications, hits, consense_times, keep_all, multiply,
            sectorial_search, tree_fuse, tree_hybridize, tree_drift, ratchet) {
    if (class(hits) == "numeric") {
      hits <- as.integer(hits)
    }
    if (class(replications) == "numeric") {
      replications <- as.integer(replications)
    }
    if (class(consense_times) == "numeric") {
      consense_times <- as.integer(consense_times)
    }
    .Object <- callNextMethod(.Object, replications = replications,
      hits = hits, consense_times = consense_times,
      keep_all = keep_all, multiply = multiply,
      sectorial_search = sectorial_search, tree_fuse = tree_fuse,
      tree_hybridize = tree_hybridize, tree_drift = tree_drift,
      ratchet = ratchet)
    .Object
  })

setMethod("show", "NitroDriven", function (object) {
  cat("Parameters for driven analysis:\n\n")
  cat(paste("Replications:               ", object@replications, "\n"))
  cat(paste("Hits:                       ", object@hits, "\n"))
  if (object@consense_times > 0) {
    cat(paste("Consense times:             ", object@consense_times, "\n"))
  }
  cat(paste("Keep all trees:             ", object@keep_all, "\n"))
  cat(paste("Multiply trees by fusing:   ", object@multiply, "\n"))
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
  n@hits <- as.integer(value)
  validObject(n)
  n
}

#' @rdname hits
setMethod("hits<-", signature("NitroDriven", "numeric"), .hits_body)

#' @rdname tnt_cmd
setMethod("tnt_cmd", "NitroDriven", function (n) {
  driven_cmd <- c()
  if (length(n@sectorial_search)) {
    driven_cmd <- c(driven_cmd, sapply(n@sectorial_search, tnt_cmd))
  }
  if (n@tree_fuse@rounds > 0) {
    driven_cmd <- c(driven_cmd, tnt_cmd(n@tree_fuse))
  }
  if (n@tree_hybridize@rounds > 0) {
    driven_cmd <- c(driven_cmd, tnt_cmd(n@tree_hybridize))
  }
  if (n@tree_drift@iterations > 0) {
    driven_cmd <- c(driven_cmd, tnt_cmd(n@tree_drift))
  }
  if (n@ratchet@iterations > 0) {
    driven_cmd <- c(driven_cmd, tnt_cmd(n@ratchet))
  }
  sect_classes <- sapply(n@sectorial_search, class)
  driven_cmd <- c(driven_cmd,
    paste0("xmult= hits ", n@hits,
      " replications ", n@replications,
      ifelse("NitroRandomSectorialSearch" %in% sect_classes,
             " rss", " norss"),
      ifelse("NitroConstraintSectorialSearch" %in% sect_classes,
             " css", " nocss"),
      ifelse(n@tree_fuse@rounds == 0, " nofuse",
             paste(" fuse", n@tree_fuse@rounds)),
      ifelse(n@tree_hybridize@rounds == 0, " nohybrid",
             " hybrid"),
      ifelse(n@tree_drift@iterations == 0, " nodrift",
             paste(" drift", n@tree_drift@iterations)),
      ifelse(n@ratchet@iterations == 0, " noratchet",
             paste(" ratchet", n@ratchet@iterations)),
      ifelse(n@consense_times == 0, " noconsense",
             paste(" consense", n@consense_times)),
      ifelse(n@keep_all, " keepall", " nokeepall"),
      ifelse(n@multiply, " multiply", " nomultiply"), ";"))
  # ifelse(n@xss, " xss", " noxss"),
  return(driven_cmd)
})
