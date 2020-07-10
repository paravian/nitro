#' Define parameters for a branch swapping analysis
#'
#' @importFrom methods new
#' @param replications an integer value indicating the number of replications.
#' @param hold_rep an integer value indicating the maximum number of trees to
#'   retain during each replication.
#' @param keep_all a logical value indicating whether to retain all generated
#'   trees from each replication regardless of length.
#' @export
NitroBranchSwap <- function (replications, hold_rep, keep_all = FALSE) {
  new("NitroBranchSwap", replications, hold_rep, keep_all)
}

#' @importFrom methods callNextMethod
setMethod("initialize", "NitroBranchSwap",
  function (.Object, replications, hold_rep, keep_all) {
    if (class(replications) == "numeric") {
       replications <- as.integer(replications)
    }
    if (class(hold_rep) == "numeric") {
       hold_rep <- as.integer(hold_rep)
    }
    objs <- ls()
    for (obj in objs) {
      slot(.Object, obj) <- get(obj)
    }
    .Object <- callNextMethod(.Object)
    .Object
})

#' Replications
#'
#' Function to return or set the number of replications for a branch swapping
#' or driven phylogenetic analysis
#' @param n an object of either \code{NitroBranchSwap} or \code{NitroDriven}.
#' @return a numeric vector indicating the number of replications for the
#' analysis.
#' @export
#' @rdname replications
setGeneric("replications", function (n) standardGeneric("replications"))

#' @rdname replications
setMethod("replications", signature("NitroBranchSwap"), function (n) n@replications)

#' @param value a numeric vector indicating the number of replications for the
#' analysis.
#' @export
#' @rdname replications
setGeneric("replications<-", function (n, value) standardGeneric("replications<-"))

.replications_body <- function (n, value) {
   n@replications <- as.integer(value)
   validObject(n)
   n
}

#' @rdname replications
setMethod("replications<-", signature("NitroBranchSwap", "numeric"), .replications_body)

#' @rdname tnt_cmd
#' @include NitroTreeSearch-methods.R
setMethod("tnt_cmd", "NitroBranchSwap", function (n) {
   return(paste0("mult= replic ", n@replications, " hold ", n@hold_rep,
                 ifelse(n@keep_all, " ", " no"), "keepall;"))
})

setMethod("show", "NitroBranchSwap", function (object) {
   cat("Parameters for branch swapping:\n\n")
   cat(paste("RAS replications:           ", object@replications, "\n"))
   cat(paste("Trees to hold per replicate:", object@hold_rep, "\n"))
   cat(paste("Keep all trees:             ", object@keep_all, "\n"))
})
