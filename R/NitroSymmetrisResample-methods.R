#' Perform symmetric resampling
#'
#' @param probability an integer value indicating the change probability.
#' @param cutoff an integer value indicating the cutoff value for
#'   frequencies.
#' @param tree_search an object inheriting class
#'   \code{"\linkS4class{NitroMethodsBase}"}.
#' @param phy a tree of class \code{phylo}. Resampling values will be
#'   calculated using this topology.
#' @param replications an integer value indicating the number of resampling
#'   replications to perform.
#' @export
NitroSymmetricResample <- function (probability = 36, cutoff = 0,
                                    tree_search = NULL, phy = NULL,
                                    replications = 100) {
  objs <- ls()
  args <- as.list(environment())[objs]
  do.call("new", c("NitroSymmetricResample", args))
}

setMethod("initialize", signature("NitroSymmetricResample"),
  function (.Object, probability, cutoff, tree_search, phy, replications,
            abs_freq_summary, freq_diff_summary, freq_slope_summary) {
    objs <- ls()
    mf <- match.call()
    m <- match(c(".Object", objs), names(mf), 0L)
    mf <- mf[m]

    args <- as.list(mf)
    if (is.numeric(args$probability)) {
      .Object@probability <- as.integer(args$probability)
    }
    if (is.numeric(args$cutoff)) {
      .Object@cutoff <- as.integer(args$cutoff)
    }
    if (is.numeric(args$replications)) {
      args$replications <- as.integer(args$replications)
    }
    do.call("callNextMethod", args)
})

setMethod("show", signature("NitroSymmetricResample"), function (object) {
  cat("Parameters for symmetric resampling:\n\n")
  cat(paste("Removal probability:        ", object@probability, "\n"))
  cat(paste("Frequency cutoff:           ", object@cutoff, "\n"))
  cat(paste("Replications:               ", object@replications, "\n"))
})

#' @rdname tnt_cmd
#' @export
setMethod("tnt_cmd", signature("NitroSymmetricResample"), function (n) {
  paste("resample= jak from 0 replications ", n@replications,
        " probability ", n@probability,
        " cut ", n@cutoff,
        " gc frequency [ ", paste(tnt_cmd(jack@tree_search), collapse = " "),
        " ];", sep = "")
})
