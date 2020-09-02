#' Perform bootstrap resampling
#'
#' @param cutoff an integer value indicating the cutoff value for
#'   frequencies.
#' @param tree_search an object inheriting class
#'   \code{"\linkS4class{NitroMethodsBase}"}.
#' @param phy a tree of class \code{phylo}. Resampling values will be
#'   calculated using this topology.
#' @param replications an integer value indicating the number of resampling
#'   replications to perform.
#' @export
NitroBootstrap <- function (cutoff = 0, tree_search = NULL, phy = NULL,
                            replications = 100) {
  objs <- ls()
  args <- as.list(environment())[objs]
  do.call("new", c("NitroBootstrap", args))
}

setMethod("initialize", signature("NitroBootstrap"),
  function (.Object, cutoff, tree_search, phy, replications, abs_freq_summary,
            freq_diff_summary, freq_slope_summary) {
    objs <- ls()
    mf <- match.call()
    m <- match(c(".Object", objs), names(mf), 0L)
    mf <- mf[m]

    args <- as.list(mf)
    if (is.numeric(args$cutoff)) {
      .Object@cutoff <- as.integer(args$cutoff)
    }
    if (is.numeric(args$replications)) {
      args$replications <- as.integer(args$replications)
    }
    do.call("callNextMethod", args)
})

setMethod("show", signature("NitroBootstrap"), function (object) {
  cat("Parameters for bootstrap resampling:\n\n")
  cat(paste("Frequency cutoff:           ", object@cutoff, "\n"))
  cat(paste("Replications:               ", object@replications, "\n"))
})

#' @rdname tnt_cmd
#' @include NitroTreeSearch-methods.R
#' @export
setMethod("tnt_cmd", signature("NitroBootstrap"), function (n) {
  paste("resample= boot from 0 replications ", n@replications,
        " cut ", n@cutoff,
        " gc frequency [ ", paste(tnt_cmd(jack@tree_search), collapse = " "),
        " ];", sep = "")
})
