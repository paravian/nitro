setMethod("initialize", signature("NitroResampleBase"),
          function (.Object, ..., tree_search, phy, replications,
                    abs_freq_summary, freq_diff_summary, freq_slope_summary) {
  objs <- ls()
  mf <- match.call()
  m <- match(c(".Object", objs), names(mf), 0L)
  mf <- mf[m]

  args <- as.list(mf)
  do.call("callNextMethod", args)
})

#' @rdname replications
setMethod("replications", signature("NitroResampleBase"), function (n) n@replications)
