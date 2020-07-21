#' @importFrom methods callNextMethod slot<-
setMethod("initialize", "NitroSectorialSearch",
  function (.Object, ..., buffer = TRUE, slack = 0) {
  objs <- ls()
  mf <- match.call()
  m <- match(c(".Object", objs), names(mf), 0L)
  mf <- mf[m]

  args <- as.list(mf)
  if (class(args$slack) == "numeric") {
    args$slack <- as.integer(args$slack)
  }
  do.call("callNextMethod", args)
})
