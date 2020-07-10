#' @importFrom methods callNextMethod slot<-
setMethod("initialize", "NitroSectorialSearch",
  function (.Object, buffer = buffer, slack = slack) {
    if (class(slack) == "numeric") {
      slack <- as.integer(slack)
    }
    objs <- ls()
    for (obj in objs) {
      slot(.Object, obj) <- get(obj)
    }
    .Object <- callNextMethod(.Object)
    .Object
})
