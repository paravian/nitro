#' Define an implicit enumeration analysis
#'
#' @importFrom methods new
#' @export
NitroImplicitEnum <- function () {
  new("NitroImplicitEnum")
}

#' @rdname tnt_cmd
setMethod("tnt_cmd", "NitroImplicitEnum", function (n) {
  return("ienum;")
})
