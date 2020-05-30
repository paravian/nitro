setMethod("tnt_cmd", "NitroConstraint", function (n) {
  fixed <- paste(which(n@fixed) - 1, collapse = " ")
  floating <- paste(which(n@floating) - 1, collapse = " ")
  if (nchar(floating) > 0) {
    floating <- paste(" (", floating, ")")
  }
  type <- ifelse(n@is_positive, "+", "-")
  return(paste(type, " [ ", fixed, floating, " ]", sep = ""))
})

#' Set and view fixed constraints
#'
#' Function to return or set fixed constraints to a
#' \code{\link{NitroConstraint}} object.
#' @seealso The S4 class \code{\link{NitroConstraint}}.
#' @keywords classes
#' @include check-classes.R
#' @export
#' @rdname fixed
setGeneric("fixed", function (n) standardGeneric("fixed"))

#' @rdname fixed
setMethod("fixed", c("NitroConstraint"), function (n) n@fixed)

#' @param taxa a character vector of taxa to include in the fixed constraint
#' @export
#' @rdname fixed
setGeneric("fixed<-", function (n, value) standardGeneric("fixed<-"))

#' @importFrom methods validObject
#' @rdname fixed
setMethod("fixed<-", c("NitroConstraint", "character"), function (n, value) {
  n@fixed <- value
  validObject(n)
  n
})

#' Set and view floating constraints
#'
#' Function to return or set floating constraints to a
#' \code{\link{NitroConstraint}} object.
#' @seealso The S4 class \code{\link{NitroConstraint}}.
#' @keywords classes
#' @include check-classes.R
#' @export
#' @rdname floating
setGeneric("floating", function (n) standardGeneric("floating"))

#' @rdname floating
setMethod("floating", c("NitroConstraint"), function (n) n@floating)

#' @param taxa a character vector of taxa to include in the floating constraint
#' @export
#' @rdname floating
setGeneric("floating<-", function (n, value) standardGeneric("floating<-"))

#' @importFrom methods validObject
#' @rdname floating
setMethod("floating<-", c("NitroConstraint", "character"), function (n, value) {
  n@floating <- value
  validObject(n)
  n
})
