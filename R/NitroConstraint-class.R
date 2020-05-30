#' Define constrained tree search
#'
#' \code{NitroConstraintGroup} is an S4 class that defines a constrainted
#' tree search.
#' @name NitroConstraintGroup-class
#' @docType class
#' @seealso The inheriting S4 class \code{\link{NitroConstraintsBase}} and
#' \code{\link{NitroUnconstrainedSearch}}.
#' @keywords classes
#' @include check-classes.R
setClass("NitroConstraint",
  slots = c(
    is_positive = "logical",
    fixed = "logical",
    floating = "logical"
))

setValidity("NitroConstraint", check_NitroConstraint)
