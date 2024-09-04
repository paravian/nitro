#' Create a new search configuration
#'
#' @description
#' Create a new search configuration object for use in a tree analysis.
#' @param name The name of a search method. Partial unambiguous matches are also
#'   accepted. The options are:
#'   \itemize{
#'     \item \code{branch_breaking}: branch swapping on a set of trees. Creates
#'       a \code{\link{BranchBreakingOptions}} object;
#'     \item \code{branch_swapping}: branch swapping from a starting
#'       tree. Creates a \code{\link{BranchSwappingOptions}} object;
#'     \item \code{driven_search}: search using targets for completion. Creates
#'       a \code{\link{DrivenSearchOptions}} object;
#'     \item \code{ratchet}: search using parsimony ratchet. Creates a
#'       \code{\link{RatchetOptions}} object.
#'   }
#' @param ... Arguments to be passed on to the R6 class responsible for creating
#'   the search method object.
#' @importFrom checkmate check_string
#' @importFrom cli cli_abort
#' @seealso The \code{new} method of \code{\link{BranchBreakingOptions}},
#'   \code{\link{BranchSwappingOptions}}, \code{\link{DrivenSearchOptions}} and
#'   \code{\link{RatchetOptions}} describe optional arguments for search method
#'   creation.
#' @export
new_search_config <- function(name, ...) {
  val_check <- check_string(name, min.chars = 1)
  if (!test_true(val_check)) {
    cli_abort(c("{.arg name} must be a string.",
                x = val_check))
  }

  object_choice <- c(
    "Branch_Breaking", "Branch_Swapping", "Driven_Search", "Ratchet"
  )

  search_obj <- create_new_object(name, object_choice, ...)
  return(search_obj)
}
