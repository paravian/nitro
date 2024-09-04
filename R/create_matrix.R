#' Create a new character matrix
#'
#' @description
#' Create a new discrete or continuous character-taxon matrix.
#' @param data A \code{phyDat} or a \code{data.frame} object, to create a
#'   discrete or continuous character-taxon matrix, respectively.
#' @param ... Arguments to be passed on to the R6 class responsible for creating
#'   the matrix object. See \code{\link{DiscreteMatrix}} and
#'   \code{\link{ContinuousMatrix}} for details.
#' @importFrom checkmate assert check_class check_data_frame
#'   makeAssertCollection test_class
#' @importFrom cli cli_abort
#' @seealso The \code{new} method of \code{\link{DiscreteMatrix}} and
#'   \code{\link{ContinuousMatrix}} describe optional arguments for matrix
#'   creation.
#' @export
create_matrix <- function(data, ...) {
  coll <- makeAssertCollection()
  assert(
    check_class(data, "phyDat"),
    check_data_frame(data)
  )
  val_check <- coll$getMessages()
  if (!coll$isEmpty()) {
    cli_abort(c("{.arg data} must be either a {.cls data.frame} or {.cls phyDat}.",
                "x" = val_check))
  }

  mtx_args <- list(...)
  mtx_args$data <- data

  if (test_class(data, "phyDat")) {
    new_mtx <- do.call(DiscreteMatrix$new, mtx_args)
  } else {
    new_mtx <- do.call(ContinuousMatrix$new, mtx_args)
  }
  return(new_mtx)
}
