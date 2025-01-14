#' Define options common to resampling analyses
#'
#' @description
#' \code{AbstractSupportOptions} is an R6 class that defines options common
#'   to resampling analyses.
#' @importFrom checkmate asInt check_character check_int check_multi_class
#'   check_subset test_true
#' @importFrom cli cli_abort cli_text
#' @importFrom R6 R6Class
AbstractSupportOptions <- R6Class("AbstractSupportOptions",
  inherit = AbstractModule,
  private = list(
    .name = "Support"
  ),
  public = list(
    #' @param .queue A \code{\link{QueueGenerator}} object.
    queue = function (.queue = NULL) {
      queue <- super$queue(.queue)

      make_new_command("ttags", "set-storing-tags", "generic", arguments = "=", queue = queue)
      make_new_command("ttags", "set-not-storing-tags", "generic", arguments = ")", queue = queue)
      make_new_command("unique", "keep-unique-trees", "generic", queue = queue)
      make_new_command("ttags", "show-tags", "output", arguments = "/", queue = queue)
    }
  )
)
