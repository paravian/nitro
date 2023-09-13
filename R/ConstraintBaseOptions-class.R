#' Set constraints on monophyly
#'
#' @description
#' \code{ConstraintBaseOptions} is an R6 class that sets common options for tree
#'   searches with constraints.
#' @importFrom checkmate assert check_flag
#' @importFrom cli cli_abort cli_text
#' @importFrom glue glue
#' @importFrom R6 R6Class
ConstraintBaseOptions <- R6Class("ConstraintBaseOptions",
  private = list(
    .is_positive = NULL
  ),
  active = list(
    #' @field is_positive A logical value indicating whether the constraint is
    #'   positive (\code{TRUE}) or negative (\code{FALSE}).
    is_positive = function (value) {
      if (missing(value)) {
        return(private$.is_positive)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg is_positive} must be a logical value.",
                      "x" = val_check))
        }

        private$.is_positive <- value
      }
    }
  ),
  public = list(
    #' @param is_positive A logical value indicating whether the constraint is
    #'   positive (\code{TRUE}) or negative (\code{FALSE}).
    initialize = function (is_positive = TRUE) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    }
  )
)

