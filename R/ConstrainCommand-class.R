#' Constrain Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures whether TNT enforces
#' topological constraints during tree searches in \pkg{nitro}.
#'
#' This command wraps the TNT `constrain` command, which must be enabled
#' after constraints have been defined via [TopologicalConstraintsCommand]
#' and before the tree search begins. It is enqueued automatically by
#' [TopologicalConstraintsCommand].
#'
#' @details
#' ## Command output
#' `$render()` produces `constrain =;` when `$enable` is `TRUE`, or
#' `constrain -;` when `$enable` is `FALSE`.
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `402`, which places it after [TopologicalConstraintsCommand] (priority
#' `401`) so that constraints are defined before enforcement is activated.
#'
#' @seealso
#' * [TopologicalConstraintsCommand] — defines the constraints that this
#'   command activates.
#' * [set_constraint()] — enqueues both this command and
#'   [TopologicalConstraintsCommand] automatically.
#'
#' @keywords internal
#' @importFrom checkmate check_flag test_true
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
ConstrainCommand <- R6Class(
  "ConstrainCommand",
  inherit = BasicCommand,
  private = list(
    .enable = NULL
  ),
  active = list(
    #' @field enable \[`logical(1)`\]\cr
    #'   Whether to enforce topological constraints during tree searches.
    #'   `TRUE` produces `constrain =;`; `FALSE` produces `constrain -;`.
    enable = function(value) {
      if (missing(value)) {
        return(private$.enable)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg enable} must be a logical flag.",
            "x" = val_check
          ))
        }
        private$.enable <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Adds this command at priority `402`, after
    #' [TopologicalConstraintsCommand] (priority `401`).
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(self, 402)
      .queue
    },
    #' @description
    #' Create a new `ConstrainCommand` object.
    #'
    #' @param enable \[`logical(1)`\]\cr
    #'   Whether to enforce topological constraints during tree searches.
    #'   See the `$enable` field.
    #'
    #' @return A new `ConstrainCommand` object.
    initialize = function(enable = TRUE) {
      a <- as.list(environment(), all = TRUE)

      super$initialize(
        name        = "constrain",
        description = "Enforce topological constraints during tree searches"
      )

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Render the TNT `constrain` command string.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT
    #'   command.
    render = function(...) {
      enable_arg <- ifelse(self$enable, "=", "-")
      paste0(self$name, " ", enable_arg, ";")
    }
  )
)
