#' Abstract Constraint Base Class
#'
#' @description
#' An [R6][R6::R6Class] class that serves as the common base for all
#' topological constraint types in \pkg{nitro}.
#'
#' `AbstractConstraint` defines the `$is_positive` field shared by all
#' constraint types. A positive constraint requires that the specified
#' group appears in the recovered trees; a negative constraint requires
#' that it does not. It is not intended to be instantiated directly.
#'
#' @details
#' ## Subclassing
#' Subclasses should call `super$initialize(is_positive)` in their own
#' `initialize()` method and define any additional fields and validation
#' logic required by the specific constraint type.
#'
#' The two concrete subclasses are:
#' * [MonophylyConstraint] — constrains a named set of taxa to be
#'   monophyletic (or non-monophyletic).
#' * [BackboneConstraint] — constrains the search to recover a topology
#'   compatible with a reference tree.
#'
#' @seealso
#' * [MonophylyConstraint] — monophyly constraint.
#' * [BackboneConstraint] — backbone constraint.
#' * [TopologicalConstraintsCommand] — the command object that holds one
#'   or more constraints and renders the TNT `force` command.
#' * [set_constraint()] — recommended way to attach constraints to a
#'   [TreeAnalysis].
#'
#' @keywords internal
#' @importFrom checkmate check_flag test_true
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
AbstractConstraint <- R6Class(
  "AbstractConstraint",
  private = list(
    .is_positive = NULL
  ),
  active = list(
    #' @field is_positive \[`logical(1)`\]\cr
    #'   Whether the constraint is positive (`TRUE`, default) or negative
    #'   (`FALSE`). A positive constraint requires the specified group to
    #'   appear in recovered trees; a negative constraint requires that it
    #'   does not.
    is_positive = function(value) {
      if (missing(value)) {
        return(private$.is_positive)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg is_positive} must be a logical value.",
            "x" = val_check
          ))
        }
        private$.is_positive <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `AbstractConstraint` object.
    #'
    #' @param is_positive \[`logical(1)`\]\cr
    #'   Whether the constraint is positive or negative (default: `TRUE`).
    #'   See the `$is_positive` field.
    #'
    #' @return A new `AbstractConstraint` object.
    initialize = function(is_positive = TRUE) {
      a <- as.list(environment(), all = TRUE)

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    }
  )
)
