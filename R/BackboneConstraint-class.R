#' Backbone Constraint
#'
#' @description
#' An [R6][R6::R6Class] class that configures a backbone constraint for
#' use in constrained tree searches in \pkg{nitro}.
#'
#' A backbone constraint requires (positive) or forbids (negative) the
#' recovered trees from being compatible with a reference topology. The
#' reference tree is supplied to `execute_analysis()` via the `reference_tree`
#' argument, which is resolved automatically when the constraint is used
#' within a [TreeAnalysis] via [set_constraint()].
#'
#' @details
#' ## Default values
#' | Parameter     | Default |
#' |---------------|---------|
#' | `is_positive` | `TRUE`  |
#'
#' @seealso
#' * [MonophylyConstraint] — constrains a named set of taxa to be
#'   monophyletic.
#' * [AbstractConstraint] — parent class defining `$is_positive`.
#' * [TopologicalConstraintsCommand] — holds and renders one or more
#'   constraints as a TNT `force` command.
#' * [set_constraint()] — recommended way to attach constraints to a
#'   [TreeAnalysis].
#'
#' @examples
#' # Positive backbone constraint (default)
#' bc <- BackboneConstraint$new()
#'
#' # Negative backbone constraint
#' bc_neg <- BackboneConstraint$new(is_positive = FALSE)
#'
#' @importFrom checkmate check_class check_null test_null test_true
#' @importFrom cli cli_abort cli_text col_grey style_italic col_red
#' @importFrom R6 R6Class
#' @export
BackboneConstraint <- R6Class(
  "BackboneConstraint",
  inherit = AbstractConstraint,
  public = list(
    #' @description
    #' Format the constraint as a summary table.
    #'
    #' @param ... Not used.
    #'
    #' @return A `data.frame` with columns for description and value.
    format = function(...) {
      options <- data.frame(
        c("Constraint type:"),
        c(ifelse(self$is_positive, "positive", "negative"))
      )

      names(options) <- c("", "Value")
      options[, 1] <- format(options[, 1], justify = "left")
      options
    },
    #' @description
    #' Create a new `BackboneConstraint` object.
    #'
    #' @param is_positive \[`logical(1)`\]\cr
    #'   Whether the constraint is positive (default: `TRUE`). See the
    #'   `$is_positive` field.
    #'
    #' @return A new `BackboneConstraint` object.
    initialize = function(is_positive = TRUE) {
      super$initialize(is_positive = is_positive)
    },
    #' @description
    #' Print a brief summary of the constraint.
    #'
    #' @param ... Not used.
    print = function(...) {
      cli_text(col_grey(
        "# A ", style_italic(col_red("nitro")),
        " backbone constraint"
      ))
      options <- format(self)
      names(options) <- NULL
      print(options, row.names = FALSE)
    }
  )
)
