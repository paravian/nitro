#' Tree Drifting Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures a tree drifting analysis in
#' \pkg{nitro}.
#'
#' Tree drifting accepts suboptimal trees with a probability that decreases
#' as the fit difference grows, allowing the search to escape local optima
#' without the aggressive perturbation of the ratchet. This wraps the TNT
#' `drift` command.
#'
#' @details
#' ## Default values
#' | Parameter              | Default        |
#' |------------------------|----------------|
#' | `iterations`           | `30`           |
#' | `substitutions`        | `60`           |
#' | `max_abs_fit_diff`     | `1`            |
#' | `max_rel_fit_diff`     | `0.2`          |
#' | `reject_factor`        | `3`            |
#' | `autoconstrain_cycles` | `0` (disabled) |
#' | `set_only`             | `FALSE`        |
#'
#' ## Command output
#' `$render()` produces a string of the form
#' `drift= iterations {iterations} numsubs {substitutions} fitdiff {max_abs_fit_diff} ...;`
#'
#' ## Acceptance criteria
#' Suboptimal trees are accepted based on three parameters:
#' * `$max_abs_fit_diff` — maximum absolute difference in fit score.
#' * `$max_rel_fit_diff` — maximum relative difference in fit score.
#' * `$reject_factor` — controls how aggressively suboptimal trees are
#'   rejected. Higher values make acceptance less likely.
#'
#' ## Disabling within ExtraSearchMethodsCommand
#' When used within [ExtraSearchMethodsCommand], setting `$iterations` to
#' `0` disables drifting without removing it from the configuration.
#'
#' ## Set-only mode and queue integration
#' When `$set_only` is `TRUE`, the command configures parameters without
#' triggering a search. This is set automatically when used within
#' [ExtraSearchMethodsCommand]. Calling `$enqueue()` adds this command to
#' a [CommandQueue] and, when `$set_only` is `FALSE`, also enqueues a
#' [CollapseTreesCommand] and a [TreePlottingCommand].
#'
#' @seealso
#' * [ExtraSearchMethodsCommand] — orchestrates multiple search strategies
#'   including tree drifting.
#' * [set_tree_search()] — recommended way to add this command to a
#'   [TreeAnalysis].
#'
#' @references
#' Goloboff, P. A., Farris, J. S., & Nixon, K. C. (2008). TNT, a free
#' program for phylogenetic analysis. *Cladistics*, 24(5), 774--786.
#'
#' @examples
#' # Create with default settings
#' td <- TreeDriftingCommand$new()
#'
#' # Increase iterations and loosen acceptance
#' td$iterations <- 50
#' td$max_abs_fit_diff <- 2
#' td$reject_factor <- 5
#'
#' # Enable autoconstraining
#' td$autoconstrain_cycles <- 3
#'
#' # Generate the TNT command
#' td$render()
#'
#' @importFrom checkmate asInt check_int check_flag check_number test_true
#' @importFrom cli cli_abort cli_text col_grey col_red style_italic
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @export
TreeDriftingCommand <- R6Class(
  "TreeDriftingCommand",
  inherit = TreeSearchCommand,
  active = list(
    #' @field iterations \[`integer(1)`\]\cr
    #'   The number of tree drifting cycles to perform. Must be a
    #'   non-negative integer. Set to `0` to disable drifting when used
    #'   within [ExtraSearchMethodsCommand].
    iterations = function(value) {
      label <- "iterations"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg iterations} must be an integer",
            "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field substitutions \[`integer(1)`\]\cr
    #'   The number of accepted tree rearrangements (replacements) to
    #'   perform in the perturbation phase. Must be a positive integer.
    substitutions = function(value) {
      label <- "substitutions"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg substitutions} must be an integer",
            "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field max_abs_fit_diff \[`numeric(1)`\]\cr
    #'   The maximum absolute fit difference allowed when accepting
    #'   suboptimal trees. Must be a non-negative number.
    max_abs_fit_diff = function(value) {
      label <- "max_abs_fit_diff"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_number(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg max_abs_fit_diff} must be a number.",
            "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field max_rel_fit_diff \[`numeric(1)`\]\cr
    #'   The maximum relative fit difference allowed when accepting
    #'   suboptimal trees. Must be a non-negative number.
    max_rel_fit_diff = function(value) {
      label <- "max_rel_fit_diff"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_number(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg max_rel_fit_diff} must be a number.",
            "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field reject_factor \[`numeric(1)`\]\cr
    #'   The rejection factor controlling how aggressively suboptimal trees
    #'   are rejected. Higher values make acceptance less likely. Must be a
    #'   non-negative number.
    reject_factor = function(value) {
      label <- "reject_factor"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_number(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg reject_factor} must be a number.",
            "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field autoconstrain_cycles \[`integer(1)`\]\cr
    #'   The number of autoconstrained cycles to perform. Set to `0`
    #'   (default) to disable autoconstraining. Must be a non-negative
    #'   integer.
    autoconstrain_cycles = function(value) {
      label <- "autoconstrain_cycles"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg autoconstrain_cycles} must be an integer",
            "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `TreeDriftingCommand` object.
    #'
    #' @param iterations \[`integer(1)`\]\cr
    #'   Number of drifting cycles (default: `30`). See the `$iterations`
    #'   field.
    #' @param substitutions \[`integer(1)`\]\cr
    #'   Number of accepted rearrangements per perturbation phase (default:
    #'   `60`). See the `$substitutions` field.
    #' @param max_abs_fit_diff \[`numeric(1)`\]\cr
    #'   Maximum absolute fit difference for accepting suboptimal trees
    #'   (default: `1`). See the `$max_abs_fit_diff` field.
    #' @param max_rel_fit_diff \[`numeric(1)`\]\cr
    #'   Maximum relative fit difference for accepting suboptimal trees
    #'   (default: `0.2`). See the `$max_rel_fit_diff` field.
    #' @param reject_factor \[`numeric(1)`\]\cr
    #'   Rejection aggressiveness factor (default: `3`). See the
    #'   `$reject_factor` field.
    #' @param autoconstrain_cycles \[`integer(1)`\]\cr
    #'   Number of autoconstrained cycles; `0` disables autoconstraining
    #'   (default: `0`). See the `$autoconstrain_cycles` field.
    #' @param set_only \[`logical(1)`\]\cr
    #'   Configure-only mode (default: `FALSE`). See the `$set_only` field.
    #'
    #' @return A new `TreeDriftingCommand` object.
    initialize = function(iterations, substitutions, max_abs_fit_diff,
                          max_rel_fit_diff, reject_factor,
                          autoconstrain_cycles, set_only = FALSE) {
      super$initialize(
        name = "drift",
        description = "Tree drifting using existing trees",
        set_only = set_only
      )

      self$new_argument("iterations", "Iterations", "iterations {value}", 30)
      self$new_argument("substitutions", "Substitutions", "numsubs {value}", 60)
      self$new_argument("max_abs_fit_diff", "Maximum absolute fit difference", "fitdiff {value}", 1)
      self$new_argument("max_rel_fit_diff", "Maximum relative fit difference", "rfitdiff {value}", 0.2)
      self$new_argument("reject_factor", "Rejection factor", "xfactor {value}", 3)

      autoconst_cmd_fmt <- function(value) {
        ifelse(value == 0, "noautoconst", glue("autoconst {value}"))
      }
      self$new_argument("autoconstrain_cycles", "Autoconstrain cycles", autoconst_cmd_fmt, 0)

      all_labels <- sapply(private$.arguments, getElement, "label")
      self$template <- paste("{", all_labels, "}", sep = "")

      validate_topology <- function(value) {
        if (!test_null(value)) {
          val_check <- check_class(value, "ReadTreesCommand")

          if (!test_true(val_check)) {
            cli_abort(c("{.arg value} must be a {.cls ReadTreesCommand} object"))
          }
        }

        value
      }

      self$new_dependency("starting trees", TRUE, validate_topology)

      for (argument in private$.arguments) {
        arg_val <- try(get(argument$label), silent = TRUE)
        if (test_class(arg_val, "try-error")) {
          arg_val <- argument$default_value
        }
        self[[argument$label]] <- arg_val
      }
    }
  )
)
