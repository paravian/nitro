#' Tree Hybridizing Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures tree hybridizing operations in
#' \pkg{nitro}.
#'
#' Tree hybridizing generates new candidate trees by combining subtrees from
#' different source trees, selecting the best results, and optionally
#' replacing the source trees. This wraps the TNT `tfuse` command with the
#' `hybrid` subcommand.
#'
#' @details
#' ## Default values
#' | Parameter        | Default |
#' |------------------|---------|
#' | `rounds`         | `1`     |
#' | `hybridizations` | `1000`  |
#' | `best_trees`     | `50`    |
#' | `replace`        | `TRUE`  |
#' | `sample_factor`  | `15`    |
#' | `set_only`       | `FALSE` |
#'
#' ## Command output
#' `$render()` produces a string of the form
#' `tfuse= hybrid {rounds}*{hybridizations}/{best_trees} [no]replace clog {sample_factor};`
#'
#' ## Disabling within ExtraSearchMethodsCommand
#' When used within [ExtraSearchMethodsCommand], setting `$rounds` to `0`
#' disables hybridizing without removing it from the configuration.
#'
#' ## Set-only mode and queue integration
#' When `$set_only` is `TRUE`, the command configures parameters without
#' triggering a search. This is set automatically when used within
#' [ExtraSearchMethodsCommand]. Calling `$enqueue()` adds this command to
#' a [CommandQueue] and, when `$set_only` is `FALSE`, also enqueues a
#' [CollapseTreesCommand] and a [TreePlottingCommand].
#'
#' @seealso
#' * [TreeFusingCommand] — a related strategy that fuses clades between
#'   tree pairs.
#' * [ExtraSearchMethodsCommand] — orchestrates multiple search strategies
#'   including tree hybridizing.
#' * [set_tree_search()] — recommended way to add this command to a
#'   [TreeAnalysis].
#'
#' @references
#' Goloboff, P. A., Farris, J. S., & Nixon, K. C. (2008). TNT, a free
#' program for phylogenetic analysis. *Cladistics*, 24(5), 774--786.
#'
#' @examples
#' # Create with default settings
#' th <- TreeHybridizingCommand$new()
#'
#' # Increase hybridizations and rounds
#' th$rounds <- 3
#' th$hybridizations <- 5000
#' th$best_trees <- 100
#'
#' # Generate the TNT command
#' th$render()
#'
#' @importFrom checkmate asInt check_int check_flag test_true
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @export
TreeHybridizingCommand <- R6Class(
  "TreeHybridizingCommand",
  inherit = TreeSearchCommand,
  active = list(
    #' @field rounds \[`integer(1)`\]\cr
    #'   The number of hybridizing rounds to perform. Must be a
    #'   non-negative integer. Set to `0` to disable hybridizing when used
    #'   within [ExtraSearchMethodsCommand].
    rounds = function(value) {
      label <- "rounds"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg rounds} must be an integer.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field hybridizations \[`integer(1)`\]\cr
    #'   The number of hybridizations to perform in each round. Must be a
    #'   positive integer.
    hybridizations = function(value) {
      label <- "hybridizations"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg hybridizations} must be an integer.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field best_trees \[`integer(1)`\]\cr
    #'   The number of best trees from the previous round to carry forward
    #'   as source trees for the next round. Must be a positive integer.
    best_trees = function(value) {
      label <- "best_trees"
      if (missing(value)) {
        rreturn(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg best_trees} must be an integer",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field replace \[`logical(1)`\]\cr
    #'   Whether to replace source trees with better trees produced by
    #'   hybridizing.
    replace = function(value) {
      label <- "replace"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg replace} must be a logical.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field sample_factor \[`integer(1)`\]\cr
    #'   The factor by which to increase the initial tree set size. The
    #'   number of trees retained is proportional to the inverse of this
    #'   value. Must be a positive integer.
    sample_factor = function(value) {
      label <- "sample_factor"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg sample_factor} must be an integer",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `TreeHybridizingCommand` object.
    #'
    #' All arguments use default values. Modify them after construction via
    #' the active bindings.
    #'
    #' @param rounds \[`integer(1)`\]\cr
    #'   The number of hybridizing rounds (default: `1`). See the `$rounds`
    #'   field.
    #' @param hybridization \[`integer(1)`\]\cr
    #'   The number of hybridizations per round (default: `1000`). See the
    #'   `$hybridizations` field.
    #' @param best_trees \[`integer(1)`\]\cr
    #'   The number of best trees to carry forward to the next round (default:
    #'   `50`. See the `best_trees` field.
    #' @param replace \[`logical(1)`\]\cr
    #'   Replace source trees with better trees? (default: `TRUE`). See the
    #'   `replace` field.
    #' @param sample_factor \[`integer(1)`\]\cr
    #'   Factor by which to increase the initial tree set size (default:
    #'  `15`). See tbe `sample_factor` field.
    #'
    #' @return A new `TreeHybridizingCommand` object.
    initialize = function(rounds, hybridization, best_trees, replace,
                          sample_factor, set_only = FALSE) {
      super$initialize(
        name = "tfuse",
        description = "Tree hybridizing using existing trees",
        set_only = set_only
      )

      self$new_argument("rounds", "Rounds", "{value}", 1)
      self$new_argument("hybridizations", "Number of hybridizations", "{value}", 1000)
      self$new_argument("best_trees", "Number of best start trees", "{value}", 50)

      replace_cmd_fmt <- function(value) {
        glue("{value}replace", value = ifelse(value, "", "no"))
      }
      replace_pty_fmt <- function(value) {
        ifelse(value, "yes", "no")
      }
      self$new_argument("replace", "Replace source trees", replace_cmd_fmt, TRUE, replace_pty_fmt)

      sample_factor_cmd_fmt <- function(value) {
        ifelse(value, glue("clog {value}"), "noclog")
      }
      self$new_argument("sample_factor", "Sampling factor", sample_factor_cmd_fmt, 15)

      self$template <- c(
        "hybrid",
        "{rounds}*{hybridizations}/{best_trees}",
        "{replace}",
        "{sample_factor}"
      )

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
