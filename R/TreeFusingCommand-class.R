#' Tree Fusing Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures tree fusing operations in
#' \pkg{nitro}.
#'
#' Tree fusing (Goloboff, 1999) exchanges clades between pairs of trees in
#' memory, accepting exchanges that improve (or optionally equal) the
#' current score. This can recover optimal topologies that branch swapping
#' alone may miss. This wraps the TNT `tfuse` command.
#'
#' @details
#' ## Default values
#' | Parameter        | Default |
#' |------------------|---------|
#' | `rounds`         | `5`     |
#' | `exchange_equal` | `FALSE` |
#' | `start_best`     | `TRUE`  |
#' | `keep_all`       | `TRUE`  |
#' | `refuse`         | `TRUE`  |
#' | `swap`           | `TRUE`  |
#' | `set_only`       | `FALSE` |
#'
#' ## Command output
#' `$render()` produces a string of the form
#' `tfuse= rounds {rounds} [no]equals [no]beststart [no]keepall [no]repeat [no]swap;`
#'
#' ## Disabling within ExtraSearchMethodsCommand
#' When used within [ExtraSearchMethodsCommand], setting `$rounds` to `0`
#' disables fusing without removing it from the configuration.
#'
#' ## Set-only mode and queue integration
#' When `$set_only` is `TRUE`, the command configures parameters without
#' triggering a search. This is set automatically when used within
#' [ExtraSearchMethodsCommand]. Calling `$enqueue()` adds this command to
#' a [CommandQueue] and, when `$set_only` is `FALSE`, also enqueues a
#' [CollapseTreesCommand] and a [TreePlottingCommand].
#'
#' @seealso
#' * [TreeHybridizingCommand] — a related strategy that hybridises trees.
#' * [ExtraSearchMethodsCommand] — orchestrates multiple search strategies
#'   including tree fusing.
#' * [set_tree_search()] — recommended way to add this command to a
#'   [TreeAnalysis].
#'
#' @references
#' Goloboff, P. A. (1999). Analyzing large data sets in reasonable times:
#' solutions for composite optima. *Cladistics*, 15(4), 415--428.
#'
#' Goloboff, P. A., Farris, J. S., & Nixon, K. C. (2008). TNT, a free
#' program for phylogenetic analysis. *Cladistics*, 24(5), 774--786.
#'
#' @examples
#' # Create with default settings
#' tf <- TreeFusingCommand$new()
#'
#' # Accept equal-score exchanges and increase rounds
#' tf$exchange_equal <- TRUE
#' tf$rounds <- 10
#'
#' # Disable TBR swapping after exchanges
#' tf$swap <- FALSE
#'
#' # Generate the TNT command
#' tf$render()
#'
#' @importFrom checkmate asInt check_int check_flag test_true
#' @importFrom cli cli_abort cli_text col_grey col_red style_italic
#' @importFrom R6 R6Class
#' @export
TreeFusingCommand <- R6Class(
  "TreeFusingCommand",
  inherit = TreeSearchCommand,
  active = list(
    #' @field rounds \[`integer(1)`\]\cr
    #'   The number of tree fusing rounds to perform. Must be an integer
    #'   between 0 and 100. Set to `0` to disable fusing when used within
    #'   [ExtraSearchMethodsCommand].
    rounds = function(value) {
      label <- "rounds"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0, upper = 100)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg rounds} must be an integer.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field exchange_equal \[`logical(1)`\]\cr
    #'   Whether to accept clade exchanges that produce trees of equal
    #'   score.
    exchange_equal = function(value) {
      label <- "exchange_equal"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg exchange_equal} must be a logical.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field start_best \[`logical(1)`\]\cr
    #'   Whether to begin fusing from the best tree in memory.
    start_best = function(value) {
      label <- "start_best"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg start_best} must be a logical.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field keep_all \[`logical(1)`\]\cr
    #'   Whether to keep all trees found during fusing, rather than only
    #'   the best.
    keep_all = function(value) {
      label <- "keep_all"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg keep_all} must be a logical.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field refuse \[`logical(1)`\]\cr
    #'   Whether to re-fuse trees after each individual fuse until no
    #'   further exchanges improve the score.
    refuse = function(value) {
      label <- "refuse"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg refuse} must be a logical.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field swap \[`logical(1)`\]\cr
    #'   Whether to perform TBR swapping after each clade exchange.
    swap = function(value) {
      label <- "swap"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg swap} must be a logical.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `TreeFusingCommand` object.
    #'
    #' #' @param rounds \[`integer(1)`\]\cr
    #'   Number of fusing rounds; `0` disables fusing within
    #'   [ExtraSearchMethodsCommand] (default: `5`). See the `$rounds`
    #'   field.
    #' @param exchange_equal \[`logical(1)`\]\cr
    #'   Accept equal-score clade exchanges (default: `FALSE`). See the
    #'   `$exchange_equal` field.
    #' @param start_best \[`logical(1)`\]\cr
    #'   Begin fusing from the best tree in memory (default: `TRUE`). See
    #'   the `$start_best` field.
    #' @param keep_all \[`logical(1)`\]\cr
    #'   Keep all trees found during fusing (default: `TRUE`). See the
    #'   `$keep_all` field.
    #' @param refuse \[`logical(1)`\]\cr
    #'   Re-fuse after each individual fuse until no further improvement
    #'   (default: `TRUE`). See the `$refuse` field.
    #' @param swap \[`logical(1)`\]\cr
    #'   Perform TBR swapping after each clade exchange (default: `TRUE`).
    #'   See the `$swap` field.
    #' @param set_only \[`logical(1)`\]\cr
    #'   Configure-only mode (default: `FALSE`). See the `$set_only` field.
    #'
    #' @return A new `TreeFusingCommand` object.
    initialize = function(rounds, exchange_equal, start_best, keep_all, refuse,
                          swap, set_only = FALSE) {
      super$initialize(
        name = "tfuse",
        description = "Tree fusing using existing trees",
        set_only = set_only
      )

      self$new_argument("rounds", "Rounds", "rounds {value}", 5)

      yes_no_pty_fmt <- function(value) {
        ifelse(value, "yes", "no")
      }
      exchange_cmd_fmt <- function(value) {
        glue("{value}equals", value = ifelse(value, "", "no"))
      }
      self$new_argument("exchange_equal", "Exchange equal score trees", exchange_cmd_fmt, FALSE, yes_no_pty_fmt)

      start_cmd_fmt <- function(value) {
        glue("{value}beststart", value = ifelse(value, "", "no"))
      }
      self$new_argument("start_best", "Start with best tree", start_cmd_fmt, TRUE, yes_no_pty_fmt)

      keep_all_cmd_fmt <- function(value) {
        glue("{value}keepall", value = ifelse(value, "", "no"))
      }
      self$new_argument("keep_all", "Keep all trees", keep_all_cmd_fmt, TRUE, yes_no_pty_fmt)

      refuse_cmd_fmt <- function(value) {
        glue("{value}repeat", value = ifelse(value, "", "no"))
      }
      self$new_argument("refuse", "Re-fuse on every fuse", refuse_cmd_fmt, TRUE, yes_no_pty_fmt)

      swap_cmd_fmt <- function(value) {
        glue("{value}swap", value = ifelse(value, "", "no"))
      }
      self$new_argument("swap", "Swap after exchanges", swap_cmd_fmt, TRUE, yes_no_pty_fmt)

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
