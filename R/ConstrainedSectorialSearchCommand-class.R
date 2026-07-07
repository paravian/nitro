#' Constrained Sectorial Search Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures a constrained sectorial search
#' in \pkg{nitro}.
#'
#' Constrained sectorial searches select sectors of the tree based on
#' constraint groups defined by fork (node) ranges, and then perform branch
#' swapping within each sector. This wraps the `sectsch` TNT command with
#' the `css` subcommand.
#'
#' @details
#' ## Default values
#' | Parameter  | Default |
#' |------------|---------|
#' | `rounds`   | `3`     |
#' | `min_fork` | `2`     |
#' | `max_fork` | `10`    |
#' | `slack`    | `0`     |
#' | `buffer`   | `FALSE` |
#' | `set_only` | `FALSE` |
#'
#' ## Command output
#' `$render()` produces a string of the form
#' `sectsch= css rounds {rounds} minfork {min_fork} maxfork {max_fork} ...;`
#'
#' ## Fork range
#' `$min_fork` and `$max_fork` define the range of fork (node) sizes used
#' to select constraint-based sectors. `$min_fork` must not exceed
#' `$max_fork`.
#'
#' ## Set-only mode and queue integration
#' When `$set_only` is `TRUE`, the command configures parameters without
#' triggering a search. This is set automatically when used within
#' [ExtraSearchMethodsCommand]. Calling `$enqueue()` adds this command to
#' a [CommandQueue] and, when `$set_only` is `FALSE`, also enqueues a
#' [CollapseTreesCommand] and a [TreePlottingCommand].
#'
#' @seealso
#' * [ExclusiveSectorialSearchCommand] — exclusive (non-overlapping)
#'   sectorial searches.
#' * [RandomSectorialSearchCommand] — random sectorial searches.
#' * [ExtraSearchMethodsCommand] — orchestrates multiple search strategies
#'   including sectorial searches.
#' * [set_tree_search()] — recommended way to add this command to a
#'   [TreeAnalysis].
#'
#' @references
#' Goloboff, P. A., Farris, J. S., & Nixon, K. C. (2008). TNT, a free
#' program for phylogenetic analysis. *Cladistics*, 24(5), 774--786.
#'
#' @examples
#' # Create with default settings
#' css <- ConstrainedSectorialSearchCommand$new()
#'
#' # Customise fork range and rounds
#' css$min_fork <- 5
#' css$max_fork <- 20
#' css$rounds <- 10
#'
#' # Generate the TNT command
#' css$render()
#'
#' @importFrom checkmate assert_int check_int check_flag makeAssertCollection test_true test_null
#' @importFrom cli cli_abort cli_text col_grey col_red
#' @importFrom glue glue glue_data
#' @importFrom R6 R6Class
#' @export
ConstrainedSectorialSearchCommand <- R6Class(
  "ConstrainedSectorialSearchCommand",
  inherit = SectorialSearchCommand,
  active = list(
    #' @field min_fork \[`integer(1)`\]\cr
    #'   The minimum fork (node) number for constraint-based sector
    #'   selection. Must be a non-negative integer and no greater than
    #'   `$max_fork`.
    min_fork = function(value) {
      label <- "min_fork"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        coll <- makeAssertCollection()
        assert_int(value, lower = 0, add = coll)
        if (!test_null(private$.min_fork)) {
          assert_int(value, upper = private$.max_fork, add = coll)
        }

        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg min_fork} must be a valid integer.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field max_fork \[`integer(1)`\]\cr
    #'   The maximum fork (node) number for constraint-based sector
    #'   selection. Must be a non-negative integer and no less than
    #'   `$min_fork`.
    max_fork = function(value) {
      label <- "max_fork"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        coll <- makeAssertCollection()
        assert_int(value, lower = 0, add = coll)
        if (!is.null(private$.min_fork)) {
          assert_int(value, lower = private$.min_fork, add = coll)
        }

        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg max_fork} must be a valid integer.",
                      "x" = val_check))
        }

        self$set_argument_value(label, value)
      }
    },
    #' @field rounds \[`integer(1)`\]\cr
    #'   The number of times to cycle over constraint groups. Must be a
    #'   non-negative integer.
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
    }
  ),
  public = list(
    #' @description
    #' Create a new `ConstrainedSectorialSearchCommand` object.
    #'
    #' @param rounds \[`integer(1)`\]\cr
    #'   Number of cycles over constraint groups (default: `3`). See the
    #'   `$rounds` field.
    #' @param min_fork \[`integer(1)`\]\cr
    #'   Minimum fork number for sector selection (default: `2`). Must not
    #'   exceed `max_fork`. See the `$min_fork` field.
    #' @param max_fork \[`integer(1)`\]\cr
    #'   Maximum fork number for sector selection (default: `10`). Must not
    #'   be less than `min_fork`. See the `$max_fork` field.
    #' @param slack \[`integer(1)`\]\cr
    #'   Percentage by which to increase available memory during searches
    #'   (default: `0`). See the `$slack` field.
    #' @param buffer \[`logical(1)`\]\cr
    #'   Use an independent memory buffer for sector analysis (default:
    #'   `FALSE`). See the `$buffer` field.
    #' @param set_only \[`logical(1)`\]\cr
    #'   Configure-only mode (default: `FALSE`). See the `$set_only` field.
    #'
    #' @return A new `ConstrainedSectorialSearchCommand` object.
    initialize = function(rounds, min_fork, max_fork, slack, buffer,
                          set_only = FALSE) {
      super$initialize(
        name = "sectsch",
        description = "Constrained sectorial searches using existing trees",
        set_only = set_only
      )

      self$new_argument("rounds", "Rounds", "rounds {value}", 3)
      self$new_argument("min_fork", "Minimum fork size", "minfork {value}", 2)
      self$new_argument("max_fork", "Maximum fork size", "maxfork {value}", 10)

      self$new_argument("slack", "Percentage memory increase", "slack {value}", 0)

      buffer_cmd_fmt <- function(value) {
        glue("{value}xbuf", value = ifelse(value, "", "no"))
      }
      buffer_pty_fmt <- function(value) {
        ifelse(value, "yes", "no")
      }
      self$new_argument("buffer", "Using independent buffer", buffer_cmd_fmt, FALSE, buffer_pty_fmt)

      all_labels <- sapply(private$.arguments, getElement, "label")
      self$template <- c(
        "css",
        paste("{", all_labels, "}", sep = "")
      )

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
