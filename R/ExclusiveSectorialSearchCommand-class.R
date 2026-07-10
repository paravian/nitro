#' Exclusive Sectorial Search Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures an exclusive sectorial search
#' in \pkg{nitro}.
#'
#' Exclusive sectorial searches subdivide the entire tree into
#' non-overlapping sectors and perform branch swapping within each. The
#' number of sectors can be specified as a fixed value or a range. This
#' wraps the `sectsch` TNT command with the `xss` subcommand.
#'
#' @details
#' ## Default values
#' | Parameter    | Default    |
#' |--------------|------------|
#' | `selections` | `c(8, 12)` |
#' | `rounds`     | `2`        |
#' | `slack`      | `0`        |
#' | `buffer`     | `FALSE`    |
#' | `set_only`   | `FALSE`    |
#'
#' ## Command output
#' `$render()` produces a string of the form
#' `sectsch= xss {selections}+{rounds} [no]xbuf slack {slack};`
#'
#' ## Sector count
#' `$selections` accepts either a single integer (fixed sector count) or a
#' sorted two-element vector specifying a range (e.g., `c(8, 12)`). All
#' values must be at least 2.
#'
#' ## Set-only mode and queue integration
#' When `$set_only` is `TRUE`, the command configures parameters without
#' triggering a search. This is set automatically when used within
#' [ExtraSearchMethodsCommand]. Calling `$enqueue()` adds this command to
#' a [CommandQueue] and, when `$set_only` is `FALSE`, also enqueues a
#' [CollapseTreesCommand] and a [TreePlottingCommand].
#'
#' @seealso
#' * [ConstrainedSectorialSearchCommand] — constraint-based sectorial
#'   searches.
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
#' xss <- ExclusiveSectorialSearchCommand$new()
#'
#' # Set a fixed number of sectors
#' xss$selections <- 10
#'
#' # Set a range of sectors
#' xss$selections <- c(6, 14)
#'
#' # Generate the TNT command
#' xss$render()
#'
#' @importFrom checkmate asInt assert_int check_int check_integerish check_flag makeAssertCollection test_true
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @export
ExclusiveSectorialSearchCommand <- R6Class(
  "ExclusiveSectorialSearchCommand",
  inherit = SectorialSearchCommand,
  active = list(
    #' @field selections \[`integer(1)` or `integer(2)`\]\cr
    #'   The number of non-overlapping sectors to subdivide the tree into.
    #'   Supply a single integer for a fixed count, or a sorted two-element
    #'   vector for a range (e.g., `c(8, 12)`). Values must be at least 2.
    selections = function(value) {
      label <- "selections"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_integerish(value, lower = 2, any.missing = FALSE, min.len = 1, max.len = 2, unique = TRUE, sorted = TRUE)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg selections} must be a valid numeric vector.",
            "x" = val_check
          ))
        }

        value <- sapply(value, floor)
        self$set_argument_value(label, value)
      }
    },
    #' @field rounds \[`integer(1)`\]\cr
    #'   The number of times to repeat the exclusive sector selection and
    #'   analysis cycle. Must be a non-negative integer.
    rounds = function(value) {
      label <- "rounds"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg rounds} must be an integer.",
            "x" = val_check
          ))
        }

        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `ExclusiveSectorialSearchCommand` object.
    #'
    #' @param selections \[`integer(1)` or `integer(2)`\]\cr
    #'   Fixed sector count or a sorted two-element range (default:
    #'   `c(8, 12)`). Values must be at least 2. See the `$selections`
    #'   field.
    #' @param rounds \[`integer(1)`\]\cr
    #'   Number of sector selection and analysis cycles (default: `2`). See
    #'   the `$rounds` field.
    #' @param slack \[`integer(1)`\]\cr
    #'   Percentage by which to increase available memory during searches
    #'   (default: `0`). See the `$slack` field.
    #' @param buffer \[`logical(1)`\]\cr
    #'   Use an independent memory buffer for sector analysis (default:
    #'   `FALSE`). See the `$buffer` field.
    #' @param set_only \[`logical(1)`\]\cr
    #'   Configure-only mode (default: `FALSE`). See the `$set_only` field.
    #'
    #' @return A new `ExclusiveSectorialSearchCommand` object.
    initialize = function(selections, rounds, slack, buffer, set_only = FALSE) {
      super$initialize(
        name = "sectsch",
        description = "Exclusive sectorial searches using existing trees",
        set_only = set_only
      )

      selections_fmt <- function(value) {
        ifelse(length(value) == 2, paste(value, collapse = "-"), value)
      }
      self$new_argument("selections", "Selections", selections_fmt, c(8, 12), selections_fmt)
      self$new_argument("rounds", "Selection rounds", "{value}", 2)

      self$new_argument("slack", "Percentage memory increase", "minfork {value}", 0)

      buffer_cmd_fmt <- function(value) {
        glue("{value}xbuf", value = ifelse(value, "", "no"))
      }
      buffer_pty_fmt <- function(value) {
        ifelse(value, "yes", "no")
      }
      self$new_argument("buffer", "Using independent buffer", buffer_cmd_fmt, FALSE, buffer_pty_fmt)

      self$template <- c(
        "xss",
        "{selections}+{rounds}",
        "{buffer}",
        "{slack}"
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
