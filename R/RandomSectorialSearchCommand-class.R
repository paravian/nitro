#' Random Sectorial Search Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures a random sectorial search in
#' \pkg{nitro}.
#'
#' Random sectorial searches select sectors of the tree randomly and perform
#' branch swapping within each sector. The size of sectors is drawn from a
#' range defined by `$min_size` and `$max_size`. This wraps the `sectsch` TNT
#' command with the `rss` subcommand.
#'
#' @details
#' ## Default values
#' | Parameter          | Default  |
#' |--------------------|----------|
#' | `min_size`         | `35`   |
#' | `max_size`         | `55`   |
#' | `selection_factor` | `50`     |
#' | `increase`         | `100`    |
#' | `small_starts`     | `3`      |
#' | `slack`            | `0`      |
#' | `buffer`           | `FALSE`  |
#' | `set_only`         | `FALSE`  |
#'
#' ## Command output
#' `$render()` produces a string of the form
#' `sectsch= rss minsize {min_size} maxsize {max_size} selfact {selection_factor} ...;`
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
#' * [ExclusiveSectorialSearchCommand] — exclusive (non-overlapping)
#'   sectorial searches.
#' * [ExtraSearchMethodsCommand] — orchestrates multiple search strategies
#'   including sectorial searches.
#' * [set_tree_search()] — recommended way to add this command to a
#'   [TreeAnalysis]; handles automatic sector sizing.
#'
#' @references
#' Goloboff, P. A., Farris, J. S., & Nixon, K. C. (2008). TNT, a free program
#' for phylogenetic analysis. *Cladistics*, 24(5), 774--786.
#'
#' @examples
#' # Create with default (automatic) sector sizing
#' rss <- RandomSectorialSearchCommand$new()
#'
#' # Set sector sizes manually (required outside of a TreeAnalysis)
#' rss$min_size <- 20
#' rss$max_size <- 25
#'
#' # Adjust search intensity
#' rss$selection_factor <- 100
#' rss$small_starts <- 5
#'
#' # Generate the TNT command
#' rss$render()
#'
#' @importFrom checkmate asInt check_int check_flag check_null check_string makeAssertCollection test_int test_true
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @export
RandomSectorialSearchCommand <- R6Class(
  "RandomSectorialSearchCommand",
  inherit = SectorialSearchCommand,
  active = list(
    #' @field min_size \[`integer(1)` or `NULL`\]\cr
    #'   The minimum size (number of taxa) of randomly selected sectors. Must be
    #'   an integer greater than or equal to 5 and no greater than `$max_size`.
    min_size = function(value) {
      label <- "min_size"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        coll <- makeAssertCollection()
        assert_int(value, lower = 5, add = coll)
        if (!test_null(self$max_size)) {
          assert_int(value, upper = self$max_size, add = coll)
        }

        if (!coll$isEmpty()) {
          val_check <- coll$getMessages()
          cli_abort(c("{.arg min_size} must be a valid integer",
            "x" = val_check
          ))
        }

        self$set_argument_value(label, value)
      }
    },
    #' @field max_size \[`integer(1)` or `NULL`\]\cr
    #'   The maximum size (number of taxa) of randomly selected sectors. Must be
    #'   an integer greater than or equal to 5 and no less than `$min_size`.
    max_size = function(value) {
      label <- "max_size"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        coll <- makeAssertCollection()
        assert_int(value, lower = 5, add = coll)
        if (!test_null(self$min_size)) {
          assert_int(value, lower = self$min_size, add = coll)
        }

        if (!coll$isEmpty()) {
          val_check <- coll$getMessages()
          cli_abort(c("{.arg max_size} must be a valid integer",
            "x" = val_check
          ))
        }

        self$set_argument_value(label, value)
      }
    },
    #' @field selection_factor \[`integer(1)`\]\cr
    #'   The maximum number of random sector selections to perform for the
    #'   currently active taxa. Must be a non-negative integer.
    selection_factor = function(value) {
      label <- "selection_factor"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg selection_factor} must be an integer",
            "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field increase \[`integer(1)`\]\cr
    #'   The factor by which to increase the sector size once enough
    #'   selections of the current size have been completed. Must be a
    #'   non-negative integer.
    increase = function(value) {
      label <- "increase"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg increase} must be an integer",
            "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field small_starts \[`integer(1)`\]\cr
    #'   The number of random addition sequence + TBR swapping rounds to
    #'   perform for sectors smaller than `$min_size`. Must be a positive
    #'   integer.
    small_starts = function(value) {
      label <- "small_starts"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg small_starts} must be an integer",
            "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `RandomSectorialSearchCommand` object.
    #'
    #' When used via [set_tree_search()], `min_size` and `max_size` are set
    #' automatically from the number of taxa. If using this class directly,
    #' set them manually.
    #'
    #' @param min_size \[`integer(1)`\]\cr
    #'   Minimum sector size in taxa. See the `$min_size` field.
    #' @param max_size \[`integer(1)`\]\cr
    #'   Maximum sector size in taxa. See the `$max_size` field.
    #' @param selection_factor \[`integer(1)`\]\cr
    #'   Maximum number of random sector selections (default: `50`). See
    #'   the `$selection_factor` field.
    #' @param increase \[`integer(1)`\]\cr
    #'   Factor by which to increase sector size after enough selections
    #'   (default: `100`). See the `$increase` field.
    #' @param small_starts \[`integer(1)`\]\cr
    #'   RAS + TBR rounds for sectors smaller than `min_size` (default:
    #'   `3`). See the `$small_starts` field.
    #' @param slack \[`integer(1)`\]\cr
    #'   Percentage by which to increase available memory during searches
    #'   (default: `0`). See the `$slack` field.
    #' @param buffer \[`logical(1)`\]\cr
    #'   Use an independent memory buffer for sector analysis (default:
    #'   `FALSE`). See the `$buffer` field.
    #' @param set_only \[`logical(1)`\]\cr
    #'   Configure-only mode (default: `FALSE`). See the `$set_only` field.
    #'
    #' @return A new `RandomSectorialSearchCommand` object.
    initialize = function(min_size, max_size, selection_factor, increase,
                          small_starts, slack, buffer, set_only = FALSE) {
      super$initialize(
        name = "sectsch",
        description = "Random sectorial searches using existing trees",
        set_only = set_only
      )

      self$new_argument("min_size", "Minimum sector size", "{value}", 35)
      self$new_argument("max_size", "Maximum sector size", "{value}", 55)
      self$new_argument("selection_factor", "Maximum random selections", "selfact {value}", 50)
      self$new_argument("increase", "Selection increase factor", "increase {value}", 100)
      self$new_argument("small_starts", "Small sector search rounds", "starts {value}", 3)

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
        "rss",
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
