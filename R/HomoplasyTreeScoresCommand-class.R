#' Homoplasy Tree Scores Command
#'
#' @description
#' An [R6][R6::R6Class] class that computes tree scores adjusted for
#' homoplasy in \pkg{nitro}.
#'
#' This command is enqueued automatically by [ImpliedWeightingCommand] at
#' priority `710` to compute homoplasy scores after a search. Users do not
#' typically need to instantiate it directly.
#'
#' @details
#' ## Default values
#' | Parameter          | Default |
#' |--------------------|---------|
#' | `soft_polytomies`  | `FALSE` |
#'
#' ## Command output
#' `$render()` produces `scores;` (standard) or `scores !;` (when
#' `$soft_polytomies` is `TRUE`).
#'
#' ## Output parsing
#' `$transform()` parses the raw TNT output and returns a numeric vector
#' of homoplasy scores, one per tree.
#'
#' ## Queue integration
#' This command has no `$enqueue()` override; it is added to a
#' [CommandQueue] directly by [ImpliedWeightingCommand] at priority `710`.
#'
#' @seealso
#' * [ImpliedWeightingCommand] — enqueues this command after a search.
#'
#' @keywords internal
#' @importFrom checkmate assert check_flag check_integerish check_null makeAssertCollection test_null test_numeric test_true test_class
#' @importFrom cli cli_abort
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom stringr str_extract_all str_remove str_split
#' @importFrom utils tail
HomoplasyTreeScoresCommand <- R6Class(
  "HomoplasyTreeScoresCommand",
  inherit = StandardCommand,
  active = list(
    #' @field soft_polytomies \[`logical(1)`\]\cr
    #'   Whether to optimise polytomies as soft (i.e., treat unresolved
    #'   nodes as compatible with any resolution) when computing scores.
    soft_polytomies = function(value) {
      label <- "soft_polytomies"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg soft_polytomies} must be a logical value.",
                      "x" = val_check))
        }
        return(self$set_argument_value(label, value))
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `HomoplasyTreeScoresCommand` object.
    #'
    #' @param soft_polytomies \[`logical(1)`\]\cr
    #'   Optimise polytomies as soft? (default: `FALSE`). See the
    #'   `$soft_polytomies` field.
    #' @param ... Optional named arguments passed to the constructor of the
    #'   command class.
    #'
    #' @return A new `HomoplasyTreeScoresCommand` object.
    initialize = function(soft_polytomies = FALSE, ...) {
      a <- as.list(environment(), all = TRUE) %>%
        head(-1)

      super$initialize(
        name = "scores",
        description = "Calculate tree scores adjusted for homoplasy",
        outputs = "homoplasy scores",
        ...
      )

      soft_cmd_fmt <- function(value) {
        ifelse(value, "!", NA)
      }
      soft_pty_fmt <- function(value) {
        ifelse(value, "yes", "no")
      }

      self$new_argument(
        label = "soft_polytomies",
        description = "Optimise polytomies as soft",
        command_format = soft_cmd_fmt,
        default_value = FALSE,
        pretty_format = soft_pty_fmt
      )

      all_labels <- sapply(private$.arguments, getElement, "label")
      self$template <- paste("{", all_labels, "}", sep = "")

      for (argument in private$.arguments) {
        arg_val <- try(get(argument$label), silent = TRUE)
        if (test_class(arg_val, "try-error")) {
          arg_val <- argument$default_value
        }
        self[[argument$label]] <- arg_val
      }
    },
    #' @description
    #' Parse raw TNT output into a numeric vector of homoplasy scores.
    #'
    #' @param output \[`character`\]\cr
    #'   A character vector of raw output lines from the TNT executable.
    #'
    #' @return A numeric vector of homoplasy scores, one per tree.
    transform = function(output) {
      output <- super$transform(output)

      output <- str_extract_all(output, "^( *[0-9\\.]+)+") %>%
        unlist() %>%
        tail(-1) %>%
        str_remove(" *[0-9\\.]+ +") %>%
        str_split(" +") %>%
        unlist() %>%
        as.numeric()

      output
    }
  )
)
