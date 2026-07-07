#' Possible Steps Command
#'
#' @description
#' An [R6][R6::R6Class] class that computes the minimum and maximum possible
#' number of steps per character for the trees currently held in memory.
#'
#' This command is enqueued automatically when step range information is
#' required (e.g., for implied weighting diagnostics). Users do not
#' typically need to instantiate it directly.
#'
#' @details
#' ## Default values
#' | Parameter     | Default |
#' |---------------|---------|
#' | `active_taxa` | `TRUE`  |
#'
#' ## Command output
#' `$render()` produces `minmax;` (all taxa) or `minmax -;` (active taxa
#' only).
#'
#' ## Output parsing
#' `$transform()` parses the raw TNT output and returns a named numeric
#' vector with elements `"minimum"` and `"maximum"` containing the total
#' step counts across all characters.
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `710`.
#'
#' @keywords internal
#' @importFrom checkmate check_flag test_true test_class
#' @importFrom cli cli_abort
#' @importFrom dplyr mutate rename
#' @importFrom magrittr %>% extract2 set_names
#' @importFrom R6 R6Class
#' @importFrom stringr str_match_all str_to_lower
#' @importFrom tibble as_tibble
PossibleStepsCommand <- R6Class(
  "PossibleStepsCommand",
  inherit = StandardCommand,
  active = list(
    #' @field active_taxa \[`logical(1)`\]\cr
    #'   Whether to compute possible steps for active taxa only.
    active_taxa = function(value) {
      label <- "active_taxa"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be a logical value.",
                      "x" = val_check))
        }

        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Adds this command at priority `710`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(self, 710)
      .queue
    },
    #' @description
    #' Create a new `PossibleStepsCommand` object.
    #'
    #' @param active_taxa \[`logical(1)`\]\cr
    #'   Compute steps for active taxa only? (default: `TRUE`). See the
    #'   `$active_taxa` field.
    #' @param ... Optional named arguments passed to the constructor of the
    #'   command class.
    #'
    #' @return A new `PossibleStepsCommand` object.
    initialize = function(active_taxa, ...) {
      super$initialize(
        name = "minmax",
        description = "Possible number of steps per character",
        outputs = "possible steps",
        ...
      )

      active_cmd_fmt <- function(value) {
        ifelse(value, "-", NA)
      }
      active_pty_fmt <- function(value) {
        ifelse(value, "yes", "no")
      }
      self$new_argument(
        label = "active_taxa",
        description = "Active taxa only",
        command_format = active_cmd_fmt,
        default_value = TRUE,
        pretty_format = active_pty_fmt
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
    #' Parse raw TNT output into a named numeric vector of step counts.
    #'
    #' @param output \[`character`\]\cr
    #'   A character vector of raw output lines from the TNT executable.
    #'
    #' @return A named numeric vector with elements `"minimum"` and
    #'   `"maximum"`.
    transform = function(output) {
      steps <- str_match_all(output, "(?<type>M(ax|in)imum).+total = (?<count>[0-9\\.]+)") %>%
        extract2(1) %>%
        as.data.frame()

      output <- as.numeric(steps$count) %>%
        set_names(str_to_lower(steps$type))

      output
    }
  )
)
