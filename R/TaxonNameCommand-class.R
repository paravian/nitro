#' Taxon Name Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures how taxon names are presented
#' in TNT output.
#'
#' This command is created automatically by [ReadDataCommand] as a
#' prerequisite to reading matrix data. Users do not typically need to
#' instantiate it directly.
#'
#' @details
#' ## Default values
#' | Parameter        | Default |
#' |------------------|---------|
#' | `use_names`      | `FALSE` |
#' | `maximum_length` | `32`    |
#'
#' ## Command output
#' `$render()` produces two commands: `taxname =;` or `taxname -;`
#' (enabling or disabling name-based taxon references), followed by
#' `taxname +{maximum_length};` (setting the maximum name length).
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `210`.
#'
#' @seealso
#' * [ReadDataCommand] — creates this command automatically.
#'
#' @keywords internal
#' @importFrom checkmate check_flag check_int test_class test_true
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
#' @importFrom stringr str_remove str_split_1
TaxonNameCommand <- R6Class(
  "TaxonNameCommand",
  inherit = StandardCommand,
  private = list(
    .data = NULL
  ),
  active = list(
    #' @field use_names \[`logical(1)`\]\cr
    #'   Whether to use taxon names (rather than indices) when referring to
    #'   taxa in TNT commands and output.
    use_names = function(value) {
      label <- "use_names"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg use_names} must be a logical value.",
            "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field maximum_length \[`integer(1)`\]\cr
    #'   The maximum number of characters allowed in a taxon name. Must be
    #'   a positive integer.
    maximum_length = function(value) {
      label <- "maximum_length"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 1)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg maximum_length} must be a valid integer.",
            "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Adds this command at priority `210`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(self, 210)
      .queue
    },
    #' @description
    #' Create a new `TaxonNameCommand` object.
    #'
    #' This command is created automatically by [ReadDataCommand]. Direct
    #' instantiation is rarely necessary.
    #'
    #' @param use_names \[`logical(1)`\]\cr
    #'   Use taxon names in output? (default: `FALSE`). See the
    #'   `$use_names` field.
    #' @param maximum_length \[`integer(1)`\]\cr
    #'   Maximum taxon name length (default: `32`). See the
    #'   `$maximum_length` field.
    #'
    #' @return A new `TaxonNameCommand` object.
    initialize = function(use_names, maximum_length) {
      super$initialize(
        name = "taxname",
        description = "Change presentation aspects of taxon names"
      )

      use_names_cmd_fmt <- function(value) {
        ifelse(value, "=", "-")
      }
      use_names_pty_fmt <- function(value) {
        ifelse(value, "yes", "no")
      }
      self$new_argument(
        label = "use_names",
        description = "Using taxon names",
        command_format = use_names_cmd_fmt,
        default_value = FALSE,
        pretty_format = use_names_pty_fmt
      )

      self$new_argument(
        label = "maximum_length",
        description = "Maximum taxon name length",
        command_format = "+{value}",
        default_value = 32
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
    #' Render the TNT `taxname` command strings.
    #'
    #' Produces two commands: one enabling or disabling name-based taxon
    #' references, and one setting the maximum name length.
    #'
    #' @param ... Not used.
    #'
    #' @return A character vector of two TNT command strings.
    render = function(...) {
      cmd <- super$render() %>%
        str_remove(";$") %>%
        str_split_1(" ")

      all_cmds <- paste(cmd[1], " ", cmd[-1], ";", sep = "")
      all_cmds
    }
  )
)
