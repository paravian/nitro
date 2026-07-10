#' Command Argument
#'
#' @description
#' An [R6][R6::R6Class] class that stores a single named argument for a
#' [StandardCommand], including its current value, default value, and
#' formatting rules for both TNT command output and human-readable display.
#'
#' `CommandArgument` objects are created and managed internally by
#' [StandardCommand] via `$new_argument()`. Users do not typically need to
#' instantiate this class directly.
#'
#' @details
#' ## Write-once fields
#' `$label`, `$description`, `$command_format`, `$default_value`, and
#' `$pretty_format` are all write-once — they can be set during
#' construction but cannot be changed afterwards. Only `$value` is freely
#' mutable.
#'
#' ## Formatting
#' `$render()` supports two modes and two value sources:
#'
#' | `mode` | Format used |
#' |--------|-------------|
#' | `"command"` | `$command_format` — produces the TNT command fragment. |
#' | `"pretty"` | `$pretty_format` — produces a human-readable string. |
#'
#' | `which` | Value used |
#' |---------|------------|
#' | `"value"` | The current `$value`. |
#' | `"default_value"` | The `$default_value`. |
#'
#' Both `command_format` and `pretty_format` accept either a
#' [glue][glue::glue] string (e.g., `"replic {value}"`) or a function
#' that takes a single `value` argument and returns a string. If
#' `pretty_format` is `NULL`, it defaults to `"{value}"`.
#'
#' @seealso
#' * [execute_analysis()] — the primary consumer of command queues.
#' * [StandardCommand] — creates and manages `CommandArgument` objects via
#'   `$new_argument()`, `$get_argument_value()`, and
#'   `$set_argument_value()`.
#'
#' @keywords internal
#' @importFrom checkmate assert assert_string check_flag check_function check_integerish check_null check_number check_string makeAssertCollection test_null test_true test_string
#' @importFrom cli cli_abort
#' @importFrom glue glue_data
#' @importFrom R6 R6Class
#' @importFrom stringr str_replace_all
CommandArgument <- R6Class(
  "CommandArgument",
  private = list(
    .command_format = NULL,
    .description = NULL,
    .default_value = NULL,
    .label = NULL,
    .pretty_format = NULL,
    .value = NULL
  ),
  active = list(
    #' @field command_format \[`character(1)` or `function`\]\cr
    #'   *(Write-once.)* A [glue][glue::glue] string or function used to
    #'   format the argument value for inclusion in a TNT command string.
    command_format = function(value) {
      if (missing(value)) {
        return(private$.command_format)
      } else {
        if (test_null(private$.command_format)) {
          coll <- makeAssertCollection()
          assert(
            check_string(value, min.chars = 1),
            check_function(value),
            add = coll
          )
          if (!coll$isEmpty()) {
            err <- coll$getMessages()
            cli_abort(c("{.arg command_format} must be either a string or a function.",
              "x" = err
            ))
          }

          private$.command_format <- value
        } else {
          cli_abort(c("{.arg command_format} is a read-only attribute."))
        }
      }
    },
    #' @field default_value \[`any`\]\cr
    #'   *(Write-once.)* The default value of the argument, used when no
    #'   user value has been supplied.
    default_value = function(value) {
      if (missing(value)) {
        return(private$.default_value)
      } else {
        if (test_null(private$.default_value)) {
          private$.default_value <- value
        } else {
          cli_abort(c("{.arg default_value} is a read-only attribute."))
        }
      }
    },
    #' @field description \[`character(1)`\]\cr
    #'   *(Write-once.)* A human-readable description of the argument.
    description = function(value) {
      if (missing(value)) {
        return(private$.description)
      } else {
        if (test_null(private$.description)) {
          val_check <- check_string(value, min.chars = 1)

          if (!test_true(val_check)) {
            cli_abort(c("{.arg description} must be a string.",
              "x" = val_check
            ))
          }

          private$.description <- value
        } else {
          cli_abort(c("{.arg description} is a read-only attribute."))
        }
      }
    },
    #' @field label \[`character(1)`\]\cr
    #'   *(Write-once.)* A unique string identifying this argument within
    #'   its parent [StandardCommand].
    label = function(value) {
      if (missing(value)) {
        return(private$.label)
      } else {
        if (test_null(private$.label)) {
          val_check <- check_string(value, min.chars = 1)

          if (!test_true(val_check)) {
            cli_abort(c("{.arg label} must be a string.",
              "x" = val_check
            ))
          }

          private$.label <- value
        } else {
          cli_abort(c("{.arg label} is a read-only attribute."))
        }
      }
    },
    #' @field pretty_format \[`character(1)`, `function`, or `NULL`\]\cr
    #'   *(Write-once.)* A [glue][glue::glue] string or function used to
    #'   format the argument value for human-readable display (e.g., in
    #'   `$format()` output). Defaults to `"{value}"` when `NULL`.
    pretty_format = function(value) {
      if (missing(value)) {
        return(private$.pretty_format)
      } else {
        if (test_null(private$.pretty_format)) {
          coll <- makeAssertCollection()
          assert(
            check_null(value),
            check_string(value, min.chars = 1),
            check_function(value),
            add = coll
          )

          if (!coll$isEmpty()) {
            err <- coll$getMessages()
            cli_abort(c("{.arg pretty_format} must be either a string or a function.",
              "x" = err
            ))
          }

          if (test_null(value)) {
            value <- "{value}"
          }

          private$.pretty_format <- value
        } else {
          cli_abort(c("{.arg pretty_format} is a read-only attribute."))
        }
      }
    },
    #' @field value \[`any`\]\cr
    #'   The current value of the argument. Freely mutable; validation is
    #'   performed by the active binding in the parent [StandardCommand]
    #'   subclass.
    value = function(value) {
      if (missing(value)) {
        return(private$.value)
      } else {
        private$.value <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `CommandArgument` object.
    #'
    #' All fields except `$value` become read-only after construction.
    #' `CommandArgument` objects are normally created via
    #' [StandardCommand]`$new_argument()` rather than directly.
    #'
    #' @param label \[`character(1)`\]\cr
    #'   A unique string identifying the argument. See the `$label` field.
    #' @param description \[`character(1)`\]\cr
    #'   A human-readable description. See the `$description` field.
    #' @param command_format \[`character(1)` or `function`\]\cr
    #'   Format for TNT command output. See the `$command_format` field.
    #' @param default_value \[`any`\]\cr
    #'   The default value (default: `NULL`). See the `$default_value`
    #'   field.
    #' @param pretty_format \[`character(1)`, `function`, or `NULL`\]\cr
    #'   Format for human-readable display (default: `NULL`, uses
    #'   `"{value}"`). See the `$pretty_format` field.
    #'
    #' @return A new `CommandArgument` object.
    initialize = function(label, description, command_format,
                          default_value = NULL, pretty_format = NULL) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Render the argument value as a formatted string.
    #'
    #' @param mode \[`character(1)`\]\cr
    #'   The format to use: `"command"` (default) for TNT command output,
    #'   or `"pretty"` for human-readable display.
    #' @param which \[`character(1)`\]\cr
    #'   Which value to format: `"value"` (default) for the current value,
    #'   or `"default_value"` for the default.
    #'
    #' @return A single-element character vector containing the formatted
    #'   argument string.
    render = function(mode = "command", which = "value") {
      coll <- makeAssertCollection()
      assert(
        check_string(mode, min.chars = 1),
        check_choice(mode, c("command", "pretty")),
        add = coll
      )
      if (!coll$isEmpty()) {
        err <- coll$getMessages()
        err <- str_replace_all(err, "[({})]", "\\1\\1")
        cli_abort(c("{.arg mode} must be a valid string.",
          "x" = err
        ))
      }

      assert(
        check_string(which, min.chars = 1),
        check_choice(which, c("value", "default_value")),
        add = coll
      )
      if (!coll$isEmpty()) {
        err <- coll$getMessages()
        err <- str_replace_all(err, "[({})]", "\\1\\1")
        cli_abort(c("{.arg which} must be a valid string.",
          "x" = err
        ))
      }

      format_obj <- ifelse(
        mode == "command",
        self$command_format,
        self$pretty_format
      )

      data_obj <- list()
      if (which == "value") {
        data_obj$value <- self$value
      } else {
        data_obj$value <- self$default_value
      }

      if (test_string(format_obj)) {
        output <- glue_data(data_obj, format_obj)
      } else {
        output <- do.call(format_obj, data_obj)
      }

      output
    }
  )
)
