#' Argument-Aware TNT Command Base Class
#'
#' @description
#' `StandardCommand` extends [BasicCommand] with a system for declaring,
#' storing, and rendering named command arguments. It is the base class for
#' any TNT command that accepts configurable parameters.
#'
#' This class is not exported. It is intended to be subclassed by
#' [TreeSearchCommand] or other command classes that require arguments.
#'
#' @details
#' ## Subclassing
#' Subclasses should call `super$initialize(name, description)` and then
#' register their arguments with `$new_argument()`. After all arguments are
#' registered, set `$template` to control how arguments are assembled into
#' the final command string. A typical pattern:
#'
#' ```r
#' super$initialize(name = "mycommand", description = "Does something")
#'
#' self$new_argument(
#'   label          = "param_a",
#'   description    = "First parameter",
#'   command_format = "a {value}",
#'   default_value  = 5
#' )
#'
#' all_labels <- sapply(private$.arguments, `[[`, "label")
#' self$template <- paste("{", all_labels, "}", sep = "")
#' ```
#'
#' ## Argument system
#' Each argument registered via `$new_argument()` is stored as a
#' [CommandArgument] object. Arguments support:
#' * **`command_format`** — a [glue][glue::glue] string (e.g.,
#'   `"replic {value}"`) or a function returning the formatted TNT
#'   command fragment.
#' * **`pretty_format`** — an optional glue string or function used by
#'   `$format()` for human-readable display.
#' * **`default_value`** — the initial value assigned when no user value
#'   is supplied.
#'
#' Subclasses typically expose each argument as an active binding that
#' delegates to `$get_argument_value()` and `$set_argument_value()`,
#' providing validation on assignment.
#'
#' ## Rendering
#' `$render()` assembles the command string by evaluating each argument
#' through its `command_format` and interpolating the results into
#' `$template`. If `$template` is a function, it is called with the
#' current argument values as named parameters. Arguments that fail to
#' render are silently omitted. The final output has the form
#' `"{name} {arguments};"`.
#'
#' @seealso
#' * [BasicCommand] — parent class providing the core command interface.
#' * [TreeSearchCommand] — extends this class for tree search commands.
#' * [CommandArgument] — the class used to store individual arguments.
#'
#' @keywords internal
#' @importFrom checkmate assert check_function check_character check_choice check_disjunct check_int check_null check_number check_string check_subset makeAssertCollection test_class test_function test_null test_true test_string
#' @importFrom cli cli_abort
#' @importFrom glue glue glue_data
#' @importFrom R6 R6Class
#' @importFrom stringr str_replace_all
StandardCommand <- R6Class(
  "StandardCommand",
  inherit = BasicCommand,
  private = list(
    .arguments = NULL
  ),
  public = list(
    #' @description
    #' Format the command as a summary table.
    #'
    #' Returns a data frame with one row per argument showing its
    #' description, current value, and default value.
    #'
    #' @param ... Not used.
    #'
    #' @return A `data.frame` with columns for description, current value,
    #'   and default value.
    format = function(...) {
      tbl <- sapply(private$.arguments, function(arg) {
        c(
          paste(arg$description, ":", sep = ""),
          arg$render("pretty", "value"),
          arg$render("pretty", "default_value")
        )
      }) %>%
        t() %>%
        as.data.frame()

      tbl[, 1] <- format(tbl[, 1], justify = "left")
      names(tbl) <- c("", "Current value", "Default value")
      tbl
    },
    #' @description
    #' Retrieve the current value of a command argument.
    #'
    #' @param label \[`character(1)`\]\cr
    #'   The unique label identifying the argument.
    #'
    #' @return The current value of the argument.
    get_argument_value = function(label) {
      all_labels <- sapply(private$.arguments, `[[`, "label")

      val_check <- check_character(all_labels, min.chars = 1, min.len = 1, any.missing = FALSE)
      if (!test_true(val_check)) {
        cli_abort(c("No command arguments have been added.",
          "x" = val_check
        ))
      }

      coll <- makeAssertCollection()
      assert(
        check_string(label, min.chars = 1),
        check_choice(label, all_labels),
        combine = "and"
      )
      if (!coll$isEmpty()) {
        val_check <- str_replace_all(val_check, "[(\\{\\})]", "\\1\\1")
        cli_abort(c("{.arg label} must be a valid label.",
          "x" = val_check
        ))
      }

      arg_idx <- which(label == all_labels)
      return(private$.arguments[[arg_idx]]$value)
    },
    #' @description
    #' Register a new command argument.
    #'
    #' Creates a [CommandArgument] and appends it to the internal argument
    #' list. The `label` must be unique among existing arguments.
    #'
    #' @param label \[`character(1)`\]\cr
    #'   A unique string identifying the argument.
    #' @param description \[`character(1)`\]\cr
    #'   A human-readable description of the argument.
    #' @param command_format \[`character(1)` or `function`\]\cr
    #'   A [glue][glue::glue] string (e.g., `"replic {value}"`) or a
    #'   function returning the formatted command fragment.
    #' @param default_value The default value for the argument, or `NULL`.
    #' @param pretty_format \[`character(1)`, `function`, or `NULL`\]\cr
    #'   Optional glue string or function for human-readable formatting.
    new_argument = function(label, description, command_format,
                            default_value = NULL, pretty_format = NULL) {
      args <- as.list(environment(), all = TRUE)
      all_labels <- sapply(private$.arguments, `[[`, "label")
      if (length(all_labels) > 0) {
        val_check <- check_disjunct(label, all_labels)
        if (!test_true(val_check)) {
          val_check <- str_replace_all(val_check, "([\\{\\}])", "\\1\\1")
          cli_abort("{.arg label} must be a valid string.",
            "x" = val_check
          )
        }
      }

      argument <- do.call(CommandArgument$new, args)
      private$.arguments <- c(
        private$.arguments,
        list(argument)
      )
    },
    #' @description
    #' Render the TNT command string.
    #'
    #' Assembles each argument via its `command_format`, interpolates the
    #' results into `$template`, and returns a string of the form
    #' `"{name} {arguments};"`. Arguments that fail to render are silently
    #' omitted.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT command.
    render = function(...) {
      if (length(private$.arguments) > 0) {
        arguments <- lapply(private$.arguments, function(obj) {
          res <- try(obj$render(), silent = TRUE)
          ifelse(test_class(res, "try-error"), NA, res)
        })
        names(arguments) <- sapply(private$.arguments, `[[`, "label")
        arguments <- arguments[!is.na(arguments)]

        if (test_function(self$template)) {
          data_obj <- lapply(private$.arguments, `[[`, "value")
          names(data_obj) <- sapply(private$.arguments, `[[`, "label")

          arguments <- do.call(self$template, data_obj)
        } else {
          arguments <- sapply(self$template, function(template) {
            res <- try(glue_data(arguments, template), silent = TRUE)
            ifelse(test_class(res, "try-error"), NA, res)
          })
          arguments <- arguments[!is.na(arguments)]
        }


        cmd_string <- glue(
          "{self$name} {arguments};",
          arguments = paste(arguments, collapse = " ")
        )
      } else {
        cmd_string <- glue("{self$name};")
      }
      cmd_string
    },
    #' @description
    #' Set the value of a command argument.
    #'
    #' @param label \[`character(1)`\]\cr
    #'   The unique label identifying the argument.
    #' @param value The value to assign.
    set_argument_value = function(label, value) {
      all_labels <- sapply(private$.arguments, `[[`, "label")

      val_check <- check_character(all_labels, min.chars = 1, any.missing = FALSE, min.len = 1)
      if (!test_true(val_check)) {
        cli_abort(c("No command arguments have been added.",
          "x" = val_check
        ))
      }

      coll <- makeAssertCollection()
      assert(
        check_string(label, min.chars = 1),
        check_choice(label, all_labels),
        combine = "and"
      )
      if (!coll$isEmpty()) {
        val_check <- str_replace_all(val_check, "[(\\{\\})]", "\\1\\1")
        cli_abort(c("{.arg label} must be a valid label.",
          "x" = val_check
        ))
      }

      arg_idx <- which(label == all_labels)
      argument <- private$.arguments[[arg_idx]]
      argument$value <- value
    }
  )
)
