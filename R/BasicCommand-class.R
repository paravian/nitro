#' Basic TNT Command Base Class
#'
#' @description
#' `BasicCommand` is the root [R6][R6::R6Class] class for all TNT command
#' objects in \pkg{nitro}. It establishes the core interface that every
#' command must support: a name, a description, dependency management,
#' rendering, and queue integration.
#'
#' This class is not exported. It is intended to be subclassed by
#' [StandardCommand] or other intermediate classes.
#'
#' @details
#' ## Subclassing
#' Subclasses should call `super$initialize(name, description)` in their
#' own `initialize()` method. The `$name` and `$description` fields become
#' read-only after the first assignment. All other fields, including
#' `$provides`, can be reassigned freely after construction.
#'
#' The following methods are designed to be overridden:
#' * **`$render()`** — must return a character string containing the
#'   formatted TNT command. The base implementation returns invisibly.
#' * **`$format()`** — should return a human-readable summary of the
#'   command's current configuration. The base implementation returns
#'   invisibly.
#' * **`$enqueue(.queue)`** — should add the command (and any follow-up
#'   commands) to a [CommandQueue]. The base implementation creates or
#'   passes through a queue without adding anything.
#'
#' ## Dependency system
#' Commands can declare named dependencies via `$new_dependency()`. Each
#' dependency has:
#' * A `name` — used as the key in the `$dependencies` list.
#' * A `required` flag — determines whether the dependency appears in
#'   `$requires` (`TRUE`) or `$optional` (`FALSE`).
#' * A `callback` function — applied to the value when the dependency is
#'   set via `$set_dependency()`, allowing validation or transformation.
#'
#' ## Inline commands
#' Commands with `$inline` set to `TRUE` are not written directly into the
#' TNT script during execution. Instead, their rendered output is written
#' to a separate temporary file and a `proc <filename>;` call is
#' substituted in the script at that position. This is intended for
#' commands such as [ReadTreesCommand] that may produce very long output,
#' and keeps the main script readable.

#' @seealso
#' * [StandardCommand] — extends this class with the argument and template
#'   system.
#' * [c.BasicCommand()] — combines commands into a `CommandList`.
#' * [TntInterface] — reads `$inline` during script assembly.
#' * [ReadTreesCommand] — an example of a command that sets `$inline` to
#'   `TRUE`.
#'
#' @keywords internal
#' @importFrom checkmate assert check_character check_choice check_class check_disjunct check_flag check_function check_int check_null check_number check_string check_subset makeAssertCollection test_function test_null test_true test_string
#' @importFrom cli cli_abort cli_text col_grey col_red style_italic
#' @importFrom glue glue glue_data
#' @importFrom R6 R6Class
#' @importFrom stringr str_replace_all str_trim str_split_1
BasicCommand <- R6Class(
  "BasicCommand",
  private = list(
    .description = NULL,
    .dependencies = NULL,
    .inline = NULL,
    .name = NULL,
    .outputs = NULL,
    .provides = NULL,
    .template = NULL
  ),
  active = list(
    #' @field dependencies \[`list`\]\cr
    #'   *(Read-only.)* Named list of the dependencies associated with the
    #'   command.
    dependencies = function(value) {
      if (missing(value)) {
        return(private$.dependencies)
      }
      cli_abort(c("{.var dependencies} is a read-only attribute."))
    },
    #' @field description \[`character(1)`\]\cr
    #'   *(Write-once.)* A description of the function performed by the command.
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
    #' @field inline \[`logical(1)`]\cr
    #'   Whether this command should be written to a separate temporary file
    #'   during TNT script generation, with a `proc <filename>;` call
    #'   substituted in its place.
    #'
    #'   Set to `TRUE` for commands that produce very long output (e.g.,
    #'   [ReadTreesCommand]) to keep the main TNT script readable. When
    #'   `FALSE` (default), the command is written directly into the script.
    #'
    #'   This field is read by [TntInterface]`$execute()` during script
    #'   assembly.
    inline = function(value) {
      if (missing(value)) {
        return(private$.inline)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg inline} must be a logical value.",
                      "x" = val_check
          ))
        }

        private$.inline <- value
      }
    },
    #' @field is_resolved \[`logical(1)`]\cr
    #'
    #'   Whether the command's dependencies are resolved. Returns `TRUE` if they
    #'   are resolved; otherwise returns `FALSE`.
    is_resolved = function(value) {
      if (missing(value)) {
        resolved <- TRUE
        for (dep in self$dependencies) {
          if (dep$required == TRUE) {
            if (test_null(dep$value)) {
              resolved <- resolved & FALSE
            } else {
              resolved <- resolved & dep$value$is_resolved
            }
          }
        }

        resolved
      } else {
        cli_abort(c("{.arg description} is a read-only attribute."))
      }
    },
    #' @field name \[`character(1)`\]\cr
    #'   *(Write-once.)* The name of the command.
    name = function(value) {
      if (missing(value)) {
        return(private$.name)
      } else {
        if (test_null(private$.name)) {
          val_check <- check_string(value, min.chars = 1)
          if (!test_true(val_check)) {
            cli_abort(c("{.arg name} must be a string.",
              "x" = val_check
            ))
          }
          private$.name <- value
        } else {
          cli_abort(c("{.arg name} is a read-only attribute."))
        }
      }
    },
    #' @field optional \[`character`\]\cr
    #'   *(Read-only.)* The names of dependencies optionally required by the
    #'   command. Derived from dependencies where `required` is `FALSE`.
    optional = function(value) {
      if (missing(value)) {
        optional <- NULL
        dep_mask <- sapply(self$dependencies, getElement, "required")
        if (length(dep_mask) > 0) {
          if (sum(!dep_mask) > 0) {
            optional <- names(dep_mask)[!dep_mask]
          }
        }
        return(optional)
      }
      cli_abort(c("{.var optional} is a read-only attribute."))
    },
    #' @field outputs \[`character`\]\cr
    #'   *(Read-only.)* The type of output produced by the command.
    outputs = function(value) {
      if (missing(value)) {
        return(private$.outputs)
      } else {
        if (test_null(private$.outputs)) {
          val_check <- check_string(value, min.chars = 1)
          if (!test_true(val_check)) {
            cli_abort(c("{.arg outputs} must be a string.",
                        "x" = val_check
            ))
          }
          private$.outputs <- value
        } else {
          cli_abort(c("{.arg outputs} is a read-only attribute."))
        }
      }
    },
    #' @field provides \[`character(1)` or `NULL`\]\cr
    #'   The dependency token satisfied by this command. Used by
    #'   [CommandQueue] during dependency resolution to match against the
    #'   `$requires` and `$optional` fields of other commands in the queue.
    #'
    #'   Must be a non-empty string or `NULL`. Can be set at any time;
    #'   reassignment replaces the previous value.
    provides = function(value) {
      if (missing(value)) {
        return(private$.provides)
      } else {
        if (test_null(private$.provides)) {
          coll <- makeAssertCollection()

          assert(
            check_null(value),
            check_string(value, min.chars = 1),
            add = coll
          )

          if (!coll$isEmpty()) {
            val_check <- coll$getMessages()
            cli_abort(c("{.arg provides} must be either a string or {.val NULL}.",
                        "x" = val_check
            ))
          }
          private$.provides <- value
        } else {
          cli_abort(c("{.arg provides} is a read-only attribute."))
        }
      }
    },
    #' @field requires \[`character`\]\cr
    #'   *(Read-only.)* The names of dependencies required by the command.
    #'   Derived from dependencies where `required` is `TRUE`.
    requires = function(value) {
      if (missing(value)) {
        requires <- NULL
        dep_mask <- sapply(self$dependencies, getElement, "required")
        if (length(dep_mask) > 0) {
          if (sum(dep_mask) > 0) {
            requires <- names(dep_mask)[dep_mask]
          }
        }
        return(requires)
      }
      cli_abort(c("{.var requires} is a read-only attribute."))
    },
    #' @field template \[`character` or `function`\]\cr
    #'   *(Write-once.)* A character vector of [glue][glue::glue] strings or a
    #'   function used to assemble the command arguments.
    template = function(value) {
      if (missing(value)) {
        return(private$.template)
      } else {
        if (test_null(private$.template)) {
          coll <- makeAssertCollection()
          assert(
            check_character(value, min.chars = 1, min.len = 1, any.missing = FALSE),
            check_function(value),
            add = coll
          )

          if (!coll$isEmpty()) {
            err <- coll$getMessages()
            cli_abort(c("{.arg template} must be either a character vector of a function.",
                        "x" = err
            ))
          }
          private$.template <- value
        } else {
          cli_abort(c("{.arg template} is a read-only attribute."))
        }
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' The base implementation creates a new [CommandQueue] if none is
    #' supplied, or passes the existing queue through unchanged. Subclasses
    #' should override this method to add themselves (and any follow-up
    #' commands) to the queue.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      coll <- makeAssertCollection()

      val_check <- assert(
        check_class(.queue, "CommandQueue"),
        check_null(.queue)
      )

      if (!coll$isEmpty()) {
        val_check <- coll$getMessages()
        cli_abort(c("{.arg .queue} must be a {.cls CommandQueue} object.",
                    "x" = val_check))
      }

      if (test_null(.queue)) {
        .queue = CommandQueue$new()
      }

      .queue
    },
    #' @description
    #' Format the command as a summary table.
    #'
    #' Returns a data frame with one row per argument showing its description
    #' and value.
    #'
    #' @param ... Not used.
    #'
    #' @return A `data.frame` with columns for description, current value,
    #'   and default value.
    format = function(...) {
      invisible()
    },
    #' @description
    #' Retrieve the value of a named dependency.
    #'
    #' @param name A string identifying the dependency.
    #'
    #' @return The current value of the dependency.
    get_dependency = function(name) {
      dep_choices <- names(self$dependencies)
      val_check <- check_choice(name, choices = dep_choices)
      if (!test_true(val_check)) {
        cli_abort(
          c(
            "{.arg name} must be a valid choice.",
            "x" = str_replace_all(val_check, "([{}])", "\\1\\1")
          )
        )
      }

      self$dependencies[[name]]$value
    },
    #' @description
    #' Create a new `BasicCommand` object.
    #'
    #' Sets the command `$name` and `$description` (both become read-only after
    #' this call), initialises an empty dependency list, and sets `$outputs` to
    #' `"text"`.
    #'
    #' @param name \[`character(1)`\]\cr
    #'   The TNT command name. See the `$name` field.
    #' @param description \[`character(1)`\]\cr
    #'   A human-readable description. See the `$description` field.
    #' @param inline \[`logical(1)`\]\cr
    #'   Whether to write this command to a separate file during script
    #'   assembly (default: `FALSE`). See the `$inline` field.
    #' @param outputs \[`character(1)`\]\cr
    #'   The type of output produced by the command (default: `"text"`).
    #'   See the `$outputs` field.
    #' @param provides \[`character(1)` or `NULL`\]\cr
    #'   The dependency token satisfied by this command (default: `NULL`).
    #'   See the `$provides` field.
    #'
    #' @return A new `BasicCommand` object.
    initialize = function(name, description, inline = FALSE, outputs = "text",
                          provides = NULL) {
      a <- as.list(environment(), all = TRUE)

      private$.dependencies <- list()

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Register a new named dependency.
    #'
    #' @param name \[`character(1)`\]\cr
    #'   A unique string identifying the dependency.
    #' @param required \[`logical(1)`\]\cr
    #'   Whether the dependency must be met for correct command rendering.
    #' @param callback \[`function`\]\cr
    #'   A function applied to the value when the dependency is set. Takes
    #'   one argument and returns the (possibly transformed) value.
    new_dependency = function(name, required, callback) {
      coll <- makeAssertCollection()

      all_names <- sapply(self$dependencies, getElement, "name") %>%
        names()
      if (length(all_names) == 0) {
        all_names <- character(0)
      }
      assert(
        check_string(name, min.chars = 1),
        check_disjunct(name, all_names),
        combine = "and",
        add = coll
      )

      if (!coll$isEmpty()) {
        val_check <- coll$getMessages()
        cli_abort(c("{.arg name} must be a valid string.",
                    "x" = str_replace_all(val_check, "([{}])", "\\1\\1")))
      }

      val_check <- check_flag(required)
      if (!test_true(val_check)) {
        cli_abort(c("{.arg required} must be a logical value.",
                    "x" = val_check))
      }

      val_check <- check_function(callback)
      if (!test_true(val_check)) {
        cli_abort(c("{.arg callback} must be a function.",
                    "x" = val_check))
      }

      private$.dependencies[[name]] <- list(
        required = required,
        callback = callback
      )
    },
    #' @description
    #' Print a brief summary of the command.
    #'
    #' @param ... Not used.
    print = function(...) {
      cli_text(col_grey("# A ", style_italic(col_red("nitro")), " TNT command"))
      options <- c(
        "Name:" = self$name,
        "Description:" = self$description
      )

      options <- data.frame(options)
      names(options) <- NULL
      print(options)
    },
    #' @description
    #' Render the TNT command string.
    #'
    #' The base implementation returns invisibly. Subclasses should override
    #' this to produce a formatted command string.
    #'
    #' @param ... Not used.
    #'
    #' @return Invisible `NULL` (overridden in subclasses).
    render = function(...) {
      invisible()
    },
    #' @description
    #' Set the value of a named dependency.
    #'
    #' The value is passed through the dependency's `callback` function
    #' before being stored.
    #'
    #' @param name \[`character(1)`\]\cr
    #'   The name of the dependency to set.
    #' @param value The object to assign to the dependency.
    set_dependency = function(name, value) {
      dep_choices <- names(self$dependencies)
      val_check <- check_choice(name, choices = dep_choices)
      if (!test_true(val_check)) {
        cli_abort(
          c(
            "{.arg name} must be a valid choice.",
            "x" = str_replace_all(val_check, "([{}])", "\\1\\1")
          )
        )
      }

      dep <- self$dependencies[[name]]

      private$.dependencies[[name]]$value <- dep$callback(value)
    },
    #' @description
    #' Clean raw output generated by TNT.
    #'
    #' Be default, trims whitespace and splits on newline characters and removes
    #' empty lines.
    #'
    #' @param output \[`character`\]\cr
    #'   A character vector of raw command output from the TNT executable.
    #'
    #' @return A character vector of trimmed, non-empty lines.
    transform = function(output) {
      val_check <- check_character(output, min.chars = 1, any.missing = FALSE, min.len = 1)
      if (!test_true(val_check)) {
        cli_abort(c("{.arg output} must be a valid character vector.",
                    "x" = val_check))
      }

      output <- str_trim(output) %>%
        str_split_1("[\n\r]+") %>%
        str_trim() %>%
        .[nchar(.) > 0]
      output
    }
  )
)

#' Combine BasicCommand Objects into a CommandList
#'
#' @description
#' Combines one or more [BasicCommand] objects into a `CommandList`.
#'
#' @param ... One or more [BasicCommand] objects.
#'
#' @return A `CommandList` object (a named list with class
#'   `c("CommandList", "list")`).
#'
#' @seealso
#' * [BasicCommand] — the base class for all command objects.
#' * [TreeAnalysis]`$add_command()` — accepts `CommandList` objects.
#'
#' @importFrom checkmate check_list test_true
#' @importFrom cli cli_abort
#' @exportS3Method
#' @keywords internal
c.BasicCommand <- function(...) {
  objs <- list(...)
  val_check <- check_list(objs, types = "BasicCommand")
  if (!test_true(val_check)) {
    cli_abort(c("All objects must inherit from class {.cls BasicCommand}.",
              "x" = val_check)
    )
  }
  class(objs) <- c("CommandList", "list")
  objs
}

#' Combine CommandList Objects
#'
#' @description
#' Flattens and combines one or more `CommandList` objects into a single
#' `CommandList`.
#'
#' @param ... One or more `CommandList` objects.
#'
#' @return A `CommandList` object.
#'
#' @importFrom checkmate check_list test_true
#' @importFrom cli cli_abort
#' @exportS3Method
#' @keywords internal
c.CommandList <- function(...) {
  objs <- list(...)
  objs <- unlist(objs)
  do.call(c, objs)
}

#' Print a CommandList Object
#'
#' @param x A `CommandList` object.
#' @param ... Ignored.
#'
#' @importFrom cli cli_text col_grey col_red style_italic
#' @exportS3Method
#' @keywords internal
print.CommandList <- function(x, ...) {
  cli_text(col_grey("# A list of ", style_italic(col_red("nitro")), " TNT commands"))

  out <- data.frame(length(x))
  rownames(out) <- "Number of commands:"

  names(out) <- NULL
  print(out)
}
