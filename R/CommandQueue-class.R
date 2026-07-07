#' Command Queue
#'
#' @description
#' An [R6][R6::R6Class] class that stores an ordered list of TNT commands
#' for sequential execution.
#'
#' Commands are added via `$add()` with a numeric priority. The queue
#' maintains commands in ascending priority order (lower numbers execute
#' first). Commands are consumed one at a time via `$read_next()`.
#'
#' Dependency resolution is performed automatically as commands are added.
#' Each command's required and optional dependencies are matched against
#' the `$provides` tokens of commands already in the queue, and
#' `$set_dependency()` is called on the dependent command when a match is
#' found. The `$is_resolved` field reflects whether all required
#' dependencies in the queue are currently satisfied.
#'
#' `CommandQueue` is the only command-related class that is exported.
#' Users interact with it primarily through [BasicCommand]`$enqueue()` and
#' the [as.character.CommandQueue()] method, which renders all queued
#' commands to a character vector of TNT command strings.
#'
#' @details
#' ## Typical usage
#' In normal use, a queue is created implicitly by calling `$enqueue()`
#' on a command object without supplying an existing queue:
#'
#' ```r
#' queue <- my_command$enqueue()
#' tnt_script <- as.character(queue)
#' ```
#'
#' Commands can also be added manually:
#'
#' ```r
#' queue <- CommandQueue$new()
#' queue$add(my_command, priority = 500)
#' ```
#'
#' ## Priority ordering
#' Commands are sorted by priority on insertion. When two commands share
#' the same priority, they are ordered by insertion order. Lower priority
#' numbers execute first.
#'
#' ## Dependency resolution
#' Dependency resolution runs incrementally as each command is added via
#' `$add()`. When a new command is inserted:
#'
#' 1. Its required and optional dependencies are checked against the
#'    `$provides` tokens of all commands currently in the queue.
#' 2. If a match is found, `$set_dependency()` is called on the dependent
#'    command immediately.
#' 3. After insertion, any previously unresolved commands in the queue are
#'    re-checked in case the newly added command satisfies their
#'    dependencies.
#'
#' The `$is_resolved` field is updated after every insertion and reflects
#' whether all required dependencies across all commands in the queue are
#' currently satisfied. [execute_analysis()] checks this field before
#' passing the queue to [TntInterface]`$execute()` and aborts if any
#' required dependencies remain unmet.
#'
#' In normal use, dependency resolution is handled transparently by each
#' command's `$enqueue()` method, which adds the command and any
#' prerequisites to the queue in the correct order.
#'
#' @seealso
#' * [BasicCommand] — all command objects support `$enqueue()`, which
#'   adds them to a queue and triggers dependency resolution.
#' * [as.character.CommandQueue()] — renders all queued commands to a
#'   character vector.
#' * [execute_analysis()] — checks `$is_resolved` before executing the
#'   queue.
#'
#' @importFrom checkmate check_class check_int check_subset test_null test_true
#' @importFrom cli cli_abort cli_text col_grey col_red
#' @importFrom magrittr extract not use_series
#' @importFrom R6 R6Class
#' @keywords internal
CommandQueue <- R6Class(
  "CommandQueue",
  private = list(
    .commands = list(),
    .is_resolved = FALSE,
  ),
  active = list(
    #' @field is_resolved \[`logical(1)`\]\cr
    #'   *(Read-only.)* Whether all required dependencies across all
    #'   commands currently in the queue are satisfied. Updated
    #'   automatically after every call to `$add()`. [execute_analysis()]
    #'   checks this field before passing the queue to
    #'   [TntInterface]`$execute()`.
    is_resolved = function(value) {
      if (missing(value)) {
        return(private$.is_resolved)
      }
      cli_abort(c("{.arg resolved} is a read-only field."))
    }
  ),
  public = list(
    #' @description
    #' Add a command to the queue and attempt to resolve its dependencies.
    #'
    #' After insertion the queue is re-sorted by priority. Dependency
    #' resolution then runs in two passes:
    #'
    #' 1. The newly added command's required and optional dependencies are
    #'    matched against the `$provides` tokens of all other commands in
    #'    the queue.
    #' 2. Any commands that were previously unresolved are re-checked in
    #'    case the newly added command satisfies their dependencies.
    #'
    #' The `$is_resolved` field is updated to reflect whether all required
    #' dependencies across the entire queue are now satisfied.
    #'
    #' @param command \[`BasicCommand`\]\cr
    #'   An object inheriting from [BasicCommand].
    #' @param priority \[`integer(1)`\]\cr
    #'   A non-negative integer controlling execution order. Lower values
    #'   execute first.
    add = function(command, priority) {
      val_check <- check_class(command, "BasicCommand")
      if (!test_true(val_check)) {
        cli_abort("{.arg command} must be an object that inherits from {.cls BasicCommand}.",
                  "x" = val_check)
      }

      val_check <- check_int(priority, lower = 0)
      if (!test_true(val_check)) {
        cli_abort("{.arg priority} must be a positive integer.",
                  "x" = val_check)
      }

      new_cmd <- list(
        command = command,
        priority = priority,
        resolved = private$do_resolve(command)
      )

      private$.commands <- c(
        private$.commands,
        list(new_cmd)
      )

      if (self$length() > 1) {
        all_priorities <- sapply(private$.commands, getElement, "priority")
        private$.commands <- private$.commands[order(all_priorities)]
      }

      unres <- sapply(private$.commands, getElement, "resolved") %>%
        not()

      optional <- sapply(private$.commands, function(x) {
        cmd <- x$command
        res <- FALSE

        if (!test_null(cmd$optional)) {
          res <- sapply(cmd$optional, function (y) {
            test_null(cmd$get_dependency(y))
          }) %>%
            any()
        }

        res
      })

      unres <- unres | optional

      if (any(unres)) {
        for (idx in which(unres)) {
          unres_cmd <- private$.commands[[idx]]
          if (!identical(command, unres_cmd$command)) {
            is_res <- private$do_resolve(unres_cmd$command)

            if (is_res) {
              unres_cmd$resolved <- TRUE
              unres[idx] <- FALSE
            }
          }
        }
      }

      private$.is_resolved <- all(!unres)
    },
    #' @description
    #' Return the number of commands currently in the queue.
    #'
    #' @param ... Not used.
    #'
    #' @return An integer.
    length = function(...) {
      length(private$.commands)
    },
    #' @description
    #' Print a brief summary of the queue.
    #'
    #' @param ... Not used.
    print = function(...) {
      cli_text(col_grey("# A ", col_red("nitro"), " command queue"))

      config <- c(
        "Queue length:" = self$length(),
        "Resolved:" = self$is_resolved
      ) %>%
        data.frame()
      names(config) <- NULL
      print(config)
    },
    #' @description
    #' Remove and return the next command from the queue.
    #'
    #' Commands are returned in priority order (lowest priority number
    #' first). Raises an error if the queue is empty.
    #'
    #' @param ... Not used.
    #'
    #' @return A [BasicCommand] object.
    read_next = function(...) {
      if (self$length() == 0) {
        cli_abort(c("Queue is empty, no more commands to read."))
      }
      item <- private$.commands[[1]]
      private$.commands[[1]] <- NULL
      item$command
    }
  )
)

#' Render a CommandQueue to a Character Vector
#'
#' @description
#' Consumes all commands in a [CommandQueue] in priority order and returns
#' their rendered TNT command strings as a character vector.
#'
#' Note that this operation is **destructive** — all commands are removed
#' from the queue as they are read. The queue will be empty after this
#' call.
#'
#' It is the caller's responsibility to ensure that `$is_resolved` is
#' `TRUE` before calling this function. [execute_analysis()] performs this
#' check automatically.
#'
#' @param x A [CommandQueue] object.
#' @param ... Not used.
#'
#' @return A character vector of TNT command strings, one element per
#'   command.
#'
#' @seealso
#' * [CommandQueue] — the queue class, including details on dependency
#'   resolution and `$is_resolved`.
#' * [BasicCommand]`$render()` — produces the string for each individual
#'   command.
#' * [execute_analysis()] — the primary entry point, which checks
#'   `$is_resolved` before rendering.
#'
#' @exportS3Method
#' @keywords internal
as.character.CommandQueue <- function(x, ...) {
  all_cmds <- character(0)

  while (x$length() > 0) {
    next_cmd <- x$read_next()
    all_cmds <- c(
      all_cmds,
      next_cmd$render()
    )
  }

  all_cmds
}
