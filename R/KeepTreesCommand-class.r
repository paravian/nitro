#' Keep Trees Command
#'
#' @description
#' An [R6][R6::R6Class] class that trims the TNT tree buffer by discarding
#' all but the last `n` trees currently held in memory.
#'
#' This command is a thin wrapper around the TNT `keep` command. Its
#' primary use within \pkg{nitro} is to flush the tree buffer after the
#' suboptimal tree sampling stage of branch support calculation, ensuring
#' that only the optimal trees — reloaded immediately afterwards via a
#' [ReadTreesCommand] — are present in memory when `bsupport` runs. This
#' is necessary because TNT's `bsupport` command compares all trees
#' currently in the buffer against the optimal trees to determine the
#' minimum score difference required to lose each group; retaining
#' suboptimal trees in the buffer at that point would produce incorrect
#' support values.
#'
#' @details
#' ## Command output
#' `$render()` produces `keep <number>;`. Setting `$number` to `0`
#' discards all trees currently in the buffer.
#'
#' ## Queue integration
#' Unlike most command classes, `$enqueue()` requires an explicit
#' `priority` argument. This is because `KeepTreesCommand` may need to be
#' inserted at different positions in the queue depending on context. When
#' used by [BranchSupportCommand], it is enqueued at priority `504`,
#' placing it after the suboptimal sampling loop (priority `502`--`503`)
#' and before the optimal trees are reloaded (priority `505`).
#'
#' @seealso
#' * [BranchSupportCommand] — the primary consumer of this command,
#'   which enqueues it automatically during branch support calculation.
#' * [ReadTreesCommand] — reloads the optimal trees after the buffer has
#'   been flushed.
#'
#' @keywords internal
#' @importFrom checkmate asInt check_int test_true
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
KeepTreesCommand <- R6Class(
  "KeepTreesCommand",
  inherit = BasicCommand,
  private = list(
    .number = NULL
  ),
  active = list(
    #' @field number \[`integer(1)`\]\cr
    #'   The number of trees to retain in the TNT tree buffer after the
    #'   command executes. Must be zero or a positive integer. Setting this
    #'   to `0` discards all trees currently in the buffer.
    number = function(value) {
      label <- "number"
      if (missing(value)) {
        return(private$.number)
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be zero or a positive integer.",
            "x" = val_check
          ))
        }
        value <- asInt(value)
        private$.number <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Unlike most command classes, an explicit `priority` argument is
    #' required. Priority must be a non-negative integer in the range
    #' `[0, 999]`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #' @param priority \[`integer(1)`\]\cr
    #'   The priority at which to insert this command. Must be a
    #'   non-negative integer no greater than `999`. Lower values execute
    #'   first.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL, priority = NULL) {
      val_check <- check_int(priority, lower = 0, upper = 999)
      if (!test_true(val_check)) {
        cli_abort(c("{.arg priority} must be a non-negative integer no greater than 999.",
          "x" = val_check
        ))
      }
      .queue <- super$enqueue(.queue)
      .queue$add(self, priority)
      .queue
    },
    #' @description
    #' Create a new `KeepTreesCommand` object.
    #'
    #' @param number \[`integer(1)`\]\cr
    #'   The number of trees to retain in the buffer. See the `$number`
    #'   field.
    #'
    #' @return A new `KeepTreesCommand` object.
    initialize = function(number) {
      a <- as.list(environment(), all = TRUE)

      super$initialize(
        name        = "keep",
        description = "Trim the tree buffer to the last n trees"
      )

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Render the TNT `keep` command string.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT
    #'   command.
    render = function(...) {
      paste0(self$name, " ", self$number, ";")
    }
  )
)
