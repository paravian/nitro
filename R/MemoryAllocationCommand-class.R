#' Memory Allocation Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures the amount of memory allocated
#' to TNT during execution.
#'
#' This command is created automatically as part of the standard
#' [CommandQueue] setup. Users do not typically need to instantiate it
#' directly.
#'
#' @details
#' ## Command output
#' `$render()` produces a string of the form `mxram {size};` where `{size}`
#' is the number of megabytes to allocate.
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `140`.
#'
#' @keywords internal
#' @importFrom checkmate asInt check_integerish test_true
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
MemoryAllocationCommand <- R6Class(
  "MemoryAllocationCommand",
  inherit = BasicCommand,
  private = list(
    .size = NULL
  ),
  active = list(
    #' @field size \[`integer(1)`\]\cr
    #'   The amount of memory to allocate, in megabytes. Must be a
    #'   positive integer.
    size = function(value) {
      label <- "size"
      if (missing(value)) {
        return(private$.size)
      } else {
        val_check <- check_integerish(value)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be an integer.",
            "x" = val_check
          ))
        }

        value <- asInt(value)
        private$.size <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Adds this command at priority `140`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(self, 140)
      .queue
    },
    #' @description
    #' Create a new `MemoryAllocateCommand` object.
    #'
    #' @param size \[`integer(1)`\]\cr
    #'   The amount of memory to allocate in megabytes. See the `$size`
    #'   field.
    #'
    #' @return A new `MemoryAllocateCommand` object.
    initialize = function(size) {
      a <- as.list(environment(), all = TRUE)

      super$initialize(
        name = "mxram",
        description = "Memory allocation"
      )

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Render the TNT `mxram` command string.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT
    #'   command.
    render = function(...) {
      cmd <- paste(self$name, " ", self$size, ";", sep = "")
      cmd
    }
  )
)
