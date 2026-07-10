#' Character Coding Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures character activity and ordering
#' in \pkg{nitro}.
#'
#' This command is created automatically from the `$inactive` and `$ordered`
#' fields of [DiscreteMatrix]. Users do not typically need to instantiate it
#' directly.
#'
#' @details
#' ## Command output
#' `$render()` produces a string of the form
#' `ccode ] {inactive_indices} + {ordered_indices};`
#' where `]` marks inactive characters and `+` marks ordered characters.
#' Either group is omitted when `NULL`. Returns `NULL` when both are
#' `NULL`.
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `410`.
#'
#' @seealso
#' * [DiscreteMatrix] — sets inactive and ordered characters via its
#'   `$inactive` and `$ordered` fields.
#'
#' @keywords internal
#' @importFrom checkmate assert check_null check_integerish makeAssertCollection test_null test_numeric
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
CharacterCodingCommand <- R6Class(
  "CharacterCodingCommand",
  inherit = BasicCommand,
  private = list(
    .inactive_indices = NULL,
    .ordered_indices = NULL
  ),
  active = list(
    #' @field inactive_indices \[`numeric` or `NULL`\]\cr
    #'   Integer indices of characters to mark as inactive, or `NULL` when
    #'   all characters are active. Values are floored to integers.
    inactive_indices = function(value) {
      if (missing(value)) {
        return(private$.inactive_indices)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_integerish(value, lower = 1, any.missing = FALSE, min.len = 1),
          add = coll
        )

        if (!coll$isEmpty()) {
          err <- coll$getMessages()
          cli_abort(c("{.arg inactive_indices} must be either {.val NULL} or a valid numeric vector.",
            "x" = err
          ))
        }

        if (test_numeric(value)) {
          value <- floor(value)
        }
        private$.inactive_indices <- value
      }
    },
    #' @field ordered_indices \[`numeric` or `NULL`\]\cr
    #'   Integer indices of characters to mark as ordered, or `NULL` when
    #'   no characters are ordered. Values are floored to integers. Only
    #'   applicable to matrices with `data_type = "numeric"`.
    ordered_indices = function(value) {
      if (missing(value)) {
        return(private$.ordered_indices)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_integerish(value, lower = 1, any.missing = FALSE, min.len = 1),
          add = coll
        )

        if (!coll$isEmpty()) {
          err <- coll$getMessages()
          cli_abort(c("{.arg ordered_indices} must be either {.val NULL} or a valid numeric vector.",
            "x" = err
          ))
        }

        if (test_numeric(value)) {
          value <- floor(value)
        }
        private$.ordered_indices <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Adds this command at priority `410`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(self, 410)
      .queue
    },
    #' @description
    #' Format the command as a summary table.
    #'
    #' @param ... Not used.
    #'
    #' @return A `data.frame` showing the number of inactive and ordered
    #'   characters.
    format = function(...) {
      counts <- list(
        private$.inactive_indices,
        private$.ordered_indices
      ) %>%
        sapply(function(x) {
          ifelse(test_null(x), 0, length(x))
        })

      options <- data.frame(
        c("Inactive character indices:", "Ordered character indices:"),
        counts
      )

      names(options) <- c("", "Current value")
      options
    },
    #' @description
    #' Create a new `CharacterCodingCommand` object.
    #'
    #' This command is created automatically from [DiscreteMatrix]. Direct
    #' instantiation is rarely necessary.
    #'
    #' @param inactive_indices \[`numeric` or `NULL`\]\cr
    #'   Indices of characters to mark as inactive (default: `NULL`). See
    #'   the `$inactive_indices` field.
    #' @param ordered_indices \[`numeric` or `NULL`\]\cr
    #'   Indices of characters to mark as ordered (default: `NULL`). See
    #'   the `$ordered_indices` field.
    #' @param ... Optional named arguments passed to the constructor of the
    #'   command class.
    #'
    #' @return A new `CharacterCodingCommand` object.
    initialize = function(inactive_indices = NULL, ordered_indices = NULL,
                          ...) {
      a <- as.list(environment(), all = TRUE) %>%
        head(-1)

      super$initialize(
        name = "ccode",
        description = "Set status codes for characters",
        provides = "character coding",
        ...
      )

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Render the TNT `ccode` command string.
    #'
    #' Returns `NULL` when both `$inactive_indices` and `$ordered_indices`
    #' are `NULL`, since no command needs to be issued in that case.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT
    #'   command, or `NULL` if no characters are inactive or ordered.
    render = function(...) {
      args <- NULL

      if (!test_null(private$.inactive_indices)) {
        args <- c(
          args,
          paste(c("]", private$.inactive_indices), collapse = " ")
        )
      }

      if (!test_null(private$.ordered_indices)) {
        args <- c(
          args,
          paste(c("+", private$.ordered_indices), collapse = " ")
        )
      }

      cmd <- NULL
      if (!test_null(args)) {
        cmd <- paste(self$name, " ", args, ";", sep = "")
      }
      cmd
    }
  )
)
