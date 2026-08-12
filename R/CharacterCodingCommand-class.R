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
#' @importFrom checkmate assert check_integer check_numeric check_true makeAssertCollection test_null test_numeric
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
CharacterCodingCommand <- R6Class(
  "CharacterCodingCommand",
  inherit = BasicCommand,
  private = list(
    .inactive_indices = NULL,
    .ordered_indices = NULL
  ),
  active = list(
    #' @field inactive_indices \[`integer`\]\cr
    #'   Integer indices of characters to mark as inactive.
    inactive_indices = function(value) {
      if (missing(value)) {
        return(private$.inactive_indices)
      } else {
        coll <- makeAssertCollection()

        assert(
          assert(
            check_numeric(value, lower = 1, any.missing = FALSE, unique = TRUE),
            check_integer(value, lower = 1, any.missing = FALSE, unique = TRUE)
          ),
          check_true(
            all(value == as.integer(value))
          ),
          combine = "and",
          add = coll
        )

        if (!coll$isEmpty()) {
          val_check <- coll$getMessages()
          cli_abort(c("{.arg inactive_indices} must be a valid integer vector.",
            "x" = val_check
          ))
        }

        if (test_numeric(value)) {
          value <- as.integer(value)
        }
        private$.inactive_indices <- value
      }
    },
    #' @field ordered_indices \[`integer`\]\cr
    #'   Integer indices of characters to mark as ordered. Only applicable to
    #'   matrices with `data_type = "numeric"`.
    ordered_indices = function(value) {
      if (missing(value)) {
        return(private$.ordered_indices)
      } else {
        coll <- makeAssertCollection()

        assert(
          assert(
            check_numeric(value, lower = 1, any.missing = FALSE, unique = TRUE),
            check_integer(value, lower = 1, any.missing = FALSE, unique = TRUE)
          ),
          check_true(
            all(value == as.integer(value))
          ),
          combine = "and",
          add = coll
        )

        if (!coll$isEmpty()) {
          val_check <- coll$getMessages()
          cli_abort(c("{.arg ordered_indices} must be a valid numeric vector.",
            "x" = val_check
          ))
        }

        if (test_numeric(value)) {
          value <- as.integer(value)
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
      ) |>
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
    #' @param inactive_indices \[`integer`\]\cr
    #'   Indices of characters to mark as inactive. See the `$inactive_indices`
    #'   field.
    #' @param ordered_indices \[`integer`\]\cr
    #'   Indices of characters to mark as ordered. See the `$ordered_indices`
    #'   field.
    #' @param ... Optional named arguments passed to the constructor of the
    #'   command class.
    #'
    #' @return A new `CharacterCodingCommand` object.
    initialize = function(inactive_indices = integer(0),
                          ordered_indices = integer(0),
                          ...) {
      if (length(inactive_indices) == 0 & length(ordered_indices) == 0) {
        cli_abort(c("{.arg inactive_indices} and {.arg ordered_indices} cannot both be unspecified."))
      }

      a <- as.list(environment(), all = TRUE) |>
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
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT
    #'   command, or `NULL` if no characters are inactive or ordered.
    render = function(...) {
      args <- NULL

      if (!length(private$.inactive_indices) == 0) {
        args <- c(
          args,
          paste(c("]", private$.inactive_indices), collapse = " ")
        )
      }

      if (!length(private$.ordered_indices) == 0) {
        args <- c(
          args,
          paste(c("+", private$.ordered_indices), collapse = " ")
        )
      }

      if (!test_null(args)) {
        cmd <- paste(self$name, " ", paste(args, collapse = " "), ";", sep = "")
      }
      cmd
    }
  )
)
