#' Outgroup Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures the outgroup taxon for a
#' phylogenetic analysis in \pkg{nitro}.
#'
#' This command is created automatically by [TreeAnalysis] from the
#' `$outgroup` field. Users do not typically need to instantiate it
#' directly.
#'
#' @details
#' ## Command output
#' `$render()` produces a string of the form `outgroup {taxon_name};`
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `430`.
#'
#' @seealso
#' * [TreeAnalysis] — sets the outgroup via its `$outgroup` field.
#'
#' @keywords internal
#' @importFrom checkmate check_flag check_string test_true
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
OutgroupCommand <- R6Class(
  "OutgroupCommand",
  inherit = BasicCommand,
  private = list(
    .taxon_name = NULL
  ),
  active = list(
    #' @field taxon_name \[`character(1)`\]\cr
    #'   The name of the outgroup taxon.
    taxon_name = function(value) {
      label <- "taxon_name"
      if (missing(value)) {
        return(private$.taxon_name)
      } else {
        val_check <- check_string(value)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be a string.",
            "x" = val_check
          ))
        }

        private$.taxon_name <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Adds this command at priority `430`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(self, 430)
      .queue
    },
    #' @description
    #' Format the command as a summary table.
    #'
    #' @param ... Not used.
    #'
    #' @return A `data.frame` showing the current outgroup taxon name.
    format = function(...) {
      options <- data.frame(
        "Taxon name:",
        self$taxon_name
      )

      names(options) <- c("", "Current value")
      options
    },
    #' @description
    #' Create a new `OutgroupCommand` object.
    #'
    #' This command is created automatically by [TreeAnalysis]. Direct
    #' instantiation is rarely necessary.
    #'
    #' @param taxon_name \[`character(1)`\]\cr
    #'   The name of the outgroup taxon. See the `$taxon_name` field.
    #' @param ... Optional named arguments passed to the constructor of the
    #'   command class.
    #'
    #' @return A new `OutgroupCommand` object.
    initialize = function(taxon_name, ...) {
      a <- as.list(environment(), all = TRUE) %>%
        head(-1)

      super$initialize(
        name = "outgroup",
        description = "Outgroup taxon",
        provides = "outgroup",
        ...
      )

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Render the TNT `outgroup` command string.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT
    #'   command.
    render = function(...) {
      cmd <- paste("outgroup ", self$taxon_name, ";", sep = "")
      cmd
    }
  )
)
