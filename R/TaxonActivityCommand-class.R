#' Taxon Activity Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures taxon activity (i.e., which
#' taxa are excluded from the analysis) in \pkg{nitro}.
#'
#' This command is created automatically by [TreeAnalysis] from the
#' `$inactive_taxa` field. Users do not typically need to instantiate it
#' directly.
#'
#' @details
#' ## Command output
#' `$render()` produces a string of the form
#' `taxcode - {taxon_1} {taxon_2} ...;`
#' listing all inactive taxa.
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `410`.
#'
#' @seealso
#' * [TreeAnalysis] — sets inactive taxa via its `$inactive_taxa` field.
#'
#' @keywords internal
#' @importFrom checkmate assert check_null check_string makeAssertCollection test_null
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
TaxonActivityCommand <- R6Class(
  "TaxonActivityCommand",
  inherit = BasicCommand,
  private = list(
    .inactive_taxa = NULL
  ),
  active = list(
    #' @field inactive_taxa \[`character`\]\cr
    #'   A character string of space-separated taxon names to mark as
    #'   inactive.
    inactive_taxa = function(value) {
      if (missing(value)) {
        return(private$.inactive_taxa)
      } else {
        val_check <- check_string(value, min.chars = 1)

        if (!val_check) {
          cli_abort(c("{.arg inactive_taxa} must be either {.val NULL} or a valid string.",
            "x" = val_check
          ))
        }

        private$.inactive_taxa <- value
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
    #' @return A `data.frame` showing the number of inactive taxa.
    format = function(...) {
      options <- data.frame(
        c("Inactive taxa count:"),
        length(self$inactive_taxa)
      )

      names(options) <- c("", "Current value")
      options
    },
    #' @description
    #' Create a new `TaxonActivityCommand` object.
    #'
    #' This command is created automatically by [TreeAnalysis]. Direct
    #' instantiation is rarely necessary.
    #'
    #' @param inactive_taxa \[`character` or `NULL`\]\cr
    #'   A string of taxon names to mark as inactive. See the `$inactive_taxa`
    #'   field.
    #' @param ... Optional named arguments passed to the constructor of the
    #'   command class.
    #'
    #' @return A new `TaxonActivityCommand` object.
    initialize = function(inactive_taxa, ...) {
      a <- as.list(environment(), all = TRUE) %>%
        head(-1)

      super$initialize(
        name = "taxcode",
        description = "Set activity status for taxa",
        provides = "taxon activity",
        ...
      )

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Render the TNT `taxcode` command string.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT
    #'   command, or `NULL` if all taxa are active.
    render = function(...) {
      args <- paste(c("-", private$.inactive_taxa), collapse = " ")

      cmd <- paste(self$name, " ", args, ";", sep = "")
      cmd
    }
  )
)
