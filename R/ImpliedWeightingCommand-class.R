#' Implied Weighting Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures standard implied weighting in
#' \pkg{nitro}.
#'
#' Implied weighting (Goloboff, 1993) down-weights homoplastic characters
#' using a concavity function controlled by a constant *k*. Lower values of
#' *k* apply stronger down-weighting. This wraps the TNT `piwe` command.
#'
#' For extended implied weighting (where each character receives an
#' independent concavity constant), see [ExtendedImpliedWeightingCommand].
#' In practice, both are most conveniently configured together via
#' [set_weighting()].
#'
#' @details
#' ## Default values
#' | Parameter            | Default |
#' |----------------------|---------|
#' | `concavity_constant` | `3`     |
#'
#' ## Command output
#' `$render()` produces a string of the form `piwe ={concavity_constant};`
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command at priority `160` and also
#' enqueues a `HomoplasyTreeScoresCommand` at priority `710` to compute
#' homoplasy scores after the search. This behaviour is inherited by
#' [ExtendedImpliedWeightingCommand], which calls `$enqueue()` on a
#' correctly configured `ImpliedWeightingCommand` before adding itself at
#' priority `230`.
#'
#' ## Role in the class hierarchy
#' `ImpliedWeightingCommand` and [ExtendedImpliedWeightingCommand] both inherit
#' from [AbstractImpliedWeightingCommand], which defines the shared
#' `$concavity_constant` active binding. [set_weighting()] handles the creation
#' and attachment of both commands transparently.
#'
#' @seealso
#' * [AbstractImpliedWeightingCommand] — parent class defining the shared
#'   `$concavity_constant` field.
#' * [ExtendedImpliedWeightingCommand] — extends implied weighting with
#'   per-character concavity constants; depends on this command.
#' * [set_weighting()] — recommended way to configure weighting in a
#'   [TreeAnalysis].
#' * [TreeAnalysisResults] — the results object that carries homoplasy
#'   scores.
#'
#' @references
#' Goloboff, P. A. (1993). Estimating character weights during tree
#' search. *Cladistics*, 9(1), 83--91.
#'
#' @examples
#' # Create with default concavity constant
#' iw <- ImpliedWeightingCommand$new()
#'
#' # Specify concavity constant at construction
#' iw <- ImpliedWeightingCommand$new(concavity_constant = 3)
#'
#' # Modify after construction
#' iw$concavity_constant <- 6
#'
#' # Generate the TNT command
#' iw$render()
#'
#' @importFrom checkmate check_number test_class test_true
#' @importFrom cli cli_abort cli_text col_grey col_red style_italic
#' @importFrom R6 R6Class
#' @export
ImpliedWeightingCommand <- R6Class(
  "ImpliedWeightingCommand",
  inherit = AbstractImpliedWeightingCommand,
  public = list(
    #' @description
    #' Add this command and a `HomoplasyTreeScoresCommand` to a
    #' [CommandQueue].
    #'
    #' Adds a `HomoplasyTreeScoresCommand` at priority `710` and this
    #' command at priority `160`. This method is also called by
    #' [ExtendedImpliedWeightingCommand]`$enqueue()`, which passes a
    #' correctly configured `ImpliedWeightingCommand` through this method
    #' before adding itself at priority `230`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      scores <- HomoplasyTreeScoresCommand$new()
      .queue$add(scores, 710)

      .queue$add(self, 160)
      .queue
    },
    #' @description
    #' Create a new `ImpliedWeightingCommand` object.
    #'
    #' @param concavity_constant \[`numeric(1)`\]\cr
    #'   The concavity constant *k* (default: `3`). See the
    #'   `$concavity_constant` field.
    #'
    #' @return A new `ImpliedWeightingCommand` object.
    initialize = function(concavity_constant) {
      a <- as.list(environment(), all = TRUE)

      super$initialize(
        name        = "piwe",
        description = "Implied weighting"
      )

      all_labels <- sapply(private$.arguments, `[[`, "label")
      self$template <- paste("{", all_labels, "}", sep = "")

      for (argument in private$.arguments) {
        arg_val <- try(get(argument$label, envir = list2env(a)), silent = TRUE)
        if (test_class(arg_val, "try-error")) {
          arg_val <- argument$default_value
        }
        self[[argument$label]] <- arg_val
      }
    }
  )
)
