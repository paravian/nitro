#' Abstract Implied Weighting Command Base Class
#'
#' @description
#' An [R6][R6::R6Class] class that serves as the common base for
#' [ImpliedWeightingCommand] and [ExtendedImpliedWeightingCommand] in
#' \pkg{nitro}.
#'
#' `AbstractImpliedWeightingCommand` defines the `$concavity_constant`
#' active binding shared by both subclasses. It is not intended to be
#' instantiated directly.
#'
#' @details
#' ## Subclassing
#' Subclasses should call `super$initialize(name, description)` and then
#' register their own arguments with `$new_argument()`. The
#' `$concavity_constant` active binding is defined here but the
#' corresponding argument must be registered by each subclass, since the
#' `command_format` differs between `piwe` and `xpiwe`.
#'
#' ## Concavity constant
#' The concavity constant *k* controls the strength of down-weighting
#' applied to homoplastic characters. Lower values apply stronger
#' down-weighting. The valid range is 0 to 1000.
#'
#' @seealso
#' * [ImpliedWeightingCommand] — standard implied weighting; registers
#'   `concavity_constant` with the `piwe` command format.
#' * [ExtendedImpliedWeightingCommand] — extended implied weighting;
#'   registers `concavity_constant` with the `xpiwe` command format and
#'   adds per-character constant fields.
#' * [set_weighting()] — recommended way to configure weighting in a
#'   [TreeAnalysis].
#' * [TreeAnalysisResults] — the results object that carries homoplasy
#'   scores.
#'
#' @keywords internal
#' @importFrom checkmate check_number test_true
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
AbstractImpliedWeightingCommand <- R6Class(
  "AbstractImpliedWeightingCommand",
  inherit = StandardCommand,
  active = list(
    #' @field concavity_constant \[`numeric(1)`\]\cr
    #'   The concavity constant *k* applied during implied weighting. Lower
    #'   values apply stronger down-weighting to homoplastic characters.
    #'   Must be a number between 0 and 1000.
    concavity_constant = function(value) {
      label <- "concavity_constant"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_number(value, lower = 0, upper = 1000)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg concavity_constant} must be a valid number.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @param name \[`character(1)`\]\cr
    #'   The TNT command name. See the `$name` field.
    #' @param description \[`character(1)`\]\cr
    #'   A human-readable description. See the `$description` field.
    initialize = function(name, description) {
      super$initialize(
        name = name,
        description = description
      )

      self$new_argument(
        label          = "concavity_constant",
        description    = "Concavity constant",
        command_format = "={value}",
        default_value  = 3
      )
    }
  )
)
