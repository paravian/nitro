#' Abstract Group Support Command Base Class
#'
#' @description
#' `AbstractGroupSupportCommand` is the base [R6][R6::R6Class] class for all
#' phylogenetic group support analysis commands in \pkg{nitro}. It defines the
#' queue integration behaviour common to all support methods, including
#' tree tagging and plotting steps.
#'
#' This class is not exported. It is intended to be subclassed by concrete
#' support command classes such as [ResamplingCommand].
#'
#' @details
#' ## Subclassing
#' Subclasses should call `super$initialize(name, description)` in their
#' own `initialize()` method. The parent constructor sets `$provides` to
#' `"support"` and `$outputs` to `"label legend"`.
#'
#' ## Queue integration
#' `$enqueue()` adds the following commands to the [CommandQueue]
#' automatically:
#'
#' | Command | Priority | Purpose |
#' |---------|----------|---------|
#' | `TreeTaggingCommand("start")` | `500` | Mark the start of the support analysis. |
#' | `TreeTaggingCommand("stop")` | `520` | Mark the end of the support analysis. |
#' | `TreeTaggingCommand("display")` | `601` | Display tagged trees. |
#' | `TreePlottingCommand(TRUE)` | `601` | Plot trees with support labels. |
#'
#' Subclasses should call `super$enqueue(.queue)` and then add their own
#' command at the appropriate priority.
#'
#' ## Active bindings
#' All active bindings from [StandardCommand] and [BasicCommand] are
#' available, including `$name`, `$description`, `$template`,
#' `$dependencies`, `$requires`, `$optional`, `$outputs` (set to
#' `"label legend"`), and `$provides` (set to `"support"`).
#'
#' @seealso
#' * [ResamplingCommand] — concrete subclass for bootstrap, jackknife, and
#'   symmetric resampling analyses.
#' * [StandardCommand] — parent class providing the argument system.
#'
#' @keywords internal
#' @importFrom checkmate asInt check_character check_int check_multi_class check_subset test_true
#' @importFrom cli cli_abort cli_text
#' @importFrom R6 R6Class
#' @importFrom stringr str_to_sentence
AbstractGroupSupportCommand <- R6Class(
  "AbstractGroupSupportCommand",
  inherit = StandardCommand,
  public = list(
    #' @description
    #' Add tree tagging and plotting commands to a [CommandQueue].
    #'
    #' Adds `TreeTaggingCommand` objects at priorities `500`, `520`, and
    #' `601`. Subclasses should call `super$enqueue(.queue)` before adding their
    #' own command.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(TreeTaggingCommand$new("start"), 500)
      .queue$add(TreeTaggingCommand$new("stop"), 520)
      .queue$add(TreeTaggingCommand$new("display"), 601)
      .queue$add(TreePlottingCommand$new(TRUE), 601)

      .queue
    },
    #' @description
    #' Create a new `AbstractGroupSupportCommand` object.
    #'
    #' Sets `$provides` to `"support"` and `$outputs` to `"label legend"`.
    #' Subclasses should call this via `super$initialize()`.
    #'
    #' @param name \[`character(1)`\]\cr
    #'   The TNT command name. See the `$name` field.
    #' @param description \[`character(1)`\]\cr
    #'   A human-readable description. See the `$description` field.
    #' @param ... Optional named arguments passed to the constructor of the
    #'   command class.
    #'
    #' @return A new `AbstractGroupSupportCommand` object.
    initialize = function(name, description, ...) {
      super$initialize(
        name = name,
        description = description,
        ...
      )

      private$.provides <- "support"
      private$.outputs <- "label legend"
    }
  )
)
