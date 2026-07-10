#' Extended Implied Weighting Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures extended implied weighting in
#' \pkg{nitro}.
#'
#' Extended implied weighting assigns each character an independent
#' concavity constant based on its observed homoplasy, rather than using a
#' single global constant *k*. This wraps the TNT `xpiwe` command.
#'
#' @details
#' ## Default values
#' | Parameter            | Default |
#' |----------------------|---------|
#' | `concavity_constant` | `3`     |
#' | `multi_constant`     | `FALSE` |
#' | `proportion`         | `0.5`   |
#' | `max_ratio`          | `5`     |
#'
#' ## Command output
#' `$render()` produces:
#' * In single-constant mode (`$multi_constant = FALSE`): `xpiwe =;`
#' * In multi-constant mode (`$multi_constant = TRUE`):
#'   `xpiwe ( *{proportion} <{max_ratio};`
#'
#' ## Multi-constant mode
#' When `$multi_constant` is `TRUE`, each character receives its own
#' concavity constant. In this mode:
#' * `$proportion` controls the assumed proportion of homoplasy for
#'   missing values.
#' * `$max_ratio` limits how much constants can diverge between
#'   characters.
#'
#' Setting `$multi_constant` to `FALSE` is equivalent to standard implied
#' weighting.
#'
#' ## Dependency and queue integration
#' `ExtendedImpliedWeightingCommand` handles the creation of the required
#' [ImpliedWeightingCommand] command. When calling `$enqueue()`, it
#' automatically creates an [ImpliedWeightingCommand] configured with the same
#' `$concavity_constant` (which adds it at priority `160` along with a
#' `HomoplasyTreeScoresCommand` at priority `710`), and then adds this command
#' at priority `230`. When using [set_weighting()], both commands are created
#' and attached to the [TreeAnalysis] transparently.
#'
#' @seealso
#' * [AbstractImpliedWeightingCommand] — parent class defining the shared
#'   `$concavity_constant` field.
#' * [ImpliedWeightingCommand] — the companion command; created and
#'   enqueued automatically by `$enqueue()`.
#' * [set_weighting()] — recommended way to configure weighting in a
#'   [TreeAnalysis].
#'
#' @references
#' Goloboff, P. A. (2014). Extended implied weighting. *Cladistics*, 30(3),
#' 260--272.
#'
#' @examples
#' # Preferred: use set_weighting() which handles both commands
#' \dontrun{
#' ta <- set_weighting(ta, "extended",
#'   concavity_constant = 6,
#'   multi_constant     = TRUE,
#'   proportion         = 0.5,
#'   max_ratio          = 5
#' )
#' }
#'
#' # Direct construction
#' eiw <- ExtendedImpliedWeightingCommand$new(
#'   concavity_constant = 6,
#'   multi_constant     = TRUE,
#'   proportion         = 0.3,
#'   max_ratio          = 3
#' )
#' eiw$render()
#'
#' @importFrom checkmate check_class check_flag check_number test_class test_true
#' @importFrom cli cli_abort cli_text col_grey col_red style_italic
#' @importFrom glue glue_data
#' @importFrom R6 R6Class
#' @export
ExtendedImpliedWeightingCommand <- R6Class(
  "ExtendedImpliedWeightingCommand",
  inherit = AbstractImpliedWeightingCommand,
  active = list(
    #' @field max_ratio \[`numeric(1)`\]\cr
    #'   The maximum acceptable ratio between any two concavity constants
    #'   when `$multi_constant` is `TRUE`. Smaller values constrain
    #'   constants to be more similar to one another. A value of `1` is
    #'   equivalent to standard implied weighting. Must be a non-negative
    #'   number. Only meaningful when `$multi_constant` is `TRUE`.
    max_ratio = function(value) {
      label <- "max_ratio"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_number(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg max_ratio} must be a number.",
            "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field multi_constant \[`logical(1)`\]\cr
    #'   Whether to assign each character an independent concavity constant
    #'   based on its observed homoplasy. When `FALSE` (default), a single
    #'   global constant is used (equivalent to standard implied
    #'   weighting). When `TRUE`, `$proportion` and `$max_ratio` become
    #'   active.
    multi_constant = function(value) {
      label <- "multi_constant"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg multi_constant} must be a logical.",
            "x" = val_check
          ))
        }

        self$set_argument_value(label, value)
      }
    },
    #' @field proportion \[`numeric(1)`\]\cr
    #'   The assumed proportion of homoplasy for missing values under
    #'   extended implied weighting. A value of `0` is equivalent to
    #'   standard implied weighting. Must be a non-negative number. Only
    #'   meaningful when `$multi_constant` is `TRUE`.
    proportion = function(value) {
      label <- "proportion"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_number(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg proportion} must be a number.",
            "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command and the required [ImpliedWeightingCommand] to a
    #' [CommandQueue].
    #'
    #' Creates an [ImpliedWeightingCommand] configured with the current
    #' `$concavity_constant` and calls its `$enqueue()` method, which adds
    #' it at priority `160` along with a `HomoplasyTreeScoresCommand` at
    #' priority `710`. Then adds this command at priority `230`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      iw <- ImpliedWeightingCommand$new(
        concavity_constant = self$concavity_constant
      )
      .queue <- iw$enqueue(.queue)

      .queue$add(self, 230)
      .queue
    },
    #' @description
    #' Create a new `ExtendedImpliedWeightingCommand` object.
    #'
    #' All parameters are optional; when omitted, default values are used.
    #' In most cases, [set_weighting()] is the recommended way to create
    #' and attach this command to a [TreeAnalysis].
    #'
    #' @param concavity_constant \[`numeric(1)`\]\cr
    #'   The concavity constant *k* (default: `3`). See the
    #'   `$concavity_constant` field.
    #' @param max_ratio \[`numeric(1)`\]\cr
    #'   Maximum ratio between concavity constants (default: `5`). See the
    #'   `$max_ratio` field.
    #' @param multi_constant \[`logical(1)`\]\cr
    #'   Use per-character concavity constants? (default: `FALSE`). See the
    #'   `$multi_constant` field.
    #' @param proportion \[`numeric(1)`\]\cr
    #'   Assumed proportion of homoplasy for missing values (default:
    #'   `0.5`). See the `$proportion` field.
    #'
    #' @return A new `ExtendedImpliedWeightingCommand` object.
    initialize = function(concavity_constant, max_ratio, multi_constant,
                          proportion) {
      super$initialize(
        name = "xpiwe",
        description = "Extended implied weighting"
      )

      multi_constant_cmd_fmt <- function(value) {
        ifelse(test_true(value), "(", "")
      }
      multi_constant_pty_fmt <- function(value) {
        ifelse(test_true(value), "yes", "no")
      }
      self$new_argument(
        label = "multi_constant",
        description = "Multiple constants",
        command_format = multi_constant_cmd_fmt,
        default_value = FALSE,
        pretty_format = multi_constant_pty_fmt
      )

      self$new_argument(
        label = "proportion",
        description = "Proportion",
        command_format = "*{value}",
        default_value = 0.5
      )

      self$new_argument(
        label = "max_ratio",
        description = "Maximum ratio",
        command_format = "<{value}",
        default_value = 5
      )

      self$template <- function(...) {
        obj <- list(...)
        obj$concavity_constant <- NULL
        arguments <- c()
        if (test_true(obj$multi_constant)) {
          arguments <- c(
            arguments,
            glue_data(obj, "(", "*{proportion}", "<{max_ratio}", .sep = " ")
          )
        } else {
          arguments <- c(
            arguments,
            "="
          )
        }
        arguments
      }

      for (argument in private$.arguments) {
        arg_val <- try(get(argument$label), silent = TRUE)
        if (test_class(arg_val, "try-error")) {
          arg_val <- argument$default_value
        }
        self[[argument$label]] <- arg_val
      }
    }
  )
)
