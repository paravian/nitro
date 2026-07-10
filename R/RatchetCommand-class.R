#' Parsimony Ratchet Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures a parsimony ratchet analysis
#' in \pkg{nitro}.
#'
#' The parsimony ratchet (Nixon, 1999) perturbs character weights
#' stochastically — upweighting and downweighting characters with specified
#' probabilities — and then performs branch swapping on the reweighted
#' matrix. This cycle of perturbation and search is repeated for a given
#' number of iterations. This wraps the TNT `ratchet` command.
#'
#' @details
#' ## Default values
#' | Parameter      | Default |
#' |----------------|---------|
#' | `iterations`   | `50`    |
#' | `replacements` | `40`    |
#' | `prob_up`      | `4`     |
#' | `prob_down`    | `4`     |
#' | `set_only`     | `FALSE` |
#'
#' ## Command output
#' `$render()` produces a string of the form
#' `ratchet= iter {iterations} numsubs {replacements} upfactor {prob_up} downfact {prob_down};`
#'
#' ## Disabling within ExtraSearchMethodsCommand
#' When used within [ExtraSearchMethodsCommand], setting `$iterations` to
#' `0` disables the ratchet without removing it from the configuration.
#'
#' ## Dependencies
#' An optional dependency on starting trees (`"starting trees"`) is
#' registered. When set, it must be a `ReadTreesCommand` object.
#'
#' ## Set-only mode and queue integration
#' When `$set_only` is `TRUE`, the command configures parameters without
#' triggering a search. This is set automatically when used within
#' [ExtraSearchMethodsCommand]. Calling `$enqueue()` adds this command to
#' a [CommandQueue] and, when `$set_only` is `FALSE`, also enqueues a
#' [CollapseTreesCommand] and a [TreePlottingCommand].
#'
#' @seealso
#' * [ExtraSearchMethodsCommand] — orchestrates multiple search strategies
#'   including the ratchet.
#' * [set_tree_search()] — recommended way to add this command to a
#'   [TreeAnalysis].
#'
#' @references
#' Goloboff, P. A., Farris, J. S., & Nixon, K. C. (2008). TNT, a free
#' program for phylogenetic analysis. *Cladistics*, 24(5), 774--786.
#'
#' Nixon, K. C. (1999). The parsimony ratchet, a new method for rapid
#' parsimony analysis. *Cladistics*, 15(4), 407--414.
#'
#' @examples
#' # Create with default settings
#' rat <- RatchetCommand$new()
#'
#' # Increase iterations and perturbation strength
#' rat$iterations <- 200
#' rat$replacements <- 80
#' rat$prob_up <- 8
#'
#' # Generate the TNT command
#' rat$render()
#'
#' @importFrom checkmate asInt check_class check_int check_logical check_null test_null test_true
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom glue glue_data
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @export
RatchetCommand <- R6Class(
  "RatchetCommand",
  inherit = TreeSearchCommand,
  active = list(
    #' @field iterations \[`integer(1)`\]\cr
    #'   The number of ratchet iterations (perturbation + search cycles) to
    #'   perform. Must be a non-negative integer. Set to `0` to disable the
    #'   ratchet when used within [ExtraSearchMethodsCommand].
    iterations = function(value) {
      label <- "iterations"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort("{.arg iterations} must be an integer.",
            "x" = val_check
          )
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field replacements \[`integer(1)`\]\cr
    #'   The number of accepted tree rearrangements (replacements) to
    #'   perform in each perturbation phase. Must be a non-negative integer.
    replacements = function(value) {
      label <- "replacements"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort("{.arg replacements} must be an integer.",
            "x" = val_check
          )
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field prob_up \[`integer(1)`\]\cr
    #'   The probability factor for upweighting a character during
    #'   perturbation. Must be a non-negative integer.
    prob_up = function(value) {
      label <- "prob_up"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort("{.arg prob_up} must be an integer.",
            "x" = val_check
          )
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field prob_down \[`integer(1)`\]\cr
    #'   The probability factor for downweighting a character during
    #'   perturbation. Must be a non-negative integer.
    prob_down = function(value) {
      label <- "prob_down"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort("{.arg prob_down} must be an integer.",
            "x" = val_check
          )
        }
        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `RatchetCommand` object.
    #'
    #' @param iterations \[`integer(1)`\]\cr
    #'   Number of ratchet iterations (default: `50`). See the
    #'   `$iterations` field.
    #' @param replacements \[`integer(1)`\]\cr
    #'   Number of accepted rearrangements per perturbation phase (default:
    #'   `40`). See the `$replacements` field.
    #' @param prob_up \[`integer(1)`\]\cr
    #'   Upweighting probability factor (default: `4`). See the `$prob_up`
    #'   field.
    #' @param prob_down \[`integer(1)`\]\cr
    #'   Downweighting probability factor (default: `4`). See the
    #'   `$prob_down` field.
    #' @param set_only \[`logical(1)`\]\cr
    #'   Configure-only mode (default: `FALSE`). See the `$set_only` field.
    #'
    #' @return A new `RatchetCommand` object.
    initialize = function(iterations, replacements, prob_up, prob_down,
                          set_only = FALSE) {
      super$initialize(
        name = "ratchet",
        description = "Ratchet search using existing trees",
        set_only = set_only
      )

      self$new_argument(
        "iterations",
        "Iterations",
        "iter {value}",
        50
      )
      self$new_argument(
        "replacements",
        "Replacements",
        "numsubs {value}",
        40
      )
      self$new_argument(
        "prob_up",
        "Upweighting probability",
        "upfactor {value}",
        4
      )
      self$new_argument(
        "prob_down",
        "Downweighting probability",
        "downfact {value}",
        4
      )

      validate_topology <- function(value) {
        if (!test_null(value)) {
          val_check <- check_class(value, "ReadTreesCommand")

          if (!test_true(val_check)) {
            cli_abort(c("{.arg value} must be a {.cls ReadTreesCommand} object"))
          }
        }

        value
      }

      self$new_dependency("starting trees", FALSE, validate_topology)

      all_labels <- sapply(private$.arguments, getElement, "label")
      self$template <- paste("{", all_labels, "}", sep = "")

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
