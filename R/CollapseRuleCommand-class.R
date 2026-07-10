#' Zero-Length Branch Collapsing Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures the rule used to collapse
#' zero-length branches in \pkg{nitro}.
#'
#' This command is created automatically by [TreeAnalysis] from the
#' `$zlb_rule` field. Users do not typically need to instantiate it
#' directly.
#'
#' @details
#' ## Collapsing rules
#' | Rule | Description |
#' |------|-------------|
#' | `"none"` | Do not collapse zero-length branches. |
#' | `"minimum"` | Collapse if the minimum possible branch length is zero (default). |
#' | `"maximum"` | Collapse if the maximum possible branch length is zero. |
#' | `"identical_states"` | Collapse only if ancestor and descendant states are identical. |
#' | `"discard_tree"` | Discard trees that must contain a zero-length branch. |
#' | `"spr"` | Collapse using SPR operations. |
#' | `"tbr"` | Collapse using TBR operations. |
#'
#' Partial matches are accepted (e.g., `"min"` resolves to `"minimum"`).
#'
#' ## Command output
#' `$render()` produces a string of the form `collapse {n};` where `{n}`
#' is the zero-based index of the selected rule in the order listed above.
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `420`.
#'
#' ## Resampling compatibility
#' Resampling analyses (bootstrap, jackknife, symmetric) require `$rule`
#' to be one of `"minimum"`, `"maximum"`, or `"identical_states"`.
#' [set_support()] validates this automatically via the `$zlb_rule` field
#' of [TreeAnalysis].
#'
#' @seealso
#' * [TreeAnalysis] — sets the collapsing rule via its `$zlb_rule` field.
#' * [set_support()] — validates the collapsing rule for resampling
#'   analyses.
#'
#' @keywords internal
#' @importFrom checkmate check_choice check_flag test_true test_class
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom stringr str_replace_all
CollapseRuleCommand <- R6Class(
  "CollapseRuleCommand",
  inherit = StandardCommand,
  private = list(
    rules = c(
      "none",
      "maximum",
      "identical_states",
      "minimum",
      "discard_tree",
      "spr",
      "tbr"
    )
  ),
  active = list(
    #' @field rule \[`character(1)`\]\cr
    #'   The rule for handling zero-length branches. Partial matches are
    #'   accepted. See **Details** for the available options. Defaults to
    #'   `"minimum"`.
    rule = function(value) {
      label <- "rule"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        value <- pmatch(value, private$rules) %>%
          na.omit() %>%
          private$rules[.]

        val_check <- check_choice(value, private$rules)

        if (!test_true(val_check)) {
          val_check <- str_replace_all(val_check, "([{}])", "\\1\\1")
          cli_abort(c("{.arg {label}} must be a valid choice.",
            "x" = val_check
          ))
        }

        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Adds this command at priority `420`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(self, 420)
      .queue
    },
    #' @description
    #' Create a new `CollapseRuleCommand` object.
    #'
    #' This command is created automatically by [TreeAnalysis]. Direct
    #' instantiation is rarely necessary.
    #'
    #' @param rule \[`character(1)`\]\cr
    #'   The collapsing rule (default: `"minimum"`). See the `$rule`
    #'   field.
    #' @param ... Optional named arguments passed to the constructor of the
    #'   command class.
    #'
    #' @return A new `CollapseRuleCommand` object.
    initialize = function(rule, ...) {
      a <- as.list(environment(), all = TRUE) %>%
        tail(-1)

      super$initialize(
        name = "collapse",
        description = "Zero-length branch collapsing",
        provides = "collapse rule",
        ...
      )

      private$.provides <- "collapse rule"

      rule_cmd_fmt <- function(value) {
        which(value == private$rules) - 1
      }
      self$new_argument(
        label = "rule",
        description = "Collapsing rule",
        command_format = rule_cmd_fmt,
        default_value = "minimum",
        pretty_format = "{value}"
      )

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
