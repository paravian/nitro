#' Resampling Support Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures resampling-based support
#' analyses in \pkg{nitro}.
#'
#' `ResamplingCommand` handles bootstrap, jackknife, and symmetric
#' resampling analyses through a single unified interface. The resampling
#' method is selected via the `$method` field. This wraps the TNT
#' `resample` command.
#'
#' The recommended way to create and attach a `ResamplingCommand` to a
#' [TreeAnalysis] is via [set_support()].
#'
#' @details
#' ## Resampling methods
#' | Method | `$method` value | `$probability` |
#' |--------|-----------------|----------------|
#' | Bootstrap | `"bootstrap"` | Not applicable (ignored). |
#' | Jackknife | `"jackknife"` | Deletion probability (default: `36`). |
#' | Symmetric resampling | `"symmetric"` | Change probability (default: `36`). |
#'
#' Setting `$method` to `"bootstrap"` automatically clears `$probability`.
#' Setting it to `"jackknife"` or `"symmetric"` sets `$probability` to
#' `36` if not already specified.
#'
#' ## Default values
#' | Parameter           | Default       |
#' |---------------------|---------------|
#' | `method`            | `"symmetric"` |
#' | `replications`      | `100`         |
#' | `probability`       | `36`          |
#' | `cutoff`            | `0`           |
#' | `frequency_summary` | `"absolute"`  |
#'
#' ## Frequency summary methods
#' One or more summary methods can be specified via `$frequency_summary`:
#'
#' | Value | Description |
#' |-------|-------------|
#' | `"absolute"` | Absolute frequencies (default). |
#' | `"difference"` | Frequency differences (group supported vs. contradicted). |
#' | `"slope"` | Frequency slopes. |
#'
#' ## Dependencies
#' Two dependencies are registered at construction:
#' * **`"tree search"`** *(required)* — a [TreeSearchCommand] defining the
#'   search to repeat during resampling. The search is automatically cloned
#'   and run in non-set-only mode within each replicate.
#' * **`"reference tree"`** *(optional)* — a `ReadTreesCommand` providing a tree
#'   on which to annotate group support frequencies calculated from the
#'   resampling replicates.
#'
#' ## Command output
#' `$render()` produces a string of the form
#' `resample= {method} replications {replications} probability {probability} cut {cutoff} ...;`
#' with optional `from 0` (when starting trees are set) and an inline tree
#' search specification.
#'
#' ## Queue integration
#' Calling `$enqueue()` adds tree tagging commands, a
#' `UniqueTreesCommand`, and this command to a [CommandQueue]. Tree tagging
#' and plotting are inherited from [AbstractGroupSupportCommand].
#'
#' ## Zero-length branch rule
#' Resampling analyses require a compatible zero-length branch rule in the
#' associated [TreeAnalysis]. The `$zlb_rule` must be one of `"maximum"`,
#' `"identical_states"`, or `"minimum"`. [set_support()] validates this
#' automatically.
#'
#' @seealso
#' * [set_support()] — recommended way to create and attach a resampling
#'   analysis to a [TreeAnalysis].
#' * [new_support()] — create a support command without attaching it.
#' * [TreeAnalysisResults] — the results object that carries support values.
#'
#' @references
#' Goloboff, P. A., Farris, J. S., & Nixon, K. C. (2008). TNT, a free
#' program for phylogenetic analysis. *Cladistics*, 24(5), 774--786.
#'
#' @examples
#' \dontrun{
#' # Preferred: use set_support()
#' ta <- set_support(ta, "bootstrap", replications = 1000)
#' ta <- set_support(ta, "jackknife", replications = 500, probability = 25)
#' ta <- set_support(ta, "symmetric", replications = 500)
#'
#' # Modify settings after creation
#' ta$commands$support$replications <- 200
#' ta$commands$support$frequency_summary <- c("absolute", "difference")
#' }
#'
#' @importFrom checkmate asInt assertInt check_character check_flag check_int check_multi_class check_null check_string check_subset test_class test_null test_true
#' @importFrom cli cli_abort cli_alert_warning cli_text col_grey
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom stringr str_replace str_replace_all str_to_lower str_to_upper
#' @importFrom tibble as_tibble
#' @importFrom dplyr across all_of mutate
#' @export
ResamplingCommand <- R6Class(
  "ResamplingCommand",
  inherit = AbstractGroupSupportCommand,
  active = list(
    #' @field cutoff \[`integer(1)`\]\cr
    #'   The minimum frequency threshold below which groups are not
    #'   displayed. Must be an integer between 0 and 99.
    cutoff = function(value) {
      label <- "cutoff"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0, upper = 99)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg cutoff} must be a valid integer.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field frequency_summary \[`character`\]\cr
    #'   The method(s) used to summarise resampling frequencies. One or
    #'   more of `"absolute"`, `"difference"`, or `"slope"`. Partial
    #'   matches are accepted. See **Details**.
    frequency_summary = function(value) {
      label <- "frequency_summary"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_character(value, min.chars = 1, min.len = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg frequency_summary} must be a character vector.",
                      "x" = val_check))
        }

        choices <- c("absolute", "difference", "slope")
        value <- pmatch(value, choices) %>%
          na.omit() %>%
          choices[.]

        val_check <- check_subset(value, choices, empty.ok = FALSE)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg frequency_summary} must be a character vector of valid frequency summary option.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field method \[`character(1)`\]\cr
    #'   The resampling method to use. One of `"bootstrap"`,
    #'   `"jackknife"`, or `"symmetric"`. Partial matches are accepted.
    #'   Changing this field automatically updates `$probability` — see
    #'   **Details**.
    method = function(value) {
      label <- "method"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_string(value, min.chars = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg method} must be a string.",
                      "x" = val_check))
        }

        choices <- c("bootstrap", "jackknife", "symmetric")
        value <- pmatch(value, choices) %>%
          na.omit() %>%
          choices[.]

        val_check <- check_subset(value, choices)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg method} must be valid method.",
                      "x" = val_check))
        }

        if (value == choices[1]) {
          self$set_argument_value("probability", NULL)
        } else {
          probability <- self$get_argument_value("probability")
          if (test_null(probability)) {
            self$set_argument_value("probability", 36)
          }
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field probability \[`integer(1)` or `NULL`\]\cr
    #'   The character change probability used in jackknife and symmetric
    #'   resampling (as a percentage). Must be an integer between 0 and 99.
    #'   Not applicable to bootstrap resampling — a warning is issued and
    #'   the value is ignored if set when `$method` is `"bootstrap"`.
    #'   Returns `NULL` for bootstrap analyses.
    probability = function(value) {
      label <- "probability"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0, upper = 99)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg probability} must be an integer.",
                      "x" = val_check))
        }

        if (self$method == "bootstrap") {
          cli_alert_warning("{.arg probability} is not applicable to bootstrap resampling analyses, ignoring request.")
          value <- NULL
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field replications \[`integer(1)`\]\cr
    #'   The number of resampling replicates to perform. Must be a
    #'   positive integer.
    replications = function(value) {
      label <- "replications"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg replications} must be an integer.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command and supporting commands to a [CommandQueue].
    #'
    #' Calls the parent `$enqueue()` (which adds tree tagging and plotting
    #' commands), then adds a `UniqueTreesCommand` at priority `600` and
    #' this command at priority `510`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(UniqueTreesCommand$new(), 600)
      .queue$add(self, 510)

      .queue
    },
    #' @description
    #' Create a new `ResamplingCommand` object.
    #'
    #' All parameters are optional; when omitted, default values are used.
    #' Parameters can also be modified after construction via the active
    #' bindings. In most cases, [set_support()] is the recommended way to
    #' create and attach this command to a [TreeAnalysis].
    #'
    #' Two dependencies are registered at construction:
    #' * `"tree search"` *(required)*.
    #' * `"reference tree"` *(optional)*.
    #'
    #' @param cutoff \[`integer(1)`\]\cr
    #'   Frequency display cutoff (default: `50`). See the `$cutoff` field.
    #' @param frequency_summary \[`character`\]\cr
    #'   Frequency summary method(s) (default: `"absolute"`). See the
    #'   `$frequency_summary` field.
    #' @param method \[`character(1)`\]\cr
    #'   Resampling method (default: `"symmetric"`). See the `$method`
    #'   field.
    #' @param probability \[`integer(1)`\]\cr
    #'   Change probability for jackknife/symmetric resampling (default:
    #'   `36`). See the `$probability` field.
    #' @param replications \[`integer(1)`\]\cr
    #'   Number of replicates (default: `100`). See the `$replications`
    #'   field.
    #'
    #' @return A new `ResamplingCommand` object.
    initialize = function(cutoff, frequency_summary, method, probability,
                          replications) {
      super$initialize(
        name = "resample",
        description = "Resampling",
        provides = "resampling"
      )

      resampling_cmd_fmt <- function(value) {
        choices <- c(bootstrap = "boot", jackknife = "jak", symmetric = "sym")
        choices[value]
      }
      resampling_pty_fmt <- function(value) {
        str_to_upper(resampling_cmd_fmt(value))
      }
      self$new_argument(
        label = "method",
        description = "Resampling method",
        command_format = resampling_cmd_fmt,
        default_value = "symmetric",
        pretty_format = resampling_pty_fmt
      )

      self$new_argument(
        label = "replications",
        description = "Replications",
        command_format = "replications {value}",
        default_value = 100
      )

      prob_cmd_fmt <- function(value) {
        ifelse(test_null(value), "", glue("probability {value}"))
      }
      prob_pty_fmt <- function(value) {
        ifelse(test_null(value), NA, value)
      }
      self$new_argument(
        "probability",
        "Probability",
        prob_cmd_fmt,
        36,
        prob_pty_fmt
      )

      self$new_argument(
        "cutoff",
        "Cutoff",
        "cut {value}",
        50
      )

      freq_summ_cmd_fmt <- function(value) {
        choices <- c(difference = "gc", absolute = "frequency", slope = "slope")
        value <- choices[value]
        if (!"gc" %in% value) {
          value <- glue("nogc {value}")
        }
        value
      }
      freq_summ_pty_fmt <- function(value) {
        choices <- c(difference = "gc", absolute = "frequency", slope = "slope")
        paste(names(choices[value]), collapse = ", ")
      }
      self$new_argument(
        "frequency_summary",
        "Frequency summary method",
        freq_summ_cmd_fmt,
        "absolute",
        freq_summ_pty_fmt
      )

      for (argument in private$.arguments) {
        self[[argument$label]] <- argument$default_value
      }

      all_labels <- sapply(private$.arguments, getElement, "label")
      self$template <- paste("{", all_labels, "}", sep = "")

      for (argument in private$.arguments) {
        arg_val <- try(get(argument$label), silent = TRUE)
        if (test_class(arg_val, "try-error")) {
          arg_val <- argument$default_value
        }
        self[[argument$label]] <- arg_val
      }

      validate_tree_search <- function(value) {
        val_check <- check_class(value, "TreeSearchCommand")

        if (!test_true(val_check)) {
          cli_abort(c("{.arg value} must be a {.cls TreeSearchCommand} object.",
                      "x" = val_check))
        }

        value$set_only <- TRUE
        ts <- value$clone(deep = TRUE)

        ts
      }

      self$new_dependency("tree search", TRUE, validate_tree_search)

      validate_zlb <- function(value) {
        val_check <- check_class(value, "CollapseRuleCommand")

        if (!test_true(val_check)) {
          cli_abort(c("{.arg value} must be a {.cls CollapseRuleCommand} object.",
                      "x" = val_check))
        }

        valid_zlb_rule <- c("maximum", "identical_states", "minimum")
        val_check <- check_subset(value$rule, valid_zlb_rule)

        if (!test_true(val_check)) {
          val_check <- str_replace_all(val_check, "([{}])", "\\1\\1")
          cli_abort(c("{.arg zlb_rule} must be valid for a resampling {.arg method}.",
                      "x" = val_check,
                      "i" = "Set {.arg zlb_rule} to {valid_zlb_rule}."))
        }

        value
      }

      self$new_dependency("collapse rule", TRUE, validate_zlb)

      validate_topology <- function(value) {
        if (!test_null(value)) {
          val_check <- check_class(value, "ReadTreesCommand")

          if (!test_true(val_check)) {
            cli_abort(c("{.arg value} must be a {.cls ReadTreesCommand} object"))
          }
        }

        value
      }

      self$new_dependency("reference tree", FALSE, validate_topology)
    },
    #' @description
    #' Render the TNT `resample` command string.
    #'
    #' Extends the parent [StandardCommand] `$render()` output by
    #' appending `from 0` when starting trees are set, and an inline tree
    #' search specification when a `"tree search"` dependency is present.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT
    #'   command.
    render = function(...) {
      cmd <- super$render()

      extras <- NULL

      tree <- self$get_dependency("reference tree")
      if (!test_null(tree)) {
        extras <- c(extras, " from 0")
      }

      ts <- self$get_dependency("tree search")
      if (!test_null(ts)) {
        extras <- c(extras, glue(" [ {ts$name}; ]"))
      }

      if (!test_null(extras)) {
        extras <- paste(c(extras, ";"), collapse = "")
        cmd <- str_replace(cmd, ";", extras)
      }

      cmd
    },
    #' @description
    #' Parse raw TNT output into a structured summary table.
    #'
    #' Extracts resampling metadata (frequency type, number of replicates,
    #' cutoff, method, and probability) from the TNT legend output and
    #' returns a tidy tibble.
    #'
    #' @param output \[`character`\]\cr
    #'   A character vector of raw output lines from the TNT executable.
    #'
    #' @return A [tibble][tibble::tibble] with one row per resampling
    #'   result and columns `frequency`, `replicates`, `cutoff`, `method`,
    #'   and `probability`.
    transform = function(output) {
      output <- super$transform(output)

      summ_pattern <- c("Group freqs." = "absolute", "GC values" = "difference")
      type_pattern <- c("Standard B" = "b", "Jacknifing" = "jackknife")

      legend_re <- "Copied legends: \"(?<frequency>[A-Za-z\\. ]+), (?<replicates>[0-9]+) replicates, cut=(?<cutoff>[0-9]+) \\(tree [0-9]\\) - (?<method>[A-Za-z ]+) \\(P=(?<probability>[0-9]+)\\)"

      output <- str_match_all(output, legend_re) %>%
        Reduce(f = rbind) %>%
        extract(, -1)

      if (is.vector(output)) {
        output <- t(output)
      }

      output <- as_tibble(output) %>%
        mutate(
          frequency = str_replace_all(frequency, summ_pattern),
          method = str_replace_all(method, type_pattern) %>%
            str_to_lower(),
          across(all_of(c("replicates", "cutoff", "probability")), as.numeric)
        )

      output
    }
  )
)
