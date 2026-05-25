#' Tree Plotting Command
#'
#' @description
#' An [R6][R6::R6Class] class that retrieves trees from TNT in parenthetic
#' (Newick) notation and parses them into `phylo` objects.
#'
#' This command is enqueued automatically by [TreeSearchCommand] and
#' [AbstractGroupSupportCommand] after a search or support analysis completes.
#' Users do not typically need to instantiate it directly.
#'
#' @details
#' ## Default values
#' | Parameter     | Default |
#' |---------------|---------|
#' | `parenthetic` | `FALSE` |
#'
#' ## Command output
#' `$render()` produces `tplot;` (standard) or `tplot *;` (parenthetic
#' notation).
#'
#' ## Output parsing
#' `$transform()` parses the raw TNT tree output into a `multiPhylo`
#' object, remapping taxon indices to names using the matrix data from the
#' required `"matrix"` dependency.
#'
#' ## Dependencies
#' A **required** dependency on `"matrix"` is registered. This must be a
#' [ReadDataCommand] object and is used to resolve taxon indices to names
#' during output parsing.
#'
#' ## Queue integration
#' This command has no `$enqueue()` override; it is added to a
#' [CommandQueue] directly by [TreeSearchCommand] and
#' [AbstractGroupSupportCommand] at priority `601`.
#'
#' @seealso
#' * [TreeSearchCommand] — enqueues this command after a search.
#' * [AbstractGroupSupportCommand] — enqueues this command after a support
#'   analysis.
#' * [ReadDataCommand] — satisfies the required `"matrix"` dependency.
#'
#' @keywords internal
#' @importFrom ape .compressTipLabel .uncompressTipLabel read.tree
#' @importFrom checkmate check_class check_flag test_class test_true
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom stringr str_match str_replace_all str_trim
#' @importFrom utils tail
TreePlottingCommand <- R6Class(
  "TreePlottingCommand",
  inherit = StandardCommand,
  active = list(
    #' @field parenthetic \[`logical(1)`\]\cr
    #'   Whether to retrieve trees in parenthetic (Newick) notation.
    parenthetic = function(value) {
      label <- "parenthetic"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be a logical value.",
                      "x" = val_check))
        }

        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `TreePlottingCommand` object.
    #'
    #' This command is created automatically by [TreeSearchCommand] and
    #' [AbstractGroupSupportCommand]. Direct instantiation is rarely necessary.
    #'
    #' @param parenthetic \[`logical(1)`\]\cr
    #'   Retrieve trees in parenthetic notation? (default: `FALSE`). See
    #'   the `$parenthetic` field.
    #' @param ... Optional named arguments passed to the constructor of the
    #'   command class.
    #'
    #' @return A new `TreePlottingCommand` object.
    initialize = function(parenthetic, ...) {
      super$initialize(
        name = "tplot",
        description = "Tree plotting",
        outputs = "trees",
        ...
      )

      private$.outputs <- "trees"

      paren_cmd_fmt <- function(value) {
        ifelse(value, "*", NA)
      }
      paren_pty_fmt <- function(value) {
        ifelse(value, "yes", "no")
      }
      self$new_argument(
        label = "parenthetic",
        description = "Use parenthetic notation",
        command_format = paren_cmd_fmt,
        default_value = FALSE,
        pretty_format = paren_pty_fmt
      )

      validate_matrix <- function(value) {
        val_check <- check_class(value, c("ReadDataCommand"))

        if (!test_true(val_check)) {
          cli_abort(c("{.arg value} must be a {.cls ReadDataCommand} object.",
                      "x" = val_check))
        }

        value
      }

      self$new_dependency("matrix", TRUE, validate_matrix)

      all_labels <- sapply(private$.arguments, `[[`, "label")
      self$template <- paste("{", all_labels, "}", sep = "")

      for (argument in private$.arguments) {
        arg_val <- try(get(argument$label), silent = TRUE)
        if (test_class(arg_val, "try-error")) {
          arg_val <- argument$default_value
        }
        self[[argument$label]] <- arg_val
      }
    },
    #' @description
    #' Parse raw TNT tree output into a `multiPhylo` object.
    #'
    #' Extracts parenthetic tree strings from the TNT output, converts
    #' them to `phylo` objects, and remaps taxon indices to names using
    #' the `"matrix"` dependency.
    #'
    #' @param output \[`character`\]\cr
    #'   A character vector of raw output lines from the TNT executable.
    #'
    #' @return A `multiPhylo` object.
    transform = function(output) {
      output <- super$transform(output)

      phy <- str_match(output, "[\\(\\)0-9 ]+") %>%
        tail(-1) %>%
        str_trim() %>%
        str_replace_all(
          c(
            " " = ",",
            ",\\)" = "\\)",
            "\\)\\(" = "\\),\\(",
            "$" = ";"
          )
        ) %>%
        lapply(read.tree, file = NULL) %>%
        .compressTipLabel()

      matrix <- self$get_dependency("matrix")

      all_taxa <- sapply(matrix$data, `[[`, "taxa") %>%
        as.vector() %>%
        unique()

      tax_idx <- attr(phy, "TipLabel") %>%
        as.numeric() %>%
        add(1)

      attr(phy, "TipLabel") <- all_taxa[tax_idx]

      output <- .uncompressTipLabel(phy)

      output
    }
  )
)
