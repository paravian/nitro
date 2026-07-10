#' Read Trees Command
#'
#' @description
#' An [R6][R6::R6Class] class that reads one or more phylogenetic trees into
#' TNT for use as starting trees in a subsequent analysis.
#'
#' This command is used when starting trees are required by a search or
#' resampling analysis. It satisfies the `"starting trees"` dependency
#' declared by [ResamplingCommand] and [RatchetCommand].
#'
#' @details
#' ## Input format
#' The `$trees` field accepts either a single `phylo` object or a
#' `multiPhylo` object. Node labels and branch lengths are stripped
#' automatically, as TNT does not use them.
#'
#' ## Command output
#' `$render()` produces a multi-line `tread` block containing the trees
#' in TNT format.
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `230`.
#'
#' ## Inline rendering
#' `ReadTreesCommand` sets `$inline` to `TRUE` by default. When used
#' within [execute_analysis()], the tree data is written to a separate
#' temporary file and a `proc <filename>;` call is substituted in the TNT
#' script, preventing very large tree blocks from making the script
#' unreadable.
#'
#' @seealso
#' * [ResamplingCommand] — declares an optional `"starting trees"`
#'   dependency satisfied by this command.
#' * [RatchetCommand] — declares an optional `"starting trees"` dependency
#'   satisfied by this command.
#'
#' @keywords internal
#' @importFrom ape write.tree .compressTipLabel
#' @importFrom checkmate check_multi_class test_class test_true
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
#' @importFrom stringr str_replace_all str_split_1
ReadTreesCommand <- R6Class(
  "ReadTreesCommand",
  inherit = BasicCommand,
  private = list(
    .trees = NULL
  ),
  active = list(
    #' @field trees \[`phylo` or `multiPhylo`\]\cr
    #'   The tree or trees to read into TNT. Node labels and branch
    #'   lengths are stripped automatically on assignment.
    trees = function(value) {
      if (missing(value)) {
        return(private$.trees)
      } else {
        val_check <- check_multi_class(
          value,
          c("phylo", "multiPhylo")
        )

        if (!test_true(val_check)) {
          cli_abort(c("{.arg data} must be either a {.cls phylo} or {.cls multiPhylo} object.",
            "x" = val_check
          ))
        }

        if (test_class(value, "phylo")) {
          value$node.label <- NULL
          value$edge.length <- NULL
        } else if (test_class(value, "multiPhylo")) {
          value <- lapply(value, function(x) {
            x$node.label <- NULL
            x$edge.length <- NULL
            x
          }) %>%
            .compressTipLabel()
        }

        private$.trees <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Adds this command at priority `230`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(self, 230)
      .queue
    },
    #' @description
    #' Create a new `ReadTreesCommand` object.
    #'
    #' @param trees \[`phylo` or `multiPhylo`\]\cr
    #'   The tree or trees to read into TNT. See the `$trees` field.
    #' @param ... Optional named arguments passed to the constructor of the
    #'   command class.
    #'
    #' @return A new `ReadTreesCommand` object.
    initialize = function(trees, ...) {
      a <- as.list(environment(), all = TRUE) %>%
        head(-1)

      super$initialize(
        name = "tread",
        description = "Read trees",
        ...
      )

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Render the TNT `tread` command block.
    #'
    #' Produces a multi-line character vector containing the trees in TNT
    #' Newick format if `$inline` is `FALSE` or a `proc`
    #'
    #' @param ... Not used.
    #'
    #' @return A character vector of TNT command lines.
    render = function(...) {
      tnt_fmt <- c(
        "; " = "*;",
        ";$" = "",
        "\\),\\(" = "\\)\\(",
        "(,[^\\)]+)" = "\\1,",
        "\\)," = "\\)",
        "," = " "
      )

      tnt_trees <- write.tree(self$trees) %>%
        paste(collapse = " ") %>%
        str_replace_all(tnt_fmt) %>%
        str_split_1(";")

      cmd <- c(self$name, tnt_trees, ";")
      cmd
    }
  )
)
