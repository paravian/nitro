#' Tree Analysis Results
#'
#' @description
#' An [R6][R6::R6Class] class that stores the results of a \pkg{nitro} tree
#' analysis, including the trees, per-tree statistics, node support labels,
#' and the command queue that produced them.
#'
#' `TreeAnalysisResults` objects are created automatically by
#' [TntInterface]`$execute()` and returned by [execute_analysis()]. Users
#' do not typically need to instantiate this class directly.
#'
#' @details
#' ## Tree storage
#' Trees are stored internally as a list of [treedata][tidytree::treedata]
#' objects, one per tree. Each `treedata` object carries:
#' * The tree topology (as a `phylo` object).
#' * Per-tree statistics in its `@info` slot (steps, homoplasy scores, CI,
#'   RI, RC where available).
#' * Node-level support annotations joined from `node_labels` (where
#'   available).
#'
#' ## Statistics
#' The `$statistics` field computes a summary tibble on demand by
#' extracting the `@info` slot from each stored tree. The following
#' statistics are included when available:
#'
#' | Column | Description |
#' |--------|-------------|
#' | `Tree steps` | Raw parsimony step count. |
#' | `Adjusted homoplasy scores` | Implied weighting fit scores. |
#' | `CI` | Consistency index. |
#' | `RI` | Retention index. |
#' | `RC` | Rescaled consistency index. |
#'
#' CI, RI, and RC are computed only when both `steps` and `possible_steps`
#' are available.
#'
#' ## Conversion
#' Use [as.phylo.TreeAnalysisResults()] to extract trees as a `phylo` or
#' `multiPhylo` object for use with other packages.
#'
#' @seealso
#' * [execute_analysis()] — runs an analysis and returns a
#'   `TreeAnalysisResults` object.
#' * [as.phylo.TreeAnalysisResults()] — converts results to `phylo` or
#'   `multiPhylo`.
#' * [TreeAnalysis] — the analysis configuration object.
#'
#' @examples
#' \dontrun{
#' interface <- create_interface("/usr/local/bin/tnt")
#'
#' nex_path <- system.file("extdata", "canale_2022.nex", package = "nitro")
#' dm <- ReadAsPhyDat(nex_path) |> create_matrix()
#' ta <- make_tree_analysis(dm, outgroup = "Abelisauridae")
#' ta <- set_tree_search(ta, "branch_swapping", replications = 100)
#'
#' results <- execute_analysis(interface, ta)
#'
#' # Single tree returned as phylo
#' phy <- as.phylo(results)
#'
#' # Multiple trees returned as multiPhylo
#' plot(phy)
#' }
#'
#' @keywords internal
#' @importFrom ape .compressTipLabel as.phylo
#' @importFrom checkmate assert check_character check_class check_null check_numeric check_tibble makeAssertCollection test_null test_true
#' @importFrom cli cli_abort cli_text col_grey col_red style_italic
#' @importFrom dplyr across bind_cols everything full_join mutate reframe rename
#' @importFrom glue glue
#' @importFrom magrittr %>% extract2 use_series
#' @importFrom R6 R6Class
#' @importFrom stringr str_to_sentence
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr unnest
#' @importFrom tidytree as.treedata
TreeAnalysisResults <- R6Class(
  "TreeAnalysisResults",
  private = list(
    .queue = NULL,
    .trees = NULL
  ),
  active = list(
    #' @field trees \[`list`\]\cr
    #'   *(Read-only.)* A list of [treedata][tidytree::treedata] objects,
    #'   one per tree, each carrying topology, statistics, and node
    #'   annotations.
    trees = function(...) {
      if (length(list(...)) == 0) {
        return(private$.trees)
      }
      cli_abort(c("{.arg trees} is a read-only attribute."))
    },
    #' @field queue \[`CommandQueue`\]\cr
    #'   *(Read-only.)* A clone of the [CommandQueue] that produced these
    #'   results.
    queue = function(...) {
      if (length(list(...)) == 0) {
        return(private$.queue$clone(deep = TRUE))
      }
      cli_abort(c("{.arg queue} is a read-only attribute."))
    },
    #' @field statistics \[`tibble`\]\cr
    #'   *(Read-only.)* A [tibble][tibble::tibble] of per-tree statistics
    #'   extracted from the `@info` slot of each stored tree. See
    #'   **Details**.
    statistics = function(...) {
      if (length(list(...)) == 0) {
        stat_df <- lapply(self$trees, function(x) {
          slot(x, "info") %>% use_series("statistics")}
          ) %>%
          Reduce(f = rbind) %>%
          as_tibble() %>%
          unnest(cols = everything())

        return(stat_df)
      }
      cli_abort(c("{.arg statistics} is a read-only attribute."))
    }
  ),
  public = list(
    #' @description
    #' Create a new `TreeAnalysisResults` object.
    #'
    #' This constructor is called automatically by [TntInterface]`$execute()`
    #' after a completed analysis. Direct instantiation is rarely necessary.
    #'
    #' @param text \[`character`\]\cr
    #'   Raw text output from the TNT executable.
    #' @param trees \[`multiPhylo`\]\cr
    #'   The trees produced by the analysis.
    #' @param homoplasy_scores \[`numeric` or `NULL`\]\cr
    #'   Implied weighting fit scores, one per tree (default: `NULL`).
    #' @param node_labels \[`tibble` or `NULL`\]\cr
    #'   Node-level support annotations (default: `NULL`).
    #' @param label_legend \[`tibble` or `NULL`\]\cr
    #'   Metadata describing the columns of `node_labels` (default:
    #'   `NULL`).
    #' @param possible_steps \[`numeric` or `NULL`\]\cr
    #'   A named numeric vector with elements `"minimum"` and `"maximum"`
    #'   giving the step range across all characters (default: `NULL`).
    #' @param steps \[`numeric` or `NULL`\]\cr
    #'   Raw step counts, one per tree (default: `NULL`).
    #'
    #' @return A new `TreeAnalysisResults` object.
    initialize = function(text, trees, homoplasy_scores = NULL,
                          node_labels = NULL, label_legend = NULL,
                          possible_steps = NULL, steps = NULL) {
      val_check <- check_character(text, min.chars = 1, min.len = 1, any.missing = FALSE)
      if (!test_true(val_check)) {
        cli_abort(c("{.arg text} must be a character vector.",
                    "x" = val_check))
      }

      val_check <- check_class(trees, "multiPhylo")
      if (!test_true(val_check)) {
        cli_abort(c("{.arg trees} must be a {.cls multiPhylo} object.",
                    "x" = val_check))
      }

      coll <- makeAssertCollection()

      assert(
        check_null(steps),
        check_numeric(steps, lower = 0, len = length(trees), any.missing = FALSE),
        add = coll
      )

      if (!coll$isEmpty()) {
        val_check <- coll$getMessages()
        cli_abort(c("{.arg steps} must be either {.val NULL} or a valid numeric vector.",
                    "x" = val_check))
      }

      assert(
        check_null(homoplasy_scores),
        check_numeric(homoplasy_scores, lower = 0, len = length(trees), any.missing = FALSE),
        add = coll
      )

      if (!coll$isEmpty()) {
        val_check <- coll$getMessages()
        cli_abort(c("{.arg homoplasy_scores} must be either {.val NULL} or a valid numeric vector.",
                    "x" = val_check))
      }

      assert(
        check_null(possible_steps),
        assert(
          check_numeric(possible_steps, lower = 0, len = 2, any.missing = FALSE),
          check_subset(names(possible_steps), c("minimum", "maximum")),
          combine = "and",
          add = coll
        ),
        add = coll
      )

      if (!coll$isEmpty()) {
        val_check <- coll$getMessages()
        cli_abort(c("{.arg possible_steps} must be a valid numeric vector.",
                    "x" = val_check))
      }

      assert(
        check_null(node_labels),
        check_tibble(node_labels, min.cols = 2, min.rows = 1, all.missing = FALSE),
        add = coll
      )

      if (!coll$isEmpty()) {
        val_check <- coll$getMessages()
        cli_abort(c("{.arg node_labels} must be a valid {.cls tibble}.",
                    "x" = val_check))
      }

      assert(
        check_null(label_legend),
        check_tibble(label_legend, min.rows = 1, all.missing = FALSE),
        add = coll
      )

      if (!coll$isEmpty()) {
        val_check <- coll$getMessages()
        cli_abort(c("{.arg label_legend} must be a valid {.cls tibble}.",
                    "x" = val_check))
      }

      possible_steps <- as.list(possible_steps)
      tree_data <- data.frame(matrix(nrow = length(trees), ncol = 0))

      if (!test_null(steps)) {
        tree_data$`Tree steps` <- steps

        if (!test_null(homoplasy_scores)) {
          tree_data$`Adjusted homoplasy scores` <- homoplasy_scores
        }

        if (!test_null(possible_steps)) {
          tree_data$CI <- possible_steps$minimum / steps
          tree_data$RI <- (possible_steps$maximum - steps) /
            (possible_steps$maximum - possible_steps$minimum)
          tree_data$RC <- tree_data$CI * tree_data$RI
        }
      }

      if (!test_null(node_labels) & !test_null(label_legend)) {
        new_header <- names(node_labels)[-1]
        names(new_header) <- label_legend$label
        node_labels <- rename(node_labels, new_header)
        label_legend$label <- NULL
      }

      trees <- lapply(trees, as_tibble) %>%
        tibble(tree = .) %>%
        bind_cols(tree_data)

      rf_fn <- function (data) {
        tree <- extract2(data$tree, 1)
        if(!test_null(node_labels)) {
          tree <- full_join(tree, node_labels, by = "node")
        }
        tree <- as.treedata(tree)
        slot(tree, "info") <- list(
          statistics = mutate(data, tree = NULL) %>%
            as.vector()
        )
        list(tree)
      }

      trees <- rowwise(trees) %>%
        reframe(
          tree = rf_fn(across(everything()))
        ) %>%
          use_series("tree")

      private$.trees <- trees
    },
    #' @description
    #' Print a brief summary of the results.
    #'
    #' @param ... Not used.
    print = function(...) {
      cli_text(col_grey("# A ", style_italic(col_red("nitro")), " tree analysis result"))

      options <- c(length(private$.trees)) %>%
        data.frame()

      rownames(options) <- c("Number of trees:")
      names(options) <- NULL

      print(options)
    }
  )
)

#' Convert Tree Analysis Results to phylo
#'
#' @description
#' Extracts the trees from a [TreeAnalysisResults] object and returns them
#' as a `phylo` object (when there is one tree) or a `multiPhylo` object
#' (when there are multiple).
#'
#' @param x A [TreeAnalysisResults] object.
#' @param ... Ignored.
#'
#' @return A `phylo` object if the results contain a single tree, or a
#'   `multiPhylo` object if they contain multiple trees.
#'
#' @seealso
#' * [TreeAnalysisResults] — the results class.
#' * [execute_analysis()] — produces `TreeAnalysisResults` objects.
#'
#' @importFrom ape .compressTipLabel as.phylo
#' @export
as.phylo.TreeAnalysisResults <- function(x, ...) {
  phy <- lapply(x$trees, as.phylo) %>%
    .compressTipLabel()
  if (length(phy) == 1) {
    return(phy[[1]])
  }
  phy
}
