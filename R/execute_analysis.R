#' Execute a Tree Analysis
#'
#' @description
#' Run a configured [TreeAnalysis] using a [TntInterface] and return the
#' results.
#'
#' This is the primary entry point for running a \pkg{nitro} analysis. It
#' assembles all commands from the [TreeAnalysis] and supporting
#' infrastructure into a [CommandQueue], verifies that all required
#' dependencies are satisfied, and passes the queue to
#' [TntInterface]`$execute()`.
#'
#' @details
#' ## Queue assembly
#' The following commands are always added to the queue in addition to
#' those configured in `tree_analysis`:
#'
#' * [EchoCommand] — enables TNT output echoing.
#' * [ScreenSizeCommand] — sets the output buffer dimensions.
#' * [MemoryAllocationCommand] — allocates RAM (controlled by `max_ram`).
#' * [TreeBufferCommand] — sets the tree buffer size (controlled by
#'   `hold`).
#' * [TreeStepsCommand] and [PossibleStepsCommand] — collect tree length
#'   statistics after the search.
#'
#' If `reference_tree` is supplied, a [ReadTreesCommand] is added for the
#' target tree. If `starting_trees` is supplied, a separate
#' [ReadTreesCommand] is added for the starting trees.
#'
#' ## Dependency resolution
#' Dependency resolution is performed incrementally by [CommandQueue] as
#' each command is added via `$enqueue()`. By the time the queue is fully
#' assembled, all required dependencies should be satisfied. If any remain
#' unmet — indicated by `$is_resolved` being `FALSE` — `execute_analysis()`
#' aborts with an informative error before passing the queue to TNT.
#'
#' In normal use this check will not fail, as each command's `$enqueue()`
#' method is responsible for adding its own prerequisites. The check exists
#' as a safeguard against incomplete manual queue construction.
#'
#' @param interface \[`TntInterface`\]\cr
#'   A [TntInterface] object created by [create_interface()].
#' @param tree_analysis \[`TreeAnalysis`\]\cr
#'   A [TreeAnalysis] object configured with data, search settings, and
#'   optional weighting or support commands.
#' @param hold \[`integer(1)`\]\cr
#'   The number of trees to hold in TNT's tree buffer (default: `100`).
#'   Passed to [TreeBufferCommand].
#' @param max_ram \[`numeric(1)`\]\cr
#'   The number of binary megabytes to allocate for TNT (default: `16`).
#'   Passed to [MemoryAllocationCommand].
#' @param reference_tree \[`phylo`]\cr
#'   Optional tree to read into TNT before the analysis begins. Used for
#'   annotating node labels in analyses of group supports or as a backbone
#'   constraint. When supplied, a [ReadTreesCommand] is added to the queue.
#' @param starting_trees \[`phylo`, `multiPhylo`, or `NULL`\]\cr
#'   Optional starting trees to read into TNT before the analysis begins. Used
#'   as a starting point for tree searches. When supplied, a [ReadTreesCommand]
#'   is added to the queue.
#' @param timeout \[`integer(1)` or `NULL`\]\cr
#'   The number of seconds to allow the analysis to run before
#'   terminating. Currently reserved for future use (default: `NULL`).
#'
#' @return A [TreeAnalysisResults] object containing the trees and
#'   associated statistics.
#'
#' @seealso
#' * [create_interface()] — creates the required [TntInterface].
#' * [make_tree_analysis()] — creates a [TreeAnalysis] configuration.
#' * [TreeAnalysis] — the analysis configuration class.
#' * [TreeAnalysisResults] — the results class returned by this function.
#' * [as.phylo.TreeAnalysisResults()] — converts results to `phylo` or
#'   `multiPhylo`.
#'
#' @examples
#' \dontrun{
#' interface <- create_interface("/usr/local/bin/tnt")
#'
#' nex_path <- system.file("extdata", "canale_2022.nex", package = "nitro")
#' dm <- ReadAsPhyDat(nex_path) |> create_matrix()
#'
#' ta <- make_tree_analysis(dm, outgroup = "Herrerasaurus")
#' ta <- set_tree_search(ta, "branch_swapping", replications = 100)
#'
#' results <- execute_analysis(interface, ta, hold = 200, max_ram = 32)
#'
#' # Extract trees
#' trees <- as.phylo(results)
#'
#' # View per-tree statistics
#' results$statistics
#' }
#'
#' @importFrom checkmate check_class check_null test_null test_true
#' @importFrom cli cli_abort
#' @importFrom stringr str_replace str_replace_all str_split str_trim
#' @importFrom utils head tail
#' @export
execute_analysis <- function(interface, tree_analysis, hold = 100, max_ram = 16,
                             reference_tree = NULL, starting_trees = NULL,
                             timeout = NULL) {
  val_check <- check_class(interface, "TntInterface")
  if (!test_true(val_check)) {
    cli_abort(c("{.arg interface} must be a {.arg TntInterface} object.",
                "x" = val_check))
  }

  val_check <- check_class(tree_analysis, "TreeAnalysis")
  if (!test_true(val_check)) {
    cli_abort(c("{.arg tree_analysis} must be a {.arg TreeAnalysis} object.",
                "x" = val_check))
  }

  all_cmds <- Reduce(c, tree_analysis$commands) %>%
    c(
      EchoCommand$new(enable = TRUE),
      ScreenSizeCommand$new(columns = 50000, rows = 25),
      MemoryAllocationCommand$new(size = max_ram),
      TreeBufferCommand$new(size = hold),
      TreeStepsCommand$new(),
      PossibleStepsCommand$new(active_taxa = TRUE)
    )

  if (!test_null(reference_tree)) {
    all_cmds <- c(
      all_cmds,
      ReadTreesCommand$new(
        trees = reference_tree,
        provides = "reference tree"
      )
    )
  }

  if (!test_null(starting_trees)) {
    all_cmds <- c(
      all_cmds,
      ReadTreesCommand$new(
        trees = starting_trees,
        inline = FALSE,
        provides = "starting trees")
    )
  }

  queue <- resolve_dependencies(all_cmds)

  interface$execute(queue)
}
