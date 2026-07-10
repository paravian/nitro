#' Create a Tree Analysis Configuration
#'
#' @description
#' Create a new [TreeAnalysis] object with data, taxon activity, outgroup,
#' and zero-length branch rule configured in one step.
#'
#' This is the recommended way to create a [TreeAnalysis] for use with
#' [execute_analysis()]. After calling `make_tree_analysis()`, use
#' [set_tree_search()], [set_weighting()], and [set_support()] to add
#' analysis commands.
#'
#' @param data \[`AbstractCharacterMatrix`, `MultiCharacterMatrix`\]\cr
#'   One or more [DiscreteMatrix] or [ContinuousMatrix] objects created by
#'   [create_matrix()].
#' @param inactive_taxa \[`character` or `NULL`\]\cr
#'   Names of taxa to exclude from the analysis (default: `NULL`).
#' @param outgroup \[`character(1)` or `NULL`\]\cr
#'   The name of the outgroup taxon (default: `NULL`).
#' @param zlb_rule \[`character(1)`\]\cr
#'   The rule for collapsing zero-length branches (default: `"minimum"`).
#'   See [CollapseRuleCommand] for the full list of options.
#'
#' @return A [TreeAnalysis] object with data and configuration commands
#'   attached.
#'
#' @seealso
#' * [create_matrix()] — creates the matrix objects required by `data`.
#' * [set_tree_search()] — add a tree search strategy.
#' * [set_weighting()] — add an implied weighting configuration.
#' * [set_support()] — add a support analysis.
#' * [execute_analysis()] — run the configured analysis.
#' * [CollapseRuleCommand] — documents the available `zlb_rule` options.
#'
#' @examples
#' \dontrun{
#' # --- Discrete matrix only ---
#' nex_path <- system.file("extdata", "canale_2022.nex", package = "nitro")
#' dm <- ReadAsPhyDat(nex_path) |> create_matrix()
#'
#' ta <- make_tree_analysis(
#'   data      = dm,
#'   outgroup  = "Abelisauridae",
#'   zlb_rule  = "minimum"
#' )
#'
#' # --- Combined discrete + continuous matrix ---
#' nex_path2 <- system.file("extdata", "raven_2017.nex", package = "nitro")
#' csv_path <- system.file("extdata", "raven_2017.csv", package = "nitro")
#' dm2 <- ReadAsPhyDat(nex_path2) |> create_matrix()
#' cm <- read.table(csv_path, sep = ",", header = TRUE) |> create_matrix()
#'
#' ta2 <- make_tree_analysis(
#'   data     = c(dm2, cm),
#'   zlb_rule = "minimum"
#' )
#' }
#'
#' @importFrom checkmate test_null
#' @export
make_tree_analysis <- function(data, inactive_taxa = NULL, outgroup = NULL,
                               zlb_rule = "minimum") {
  a <- as.list(environment(), all = TRUE)
  tree_analysis <- do.call(TreeAnalysis$new, a)

  cmds <- c(
    CollapseRuleCommand$new(rule = tree_analysis$zlb_rule),
    OutgroupCommand$new(taxon_name = tree_analysis$outgroup)
  )

  if (!test_null(inactive_taxa)) {
    cmds <- c(
      cmds,
      TaxonActivityCommand$new(inactive_taxa = tree_analysis$inactive_taxa)
    )
  }

  tree_analysis$add_command(cmds)

  tree_analysis
}
