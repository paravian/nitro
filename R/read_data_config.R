#' Configure Matrix Data for a Tree Analysis
#'
#' @description
#' Add one or more character–taxon matrices to an existing [TreeAnalysis],
#' automatically creating the necessary [ReadDataCommand] and
#' [CharacterCodingCommand] objects from the matrix settings.
#'
#' Character inactivity and ordering are read directly from the matrix
#' objects and translated into the correct character index offsets when
#' multiple matrices are combined.
#'
#' @param tree_analysis \[`TreeAnalysis`\]\cr
#'   A [TreeAnalysis] object to add the data to.
#' @param data \[`AbstractCharacterMatrix` or `MultiCharacterMatrix`\]\cr
#'   One or more [DiscreteMatrix] or [ContinuousMatrix] objects created by
#'   [create_matrix()].
#'
#' @return The modified [TreeAnalysis] object with data commands attached.
#'   Note that unlike [set_tree_search()], [set_weighting()], and
#'   [set_support()], this function modifies `tree_analysis` in place
#'   rather than returning a clone.
#'
#' @seealso
#' * [create_matrix()] — creates the matrix objects required by `data`.
#' * [make_tree_analysis()] — the recommended way to create a
#'   [TreeAnalysis] with data already attached.
#' * [ReadDataCommand] — the command created for the matrix data.
#' * [CharacterCodingCommand] — the command created for inactive and
#'   ordered characters.
#'
#' @examples
#' \dontrun{
#' # --- Discrete matrix ---
#' nex_path <- system.file("extdata", "canale_2022.nex", package = "nitro")
#' dm <- ReadAsPhyDat(nex_path) |> create_matrix()
#' dm$ordered <- c(3, 7)
#' dm$inactive <- c(1)
#'
#' ta <- TreeAnalysis$new()
#' read_data_config(ta, dm)
#'
#' # --- Combined discrete + continuous ---
#' nex_path2 <- system.file("extdata", "raven_2017.nex", package = "nitro")
#' csv_path  <- system.file("extdata", "raven_2017.csv", package = "nitro")
#' dm2 <- ReadAsPhyDat(nex_path2) |> create_matrix()
#' cm  <- read.table(csv_path, sep = ",", header = TRUE) |> create_matrix()
#'
#' ta2 <- TreeAnalysis$new()
#' read_data_config(ta2, c(dm2, cm))
#' }
#'
#' @importFrom checkmate check_class test_class test_list test_null test_true
#' @importFrom cli cli_abort
#' @export
read_data_config <- function(tree_analysis, data) {
  val_check <- check_class(tree_analysis, "TreeAnalysis")

  if (!test_true(val_check)) {
    cli_abort(c("{.arg tree_analysis} must be an object of class {.obj TreeAnalysis}.",
                "x" = val_check))
  }

  all_data <- ReadDataCommand$new(data)

  if (!test_list(data)) {
    data <- c(data)
  }

  n <- 0
  any_continuous <- FALSE
  inactive_indices <- NULL
  ordered_indices <- NULL
  for (mtx in data) {
    is_continuous <- test_class(mtx, "ContinuousMatrix")
    any_continuous <- any_continuous | is_continuous

    if (!is_continuous) {
      if (!test_null(mtx$ordered)) {
        ordered_indices <- c(
          ordered_indices,
          mtx$ordered + n - 1
        )
      }
    }

    if (!test_null(mtx$inactive)) {
      inactive_indices <- c(
        inactive_indices,
        mtx$inactive + n - 1
      )
    }
    n <- n + mtx$n_characters
  }

  all_cmds <- c(all_data)

  if (length(inactive_indices) > 0 | length(ordered_indices) > 0) {
    char_coding <- CharacterCodingCommand$new(
      inactive_indices = inactive_indices,
      ordered_indices = ordered_indices
    )
    all_cmds <- c(
      all_cmds,
      char_coding
    )
  }

  tree_analysis$add_command(all_cmds)
  invisible(tree_analysis)
}
