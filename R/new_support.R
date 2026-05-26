#' New support method configuration
#'
#' @description
#' Creates a new support method configuration.
#' @param name The name of a support method. Partial unambiguous matches are
#'   also accepted. The valid options are:
#'   \itemize{
#'     \item \code{branch}: Calculates branch (i.e. Bremer) support statistics.
#'       Attaches a \code{\link{BranchSupportCommand}} object.
#'     \item \code{bootstrap}: Calculates bootstrap resampling support
#'       statistics. Attaches a \code{\link{ResamplingCommand}} object.
#'     \item \code{jackknife}: Calculates jackknife resmpling support
#'       statistics. Attaches a \code{\link{ResamplingCommand}} object.
#'     \item \code{symmetric}: Calculate symmetric resampling support
#'       statistics. Attaches a \code{\link{ResamplingCommand}} object.
#'   }
#' @param ... Arguments to be passed on to the weighting method.
#' @returns A \code{\link{ResamplingCommand}} object.
#' @seealso The \code{new} method of \code{\link{BranchSupportCommand}} and
#'   \code{\link{ResamplingCommand}}.
#' @importFrom checkmate check_class check_string test_null test_true
#' @importFrom cli cli_abort
#' @export
new_support <- function (name, ...) {
  val_check <- check_string(name, min.chars = 1)
  if (!test_true(val_check)) {
    cli_abort(c("{.arg name} must be a string.",
                x = val_check))
  }

  args <- list()
  if (!missing(...)) {
    args <- list(...)
    if (test_null(unlist(args))) {
      args <- list()
    } else {
      val_check <- check_list(args, names = "named")
      if (!test_true(val_check)) {
        cli_abort(c("Additional arguments must all be named.",
                    "x" = val_check))
      }
    }
  }

  object_choice <- c(
    "BranchSupportCommand",
    "ResamplingCommand"
  )

  args <- list(
    name = name,
    object_choice = object_choice
  ) %>%
    c(args)

  resampling_choice <- c(
    "Bootstrap",
    "Jackknife",
    "SymmetricResampling"
  )

  resampling_idx <- pmatch(name, str_to_lower(resampling_choice))

  if (!is.na(resampling_idx)) {
    args$method <- name
    args$name <- "ResamplingCommand"
  }

  resampling_obj <- do.call(create_new_object, args)
  resampling_obj
}

#' Add a Support Analysis to a Tree Analysis
#'
#' @description
#' Create a support analysis command object by name and add it to an
#' existing [TreeAnalysis]. This is a convenience wrapper around
#' [new_support()] that handles creation, validation, and attachment in
#' one step.
#'
#' @details
#' ## Zero-length branch rule validation
#' Resampling analyses (bootstrap, jackknife, symmetric) require a
#' compatible zero-length branch rule. The `$zlb_rule` of the supplied
#' [TreeAnalysis] must be one of `"maximum"`, `"identical_states"`, or
#' `"minimum"`. An informative error is raised if an incompatible rule is
#' set.
#'
#' ## Branch support
#' When `name` resolves to `"branch"`, any arguments not recognised by
#' `BranchSupportCommand` are automatically routed to a
#' `SuboptimalCommand`, which is also added to the analysis.
#'
#' @param tree_analysis \[`TreeAnalysis`\]\cr
#'   A [TreeAnalysis] object to add the support analysis to.
#' @param name \[`character(1)`\]\cr
#'   The name of the support method. See [new_support()] for the full
#'   list of available methods and accepted names.
#' @param ... Optional named arguments passed to the constructor of the
#'   selected command class.
#'
#' @return A **copy** of `tree_analysis` with the support command added.
#'   The original object is not modified.
#'
#' @seealso
#' * [new_support()] — create a support command without attaching it to
#'   an analysis.
#' * [ResamplingCommand] — the command class for resampling analyses.
#' * [TreeAnalysis] — the analysis container class.
#'
#' @examples
#' \dontrun{
#' nex_path <- system.file("extdata", "canale_2022.nex", package = "nitro")
#' dm <- ReadAsPhyDat(nex_path) |> create_matrix()
#' ta <- make_tree_analysis(dm, outgroup = "Abelisauridae")
#' ta <- set_tree_search(ta, "branch_swapping", replications = 100)
#'
#' # Bootstrap resampling
#' ta <- set_support(ta, "bootstrap", replications = 1000)
#'
#' # Jackknife with custom deletion probability
#' ta <- set_support(ta, "jackknife", replications = 500, probability = 25)
#'
#' # Symmetric resampling with multiple frequency summaries
#' ta <- set_support(ta, "symmetric",
#'                   replications       = 500,
#'                   frequency_summary  = c("absolute", "difference"))
#' }
#'
#' @importFrom checkmate check_class check_subset test_true
#' @importFrom cli cli_abort
#' @importFrom stringr str_replace_all
#' @export
set_support <- function(tree_analysis, name, ...) {
  val_check <- check_class(tree_analysis, "TreeAnalysis")
  if (!test_true(val_check)) {
    cli_abort(c("{.arg tree_analysis} must be a {.obj TreeAnalysis} object."))
  }

  args <- list(...)
  support_obj <- new_support(name)

  ta <- tree_analysis$clone(deep = TRUE)
  ta$add_command(support_obj)

  ta
}
