#' Create a Group Support Command
#'
#' @description
#' Create a new group support command object by name. This is the
#' recommended way to instantiate support command objects in \pkg{nitro},
#' rather than calling R6 constructors directly.
#'
#' @param name \[`character(1)`\]\cr
#'   The name of the support method. Partial unambiguous matches are
#'   accepted (case-insensitive). The available methods are:
#'
#'   | Name | Creates | Support type |
#'   |------|---------|--------------|
#'   | `"branch"` | [BranchSupportCommand] | Bremer (branch) support |
#'   | `"bootstrap"` | [ResamplingCommand] | Bootstrap resampling |
#'   | `"jackknife"` | [ResamplingCommand] | Jackknife resampling |
#'   | `"symmetric"` | [ResamplingCommand] | Symmetric resampling |
#'
#' @param ... Optional named arguments passed to the constructor of the
#'   selected command class. For [BranchSupportCommand], the
#'   `suboptimal_steps` argument is required. See [BranchSupportCommand]
#'   and [ResamplingCommand] for the full list of available parameters.
#'
#' @return A [BranchSupportCommand] or [ResamplingCommand] object,
#'   depending on `name`.
#'
#' @seealso
#' * [set_support()] — create a support command and attach it to a
#'   [TreeAnalysis] in one step.
#' * [BranchSupportCommand] — branch (Bremer) support configuration,
#'   including details of the suboptimal sampling approach.
#' * [ResamplingCommand] — resampling-based support configuration.
#'
#' @examples
#' # Branch (Bremer) support with a five-step suboptimality schedule
#' bs <- new_support("branch", suboptimal_steps = 1:5)
#'
#' # Bootstrap resampling
#' boot <- new_support("bootstrap", replications = 1000)
#'
#' # Jackknife resampling
#' jack <- new_support("jackknife", replications = 1000)
#'
#' @importFrom checkmate check_list check_string test_null test_true
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @importFrom stringr str_to_lower
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

#' Add a Group Support Analysis to a Tree Analysis
#'
#' @description
#' Create a group support command object by name and add it to an existing
#' [TreeAnalysis]. This is a convenience wrapper around [new_support()]
#' that handles creation, validation, and attachment in one step.
#'
#' @details
#' ## Zero-length branch rule
#' Resampling analyses (bootstrap, jackknife, symmetric resampling)
#' require a compatible zero-length branch rule. The `$zlb_rule` of the
#' supplied [TreeAnalysis] must be one of `"maximum"`,
#' `"identical_states"`, or `"minimum"`. An informative error is raised
#' if an incompatible rule is set.
#'
#' ## Branch support dependencies
#' When `name` resolves to `"branch"`, the [BranchSupportCommand] requires
#' a [BranchBreakingCommand] and a [TreeBufferCommand] to be present in
#' the [TreeAnalysis] as resolved dependencies. These are typically added
#' automatically when a tree search is configured and executed via
#' [set_tree_search()] and [execute_analysis()] respectively.
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
#' * [BranchSupportCommand] — branch (Bremer) support configuration.
#' * [ResamplingCommand] — resampling-based support configuration.
#' * [TreeAnalysis] — the analysis container class.
#'
#' @examples
#' \dontrun{
#' nex_path <- system.file("extdata", "canale_2022.nex", package = "nitro")
#' dm <- ReadAsPhyDat(nex_path) |> create_matrix()
#' ta <- make_tree_analysis(dm, outgroup = "Abelisauridae")
#' ta <- set_tree_search(ta, "branch_swapping", replications = 100)
#'
#' # Branch (Bremer) support with a five-step suboptimality schedule
#' ta <- set_support(ta, "branch", suboptimal_steps = 1:5)
#'
#' # Bootstrap resampling
#' ta <- set_support(ta, "bootstrap", replications = 1000)
#'
#' # Jackknife with custom deletion probability
#' ta <- set_support(ta, "jackknife", replications = 500, probability = 25)
#'
#' # Symmetric resampling with multiple frequency summaries
#' ta <- set_support(ta, "symmetric",
#'                   replications      = 500,
#'                   frequency_summary = c("absolute", "difference"))
#' }
#'
#' @importFrom checkmate check_class test_true
#' @importFrom cli cli_abort
#' @export
set_support <- function(tree_analysis, name, ...) {
  val_check <- check_class(tree_analysis, "TreeAnalysis")
  if (!test_true(val_check)) {
    cli_abort(c("{.arg tree_analysis} must be a {.obj TreeAnalysis} object."))
  }

  support_obj <- new_support(name, ...)

  ta <- tree_analysis$clone(deep = TRUE)
  ta$add_command(support_obj)

  ta
}
