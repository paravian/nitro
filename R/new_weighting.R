#' Create a Weighting Configuration
#'
#' @description
#' Create a new implied weighting command object by name. This is the
#' recommended way to instantiate weighting commands in \pkg{nitro}, rather
#' than calling R6 constructors directly.
#'
#' @param name \[`character(1)`\]\cr
#'   The name of the weighting method. Partial unambiguous matches are
#'   accepted (case-insensitive, underscores optional). The available
#'   methods are:
#'
#'   | Name | Creates | TNT command |
#'   |------|---------|-------------|
#'   | `"implied_weighting"` | [ImpliedWeightingCommand] | `piwe` |
#'   | `"extended_implied_weighting"` | [ExtendedImpliedWeightingCommand] | `xpiwe` |
#'
#' @param ... Optional named arguments passed to the constructor of the
#'   selected command class. See [ImpliedWeightingCommand] and
#'   [ExtendedImpliedWeightingCommand] for available parameters.
#'
#' @return An [ImpliedWeightingCommand] or
#'   [ExtendedImpliedWeightingCommand] object, depending on `name`.
#'
#' @seealso
#' * [set_weighting()] — create and attach a weighting command to a
#'   [TreeAnalysis] in one step, with automatic handling of the dependency
#'   between the two command classes.
#' * [ImpliedWeightingCommand], [ExtendedImpliedWeightingCommand] —
#'   individual command classes with full parameter documentation.
#'
#' @examples
#' # Standard implied weighting with default concavity constant
#' iw <- new_weighting("implied_weighting")
#'
#' # Partial matching works
#' iw <- new_weighting("implied")
#'
#' # Pass arguments to the constructor
#' iw <- new_weighting("implied_weighting", concavity_constant = 6)
#'
#' # Extended implied weighting
#' eiw <- new_weighting("extended_implied_weighting")
#'
#' @importFrom checkmate check_list check_string test_null test_true
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @export
new_weighting <- function(name, ...) {
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
    "ImpliedWeightingCommand",
    "ExtendedImpliedWeightingCommand"
  )

  args <- list(
    name = name,
    object_choice = object_choice
  ) %>%
    c(args)

  weighting_obj <- do.call(create_new_object, args)

  weighting_obj
}

#' Add a Weighting Method to a Tree Analysis
#'
#' @description
#' Create a weighting command object by name and add it to an existing
#' [TreeAnalysis]. This is a convenience wrapper around [new_weighting()]
#' that handles creation and attachment in one step.
#'
#' @details
#' ## Automatic dependency handling
#' When `name` resolves to `"extended_implied_weighting"`, `set_weighting()`
#' creates an [ExtendedImpliedWeightingCommand]. The required
#' [ImpliedWeightingCommand] dependency is satisfied automatically when the
#' queue is executed: [ExtendedImpliedWeightingCommand]`$enqueue()`
#' creates and enqueues a correctly configured [ImpliedWeightingCommand]
#' before adding itself. No manual handling of the dependency is required.
#'
#' When `name` resolves to `"implied_weighting"`, only an
#' [ImpliedWeightingCommand] is created. Use `"extended_implied_weighting"`
#' if you need per-character concavity constants.
#'
#' @param tree_analysis \[`TreeAnalysis`\]\cr
#'   A [TreeAnalysis] object to add the weighting method to.
#' @param name \[`character(1)`\]\cr
#'   The name of the weighting method. See [new_weighting()] for the full
#'   list of available methods and accepted names.
#' @param ... Optional named arguments passed to the constructor of the
#'   selected command class. See [ImpliedWeightingCommand] and
#'   [ExtendedImpliedWeightingCommand] for available parameters.
#'
#' @return A **copy** of `tree_analysis` with the weighting command added.
#'   The original object is not modified.
#'
#' @seealso
#' * [new_weighting()] — create a weighting command without attaching it
#'   to an analysis.
#' * [ImpliedWeightingCommand] — standard implied weighting.
#' * [ExtendedImpliedWeightingCommand] — extended implied weighting with
#'   per-character concavity constants.
#' * [TreeAnalysis] — the analysis container class.
#'
#' @examples
#' \dontrun{
#' nex_path <- system.file("extdata", "canale_2022.nex", package = "nitro")
#' dm <- ReadPhyDat(nex_path) |> create_matrix()
#' ta <- make_tree_analysis(dm, outgroup = "Herrerasauridae")
#' ta <- set_tree_search(ta, "branch_swapping", replications = 100)
#'
#' # Standard implied weighting
#' ta <- set_weighting(ta, "implied_weighting", concavity_constant = 6)
#'
#' # Extended implied weighting with multi-constant mode
#' ta <- set_weighting(ta, "extended_implied_weighting",
#'                     concavity_constant = 6,
#'                     multi_constant     = TRUE,
#'                     proportion         = 0.3,
#'                     max_ratio          = 3)
#' }
#'
#' @importFrom checkmate check_class test_true
#' @importFrom cli cli_abort
#' @export
set_weighting <- function(tree_analysis, name, ...) {
  val_check <- check_class(tree_analysis, "TreeAnalysis")
  if (!test_true(val_check)) {
    cli_abort(c("{.arg tree_analysis} must be a {.obj TreeAnalysis} object.",
                "x" = val_check))
  }

  weighting_obj <- new_weighting(name, ...)

  tree_analysis$add_command(weighting_obj)

  tree_analysis
}
