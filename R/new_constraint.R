#' Create a Topological Constraint
#'
#' @description
#' Create a new topological constraint object by name. This is the
#' recommended way to instantiate constraint objects in \pkg{nitro},
#' rather than calling R6 constructors directly.
#'
#' @param name \[`character(1)`\]\cr
#'   The name of the constraint type. Partial unambiguous matches are
#'   accepted (case-insensitive, underscores optional). The available
#'   types are:
#'
#'   | Name | Creates |
#'   |------|---------|
#'   | `"monophyly"` | [MonophylyConstraint] |
#'   | `"backbone"` | [BackboneConstraint] |
#'
#' @param ... Optional named arguments passed to the constructor of the
#'   selected constraint class. See [MonophylyConstraint] and
#'   [BackboneConstraint] for available parameters.
#'
#' @return A [MonophylyConstraint] or [BackboneConstraint] object,
#'   depending on `name`.
#'
#' @seealso
#' * [set_constraint()] — create constraints and attach them to a
#'   [TreeAnalysis] in one step.
#' * [MonophylyConstraint], [BackboneConstraint] — individual constraint
#'   classes with full parameter documentation.
#'
#' @examples
#' # Monophyly constraint
#' mc <- new_constraint("monophyly",
#'                      fixed_otus = c("TaxonA", "TaxonB", "TaxonC"))
#'
#' # Backbone constraint
#' bc <- new_constraint("backbone")
#'
#' @importFrom checkmate check_list check_string test_null test_true
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @export
new_constraint <- function(name, ...) {
  val_check <- check_string(name, min.chars = 1)
  if (!test_true(val_check)) {
    cli_abort(c("{.arg name} must be a string.",
                "x" = val_check))
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
    "MonophylyConstraint",
    "BackboneConstraint"
  )

  args <- list(
    name          = name,
    object_choice = object_choice
  ) %>%
    c(args)

  do.call(create_new_object, args)
}

#' Add Topological Constraints to a Tree Analysis
#'
#' @description
#' Create a [TopologicalConstraintsCommand] from one or more constraint
#' objects and add it to an existing [TreeAnalysis]. This is a convenience
#' wrapper that handles creation, queue integration, and attachment in one
#' step.
#'
#' @details
#' ## Queue integration
#' `set_constraint()` adds a [TopologicalConstraintsCommand] at priority
#' `401` and a [ConstrainCommand] at priority `411`, ensuring that
#' constraints are defined and enforcement is activated before the tree
#' search begins.
#'
#' ## Backbone constraints and reference trees
#' When any supplied constraint is a [BackboneConstraint], the reference
#' tree must be passed to `execute_analysis()` via the `reference_tree`
#' argument. The `"reference tree"` dependency on
#' [TopologicalConstraintsCommand] will be resolved automatically at that
#' point.
#'
#' @param tree_analysis \[`TreeAnalysis`\]\cr
#'   A [TreeAnalysis] object to add the constraints to.
#' @param ... One or more [MonophylyConstraint] or [BackboneConstraint]
#'   objects.
#'
#' @return A **copy** of `tree_analysis` with the constraint commands
#'   added. The original object is not modified.
#'
#' @seealso
#' * [new_constraint()] — create a constraint object without attaching it
#'   to an analysis.
#' * [MonophylyConstraint] — monophyly constraint type.
#' * [BackboneConstraint] — backbone constraint type.
#' * [TopologicalConstraintsCommand] — the command object created
#'   internally by this function.
#' * [execute_analysis()] — pass `target_tree` here when using a backbone
#'   constraint.
#'
#' @examples
#' \dontrun{
#' nex_path <- system.file("extdata", "canale_2022.nex", package = "nitro")
#' dm <- ReadAsPhyDat(nex_path) |> create_matrix()
#' ta <- make_tree_analysis(dm, outgroup = "Abelisauridae")
#' ta <- set_tree_search(ta, name = "branch_swapping", replications = 1000)
#'
#' # Monophyly constraint
#' mc <- new_constraint("monophyly",
#'                      fixed_otus = c("TaxonA", "TaxonB", "TaxonC"))
#' ta <- set_constraint(ta, mc)
#'
#' # Multiple constraints
#' mc2 <- new_constraint("monophyly",
#'                       fixed_otus  = c("TaxonD", "TaxonE"),
#'                       is_positive = FALSE)
#' ta <- set_constraint(ta, mc, mc2)
#'
#' results <- execute_analysis(interface, ta, hold = 10000)
#' }
#'
#' @importFrom checkmate check_class test_true
#' @importFrom cli cli_abort
#' @export
set_constraint <- function(tree_analysis, ...) {
  val_check <- check_class(tree_analysis, "TreeAnalysis")
  if (!test_true(val_check)) {
    cli_abort(c("{.arg tree_analysis} must be a {.cls TreeAnalysis} object.",
                "x" = val_check))
  }

  constraint_obj <- TopologicalConstraintsCommand$new(
    constraints = list(...)
  )

  ta <- tree_analysis$clone(deep = TRUE)
  ta$add_command(constraint_obj)
  ta
}
