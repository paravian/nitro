#' Create a Tree Search Configuration
#'
#' @description
#' Create a new tree search command object by name. This is the recommended
#' way to instantiate tree search commands in \pkg{nitro}, rather than
#' calling R6 constructors directly.
#'
#' @param name \[`character(1)`\]\cr
#'   The name of the search method. Partial unambiguous matches are
#'   accepted (case-insensitive, underscores optional). The available
#'   methods are:
#'
#'   | Name | Creates | TNT command |
#'   |------|---------|-------------|
#'   | `"branch_breaking"` | [BranchBreakingCommand] | `bbreak` |
#'   | `"branch_swapping"` | [BranchSwappingCommand] | `mult` |
#'   | `"constrained_sectorial_search"` | [ConstrainedSectorialSearchCommand] | `sectsch` (css) |
#'   | `"exclusive_sectorial_search"` | [ExclusiveSectorialSearchCommand] | `sectsch` (xss) |
#'   | `"extra_methods_search"` | [ExtraSearchMethodsCommand] | `xmult` |
#'   | `"random_sectorial_search"` | `RandomSectorialSearchCommand` | `sectsch` (rss) |
#'   | `"ratchet"` | [RatchetCommand] | `ratchet` |
#'   | `"tree_drifting"` | [TreeDriftingCommand] | `drift` |
#'   | `"tree_fusing"` | [TreeFusingCommand] | `tfuse` |
#'   | `"tree_hybridizing"` | [TreeHybridizingCommand] | `tfuse` (hybrid) |
#'
#' @param ... Optional named arguments passed to the constructor of the
#'   selected command class. See the documentation for each class for
#'   available parameters.
#'
#' @return A [TreeSearchCommand] object (the specific subclass depends on
#'   `name`).
#'
#' @seealso
#' * [set_tree_search()] — create and add a tree search to a
#'   [TreeAnalysis] in one step.
#' * [BranchSwappingCommand], [BranchBreakingCommand],
#'   [ExtraSearchMethodsCommand], [RatchetCommand],
#'   [TreeDriftingCommand], [TreeFusingCommand],
#'   [TreeHybridizingCommand],
#'   [ConstrainedSectorialSearchCommand],
#'   [ExclusiveSectorialSearchCommand] — individual command classes
#'   with full parameter documentation.
#'
#' @examples
#' # Create a branch swapping search with defaults
#' bs <- new_tree_search("branch_swapping")
#'
#' # Partial matching works
#' bs <- new_tree_search("branch_swap")
#'
#' # Pass arguments to the constructor
#' bs <- new_tree_search("branch_swapping", replications = 100,
#'                        hold_rep = 25)
#'
#' # Create an extra methods (driven) search
#' xm <- new_tree_search("extra_methods")
#'
#' @importFrom checkmate check_list check_string test_null test_true
#' @importFrom cli cli_abort
#' @export
new_tree_search <- function(name, ...) {
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

  pkg_objs <- get_package_objects()
  object_choice <- c()

  for (obj in pkg_objs) {
    r6_obj <- try(get(obj)$new(), silent = TRUE)
    if (inherits(r6_obj, "try-error")) {
      next()
    }

    if (inherits(r6_obj, "TreeSearchCommand")) {
      object_choice <- c(object_choice, obj)
    }
  }

  args <- list(
    name = name,
    object_choice = object_choice
  ) %>%
    c(args)

  tree_search_obj <- do.call(create_new_object, args)
  tree_search_obj
}

#' Add a Tree Search to a Tree Analysis
#'
#' @description
#' Create a tree search command object by name and add it to an existing
#' [TreeAnalysis]. This is a convenience wrapper around [new_tree_search()]
#' that handles the creation and attachment in one step.
#'
#' @param tree_analysis \[`TreeAnalysis`\]\cr
#'   A [TreeAnalysis] object to add the search method to.
#' @param name \[`character(1)`\]\cr
#'   The name of the search method. See [new_tree_search()] for the full
#'   list of available methods and accepted names.
#' @param ... Optional named arguments passed to the constructor of the
#'   selected command class. See the documentation for each class for
#'   available parameters.
#'
#' @return A **copy** of `tree_analysis` with the tree search command
#'   added. The original object is not modified.
#'
#' @seealso
#' * [new_tree_search()] — create a tree search command without attaching
#'   it to an analysis.
#' * [TreeAnalysis] — the analysis container class.
#'
#' @examples
#' \dontrun{
#' nex_path <- system.file("extdata", "canale_2022.nex", package = "nitro")
#' dm <- ReadAsPhyDat(nex_path) |> create_matrix()
#' ta <- make_tree_analysis(dm, outgroup = "Herrerasaurus")
#'
#' # Traditional branch swapping search
#' ta <- set_tree_search(ta, "branch_swapping", replications = 100)
#'
#' # Driven search with default sub-strategies
#' ta <- set_tree_search(ta, "driven_search")
#'
#' # Inspect the search configuration
#' ta$commands$`tree search`$format()
#' }
#'
#' @importFrom checkmate check_class test_class test_true
#' @importFrom cli cli_abort
#' @export
set_tree_search <- function(tree_analysis, name, ...) {
  val_check <- check_class(tree_analysis, "TreeAnalysis")
  if (!test_true(val_check)) {
    cli_abort(c("{.arg tree_analysis} must be a {.obj TreeAnalysis} object."))
  }

  tree_search_obj <- new_tree_search(name, ...)

  if (test_class(tree_search_obj, "ExtraSearchMethodsCommand")) {
    set_sel_size <- function(extra_methods_class) {
      sect_classes <- sapply(extra_methods_class$sectorial_search, function(x) {
        class(x)[1]
      })
      if ("RandomSectorialSearchCommand" %in% sect_classes) {
        def_size <- min(ceiling(tree_analysis$n_taxa / 2), 45L)
        idx <- which(sect_classes == "RandomSectorialSearchCommand")
        if (extra_methods_class$sectorial_search[[idx]]$max_size == "auto") {
          extra_methods_class$sectorial_search[[idx]]$max_size <- def_size
        }
        if (extra_methods_class$sectorial_search[[idx]]$min_size == "auto") {
          extra_methods_class$sectorial_search[[idx]]$min_size <- def_size
        }
      }
      return(extra_methods_class)
    }

    tree_search_obj <- set_sel_size(tree_search_obj)
  }

  ta <- tree_analysis$clone(deep = TRUE)
  ta$add_command(tree_search_obj)

  ta
}
