#' Topological Constraints Command
#'
#' @description
#' An [R6][R6::R6Class] class that holds one or more topological
#' constraints and renders the TNT `force` command to apply them during
#' tree searches in \pkg{nitro}.
#'
#' Each constraint is either a [MonophylyConstraint], which constrains a
#' named set of taxa to be monophyletic or non-monophyletic, or a
#' [BackboneConstraint], which constrains the search to recover a topology
#' compatible with a reference tree. Multiple constraints of either type
#' can be combined in a single `TopologicalConstraintsCommand`.
#'
#' Constraints are activated during a search by a companion
#' [ConstrainCommand] object, which is enqueued automatically by
#' [set_constraint()].
#'
#' @details
#' ## Command output
#' `$render()` produces one or more `force` statements of the form:
#' * Positive monophyly: `force + [ TaxonA TaxonB TaxonC ];`
#' * Negative monophyly: `force - [ TaxonA TaxonB ];`
#' * With floating OTUs: `force + [ TaxonA TaxonB (TaxonC TaxonD) ];`
#' * Positive backbone: `force / &0;`
#' * Negative backbone: `force : &0;`
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `401`. A [ConstrainCommand] enabling constraint enforcement is added
#' separately at priority `411` by [set_constraint()].
#'
#' ## Dependencies
#' * `"matrix"` (required) — a [ReadDataCommand] object. Used to validate
#'   that all taxon names in the constraints are present in the matrix.
#' * `"reference tree"` (optional) — a [ReadTreesCommand] object. Required
#'   when any constraint is a [BackboneConstraint].
#'
#' @seealso
#' * [MonophylyConstraint] — monophyly constraint type.
#' * [BackboneConstraint] — backbone constraint type.
#' * [ConstrainCommand] — enables constraint enforcement during searches.
#' * [set_constraint()] — recommended way to attach constraints to a
#'   [TreeAnalysis].
#' * [new_constraint()] — create a constraint object by name.
#'
#' @keywords internal
#' @importFrom checkmate assert check_class check_list check_null
#'   makeAssertCollection test_class test_list test_null test_true
#' @importFrom cli cli_abort cli_text col_grey style_italic col_red
#' @importFrom glue glue
#' @importFrom magrittr extract2
#' @importFrom R6 R6Class
TopologicalConstraintsCommand <- R6Class(
  "TopologicalConstraintsCommand",
  inherit = BasicCommand,
  private = list(
    .constraints = NULL
  ),
  active = list(
    #' @field constraints \[`AbstractConstraint` or `list`\]\cr
    #'   One or more [AbstractConstraint] objects (i.e., [MonophylyConstraint]
    #'   or [BackboneConstraint]) defining the topological constraints to
    #'   enforce. A single object is coerced to a list internally.
    constraints = function(value) {
      if (missing(value)) {
        return(private$.constraints)
      } else {
        if (!test_list(value)) {
          value <- c(value)
        }

        coll <- makeAssertCollection()
        assert(
          check_list(value, types = "MonophylyConstraint", any.missing = FALSE, min.len = 1, unique = TRUE),
          check_list(value, types = "MultiMonophylyConstraint", any.missing = FALSE, min.len = 1, unique = TRUE),
          check_list(value, types = "BackboneConstraint", any.missing = FALSE, min.len = 1, unique = TRUE),
          add = coll
        )

        if (!coll$isEmpty()) {
          val_check <- coll$getMessages()
          cli_abort(c("{.arg constraints} must be either {.cls BackboneConstraint} object a (list of) {.cls MonophylyConstraint} object(s).",
                      "x" = val_check))
        }

        private$.constraints <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Adds this command at priority `401`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      con_cmd <- ConstrainCommand$new(enable = TRUE)
      .queue <- con_cmd$enqueue(.queue)

      .queue$add(self, 401)
      .queue
    },
    #' @description
    #' Format the command as a summary table.
    #'
    #' @param ... Not used.
    #'
    #' @return A `data.frame` with columns for description and value.
    format = function(...) {
      options <- data.frame(
        c("Number of constraints:"),
        c(length(self$constraints))
      )
      names(options) <- c("", "Value")
      options[, 1] <- format(options[, 1], justify = "left")
      options
    },
    #' @description
    #' Create a new `TopologicalConstraintsCommand` object.
    #'
    #' In most cases, [set_constraint()] is the recommended way to create
    #' and attach this command to a [TreeAnalysis].
    #'
    #' @param constraints \[`AbstractConstraint` or `list`\]\cr
    #'   One or more [MonophylyConstraint] or [BackboneConstraint] objects.
    #'   See the `$constraints` field.
    #'
    #' @return A new `TopologicalConstraintsCommand` object.
    initialize = function(constraints) {
      a <- as.list(environment(), all = TRUE)

      super$initialize(
        name        = "force",
        description = "Set topological constraints",
        provides    = "constraints"
      )

      validate_matrix <- function(value) {
        val_check <- check_class(value, "ReadDataCommand")
        if (!test_true(val_check)) {
          cli_abort(c("{.arg value} must be a {.cls ReadDataCommand} object.",
                      "x" = val_check))
        }

        all_taxa <- value$data %>%
          sapply(getElement, "taxa") %>%
          as.vector() %>%
          unique()

        for (constraint in self$constraints) {
          if (test_class(constraint, "MonophylyConstraint")) {
            constraint_taxa <- c(constraint$fixed_otus,
                                 constraint$floating_otus)
          } else if (test_class(constraint, "BackboneConstraint")) {
            ref_tree <- self$get_dependency("reference tree")
            constraint_taxa <- ref_tree$trees %>%
                use_series("tip.label")
          }

          val_check <- check_subset(constraint_taxa, all_taxa)
          if (!test_true(val_check)) {
            cli_abort(c("{.arg constraints} contains taxa not present in the matrix.",
                        "x" = val_check))
          }
        }

        value
      }

      self$new_dependency("matrix", TRUE, validate_matrix)

      validate_topology <- function(value) {
        if (!test_null(value)) {
          val_check <- check_class(value, "ReadTreesCommand")
          if (!test_true(val_check)) {
            cli_abort(c("{.arg value} must be a {.cls ReadTreesCommand} object.",
                        "x" = val_check))
          }
        }
        value
      }

      self$new_dependency("reference tree", FALSE, validate_topology)

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Print a brief summary of the command.
    #'
    #' @param ... Not used.
    print = function(...) {
      cli_text(col_grey("# A ", style_italic(col_red("nitro")),
                        " topological constraints command"))
      options <- format(self)
      names(options) <- NULL
      print(options, row.names = FALSE)
    },
    #' @description
    #' Render the TNT `force` command string.
    #'
    #' Produces one `force` statement per constraint. Monophyly constraints
    #' render as `force +/-  [ taxa (floating) ];` and backbone constraints
    #' render as `force /: &0;`.
    #'
    #' @param ... Not used.
    #'
    #' @return A character vector with one element per constraint.
    render = function(...) {
      force_args <- character(0)

      for (constraint in self$constraints) {
        if (test_class(constraint, "MonophylyConstraint")) {
          force_arg <- paste(constraint$fixed_otus, collapse = " ")

          if (!is.null(constraint$floating_otus)) {
            force_arg <- glue(
              "{force_arg} ({floating})",
              floating = paste(constraint$floating_otus, collapse = " ")
            )
          }

          force_arg <- glue(
            "{type} [ {force_arg} ]",
            type = ifelse(constraint$is_positive, "+", "-")
          )

        } else if (test_class(constraint, "BackboneConstraint")) {
          force_arg <- ifelse(constraint$is_positive, "/", ":") %>%
            paste("&0")
        }

        force_args <- c(force_args, force_arg)
      }

      cmd <- paste(force_args, collapse = " ") %>%
        paste(self$name, " ", ., ";", sep = "")
      cmd
    }
  )
)
