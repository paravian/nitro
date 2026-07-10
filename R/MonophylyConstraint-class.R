#' Monophyly Constraint
#'
#' @description
#' An [R6][R6::R6Class] class that configures a monophyly constraint for
#' use in constrained tree searches in \pkg{nitro}.
#'
#' A monophyly constraint requires (positive) or forbids (negative) a
#' specified set of taxa from forming a monophyletic group in the
#' recovered trees. An optional set of floating OTUs may also be
#' specified; these are taxa that are allowed to fall inside the
#' constrained group but are not required to do so.
#'
#' @details
#' ## Fixed and floating OTUs
#' * `$fixed_otus` — taxa that must (positive) or must not (negative) form
#'   a clade. At least two taxa are required.
#' * `$floating_otus` — taxa that are permitted to fall within the
#'   constrained clade but are not required to. Must be disjoint from
#'   `$fixed_otus`. Optional.
#'
#' ## Default values
#' | Parameter       | Default |
#' |-----------------|---------|
#' | `floating_otus` | `NULL`  |
#' | `is_positive`   | `TRUE`  |
#'
#' ## Combining constraints
#' Multiple `MonophylyConstraint` objects can be combined into a
#' `MultiMonophylyConstraint` list using `c()`. This is handled
#' automatically by [TopologicalConstraintsCommand] when a list of
#' constraints is supplied.
#'
#' @seealso
#' * [BackboneConstraint] — constrains searches using a reference tree.
#' * [AbstractConstraint] — parent class defining `$is_positive`.
#' * [TopologicalConstraintsCommand] — holds and renders one or more
#'   constraints as a TNT `force` command.
#' * [set_constraint()] — recommended way to attach constraints to a
#'   [TreeAnalysis].
#'
#' @examples
#' # Positive monophyly constraint (taxa must form a clade)
#' mc <- MonophylyConstraint$new(
#'   fixed_otus = c("TaxonA", "TaxonB", "TaxonC")
#' )
#'
#' # Negative constraint (taxa must not form a clade)
#' mc_neg <- MonophylyConstraint$new(
#'   fixed_otus  = c("TaxonA", "TaxonB"),
#'   is_positive = FALSE
#' )
#'
#' # With floating OTUs
#' mc_float <- MonophylyConstraint$new(
#'   fixed_otus    = c("TaxonA", "TaxonB", "TaxonC"),
#'   floating_otus = c("TaxonD", "TaxonE")
#' )
#'
#' @importFrom checkmate assert check_character check_disjunct check_flag
#'   check_list check_null makeAssertCollection test_true
#' @importFrom cli cli_abort cli_text col_grey style_italic col_red
#' @importFrom R6 R6Class
#' @importFrom stringr str_replace_all
#' @export
MonophylyConstraint <- R6Class(
  "MonophylyConstraint",
  inherit = AbstractConstraint,
  private = list(
    .fixed_otus    = NULL,
    .floating_otus = NULL
  ),
  active = list(
    #' @field fixed_otus \[`character`\]\cr
    #'   A character vector of taxon names that must (positive constraint)
    #'   or must not (negative constraint) form a monophyletic group. Must
    #'   contain at least two unique, non-missing names. Must be disjoint
    #'   from `$floating_otus`.
    fixed_otus = function(value) {
      if (missing(value)) {
        return(private$.fixed_otus)
      } else {
        val_check <- check_character(value,
          min.chars = 1,
          any.missing = FALSE,
          min.len = 2, unique = TRUE
        )
        if (!test_true(val_check)) {
          cli_abort(c("{.arg fixed_otus} must be a valid character vector.",
            "x" = val_check
          ))
        }

        val_check <- check_disjunct(value, self$floating_otus)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg fixed_otus} must not contain taxa from {.arg floating_otus}.",
            "x" = val_check
          ))
        }

        private$.fixed_otus <- value
      }
    },
    #' @field floating_otus \[`character` or `NULL`\]\cr
    #'   An optional character vector of taxon names that are permitted to
    #'   fall within the constrained clade but are not required to. Must
    #'   contain at least one unique, non-missing name if supplied. Must be
    #'   disjoint from `$fixed_otus`. Set to `NULL` (default) to disable.
    floating_otus = function(value) {
      if (missing(value)) {
        return(private$.floating_otus)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_character(value,
            min.chars = 1, any.missing = FALSE,
            min.len = 1, unique = TRUE
          ),
          add = coll
        )

        if (!coll$isEmpty()) {
          val_check <- coll$getMessages()
          cli_abort(c("{.arg floating_otus} must be either a valid character vector or {.val NULL}.",
            "x" = val_check
          ))
        }

        val_check <- check_disjunct(value, self$fixed_otus)
        if (!test_true(val_check)) {
          val_check <- str_replace_all(val_check, "([\\{\\}])", "\\1\\1")
          cli_abort(c("{.arg floating_otus} must not contain taxa from {.arg fixed_otus}.",
            "x" = val_check
          ))
        }

        private$.floating_otus <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Format the constraint as a summary table.
    #'
    #' @param ... Not used.
    #'
    #' @return A `data.frame` with columns for description and value.
    format = function(...) {
      options <- data.frame(
        c("Constraint type:", "Fixed OTUs:"),
        c(
          ifelse(self$is_positive, "positive", "negative"),
          length(self$fixed_otus)
        )
      )

      if (length(self$floating_otus) > 0) {
        options <- rbind(options, c(
          "Floating OTUs:",
          length(self$floating_otus)
        ))
      }

      names(options) <- c("", "Value")
      options[, 1] <- format(options[, 1], justify = "left")
      options
    },
    #' @description
    #' Create a new `MonophylyConstraint` object.
    #'
    #' @param fixed_otus \[`character`\]\cr
    #'   Taxon names forming the constrained group. See the `$fixed_otus`
    #'   field.
    #' @param floating_otus \[`character` or `NULL`\]\cr
    #'   Optional taxon names permitted inside the constrained group
    #'   (default: `NULL`). See the `$floating_otus` field.
    #' @param is_positive \[`logical(1)`\]\cr
    #'   Whether the constraint is positive (default: `TRUE`). See the
    #'   `$is_positive` field.
    #'
    #' @return A new `MonophylyConstraint` object.
    initialize = function(fixed_otus, floating_otus = NULL,
                          is_positive = TRUE) {
      a <- as.list(environment(), all = TRUE)

      super$initialize(is_positive = is_positive)

      private$.fixed_otus <- character(0)
      private$.floating_otus <- character(0)

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Print a brief summary of the constraint.
    #'
    #' @param ... Not used.
    print = function(...) {
      cli_text(col_grey(
        "# A ", style_italic(col_red("nitro")),
        " monophyly constraint"
      ))
      options <- format(self)
      names(options) <- NULL
      print(options, row.names = FALSE)
    }
  )
)

#' Combine MonophylyConstraint Objects
#'
#' @description
#' Combines two or more [MonophylyConstraint] objects into a
#' `MultiMonophylyConstraint` list for use with
#' [TopologicalConstraintsCommand].
#'
#' @param ... Two or more [MonophylyConstraint] objects.
#'
#' @return A `MultiMonophylyConstraint` object (a named list with class
#'   `c("MultiMonophylyConstraint", "list")`).
#'
#' @seealso
#' * [MonophylyConstraint] — the individual constraint class.
#' * [TopologicalConstraintsCommand] — accepts a list of constraints.
#'
#' @importFrom checkmate check_list test_true
#' @importFrom cli cli_abort
#' @exportS3Method
c.MonophylyConstraint <- function(...) {
  objs <- list(...)
  val_check <- check_list(objs, types = "MonophylyConstraint")
  if (!test_true(val_check)) {
    cli_abort(c("All objects must inherit from class {.cls MonophylyConstraint}.",
      "x" = val_check
    ))
  }
  class(objs) <- c("MultiMonophylyConstraint", "list")
  objs
}

#' Print a MultiMonophylyConstraint Object
#'
#' @param x A `MonophylyConstraint` object.
#' @param ... Ignored.
#'
#' @importFrom cli cli_text col_grey col_red style_italic
#' @exportS3Method
#' @keywords internal
print.MultiMonophylyConstraint <- function(x, ...) {
  cli_text(col_grey("# Multiple ", style_italic(col_red("nitro")), " monophyly constraints"))

  out <- data.frame(length(x))
  rownames(out) <- "Number of constraints:"

  names(out) <- NULL
  print(out)
}
