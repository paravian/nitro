#' Abstract Character Matrix Base Class
#'
#' @description
#' `AbstractCharacterMatrix` is the base [R6][R6::R6Class] class for all
#' character–taxon matrix objects in \pkg{nitro}. It defines the common
#' interface shared by [DiscreteMatrix] and [ContinuousMatrix]: taxon names,
#' character count, and character activity.
#'
#' This class is not exported. It is intended to be subclassed by
#' [DiscreteMatrix] and [ContinuousMatrix], not instantiated directly. Use
#' [create_matrix()] to create matrix objects.
#'
#' @details
#' ## Subclassing
#' Subclasses should populate `private$.data`, `private$.taxa`,
#' `private$.n_characters`, and `private$.is_inactive` in their own
#' `$data` active binding setter. The `$inactive` binding defined here
#' operates on `private$.is_inactive` and is available to all subclasses
#' without further implementation.
#'
#' ## Active bindings
#' | Field | Type | Mutable | Description |
#' |-------|------|---------|-------------|
#' | `taxa` | `character` | Read-only | Names of the taxa in the matrix. |
#' | `n_characters` | `integer(1)` | Read-only | Total number of characters. |
#' | `inactive` | `numeric` or `NULL` | Yes | Indices of inactive characters. |
#'
#' @seealso
#' * [DiscreteMatrix] — subclass for discrete character data.
#' * [ContinuousMatrix] — subclass for continuous character data.
#' * [create_matrix()] — recommended way to create matrix objects.
#'
#' @keywords internal
#' @importFrom checkmate assert check_numeric check_null makeAssertCollection test_null test_true
#' @importFrom cli cli_abort
#' @importFrom dplyr across everything group_by mutate where
#' @importFrom R6 R6Class
AbstractCharacterMatrix <- R6Class(
  "AbstractCharacterMatrix",
  private = list(
    .data = NULL,
    .data_type = NULL,
    .is_inactive = NULL,
    .n_characters = NULL,
    .taxa = NULL
  ),
  active = list(
    #' @field data_type \[`character(1)`\]\cr
    #'   *(Read-only.)* The type of character data in the matrix.
    data_type = function(value) {
      if (missing(value)) {
        return(private$.data_type)
      } else {
        cli_abort(c("{.arg data_type} is a read-only attribute."))
      }
    },
    #' @field taxa \[`character`\]\cr
    #'   *(Read-only.)* The names of the taxa contained in the matrix.
    taxa = function(value) {
      if (missing(value)) {
        return(private$.taxa)
      } else {
        cli_abort(c("{.arg taxa} is a read-only attribute."))
      }
    },
    #' @field n_characters \[`integer(1)`\]\cr
    #'   *(Read-only.)* The number of characters contained in the matrix.
    n_characters = function(value) {
      if (missing(value)) {
        return(private$.n_characters)
      } else {
        cli_abort(c("{.arg n_characters} is a read-only attribute."))
      }
    },
    #' @field inactive \[`numeric` or `NULL`\]\cr
    #'   The indices of characters to mark as inactive (i.e., excluded from
    #'   analysis). Must be unique integer indices between 1 and
    #'   `$n_characters`. Set to `NULL` to activate all characters.
    #'   Returns `NULL` when no characters are inactive.
    inactive = function(value) {
      if (missing(value)) {
        if (any(private$.is_inactive)) {
          return(which(private$.is_inactive))
        }
        return(NULL)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_numeric(value, min.len = 1, lower = 1, upper = self$n_characters,
                        unique = TRUE, any.missing = FALSE),
          add = coll
        )
        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg inactive} must contain valid unique character indices.",
                      "x" = val_check))
        }
        is_inactive <- rep(FALSE, self$n_characters)
        if (!test_null(value)) {
          is_inactive[value] <- TRUE
        }
        private$.is_inactive <- is_inactive
      }
    }
  )
)

#' Concatenate Character Matrix Objects
#'
#' @description
#' Combine two or more character matrix objects into a `MultiCharacterMatrix`
#' list. This is the standard way to supply multiple matrices to
#' [create_matrix()] or [TreeAnalysis].
#'
#' @param ... Two or more objects inheriting from [AbstractCharacterMatrix]
#'   (i.e., [DiscreteMatrix] or [ContinuousMatrix] objects).
#'
#' @return A `MultiCharacterMatrix` object (a named list with class
#'   `c("MultiCharacterMatrix", "list")`).
#'
#' @seealso
#' * [DiscreteMatrix], [ContinuousMatrix] — the matrix classes that can be
#'   combined.
#' * [TreeAnalysis] — accepts `MultiCharacterMatrix` objects via its
#'   `$data` field.
#'
#' @examples
#' \dontrun{
#' combined <- c(discrete_matrix, continuous_matrix)
#' ta <- TreeAnalysis$new(data = combined)
#' }
#'
#' @importFrom checkmate check_list test_true
#' @importFrom cli cli_abort
#' @export
c.AbstractCharacterMatrix <- function(...) {
  objs <- list(...)
  val_check <- check_list(objs, types = "AbstractCharacterMatrix")
  if (!test_true(val_check)) {
    cli_abort(c("All objects must inherit from class {.cls AbstractCharacterMatrix}."),
              "x" = val_check)
  }
  class(objs) <- c("MultiCharacterMatrix", "list")
  objs
}

#' Print a MultiCharacterMatrix Object
#'
#' @param x A `MultiCharacterMatrix` object.
#' @param ... Ignored.
#'
#' @importFrom cli cli_text col_grey col_red style_italic
#' @importFrom glue glue
#' @importFrom stringr str_remove str_to_lower str_replace
#' @export
print.MultiCharacterMatrix <- function(x, ...) {
  cli_text(col_grey("# Multiple ", style_italic(col_red("nitro")), " character matrices"))

  which_mtx <- sapply(x, function(x) class(x)[1]) %>%
    table()
  names(which_mtx) <- names(which_mtx) %>%
    str_to_lower() %>%
    str_replace("(matrix)", " \\1")
  which_mtx <- glue("* {which_mtx} {names(which_mtx)}") %>%
    paste(collapse = "\n")

  cat("\n", which_mtx, sep = "")
}
