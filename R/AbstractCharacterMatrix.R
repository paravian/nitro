#' Abstract character matrix
#'
#' \code{AbstractCharacterMatrix} is an R6 class that defines properties common
#'   to any type of character matrix. Intended to be inherited, not instantiated
#'   directly.
#' @importFrom checkmate assert check_numeric check_null makeAssertCollection
#'   test_null test_true
#' @importFrom cli cli_abort
#' @importFrom dplyr across everything group_by mutate where
#' @importFrom R6 R6Class
AbstractCharacterMatrix <- R6Class("AbstractCharacterMatrix",
  private = list(
    .data = NULL,
    .is_inactive = NULL,
    .n_characters = NULL,
    .taxa = NULL
  ),
  active = list(
    #' @field data The character matrix.
    data = function (value) {
      if (missing(value)) {
        return(private$.data)
      } else {
        cli_abort(c("{.arg data} is a read-only attribute."))
      }
    },
    #' @field taxa The names of the taxa contained in the matrix.
    taxa = function (value) {
      if (missing(value)) {
        return(private$.taxa)
      } else {
        cli_abort(c("{.arg taxa} is a read-only attribute."))
      }
    },
    #' @field n_characters The number of the characters contained in the matrix.
    n_characters = function (value) {
      if (missing(value)) {
        return(private$.n_characters)
      } else {
        cli_abort(c("{.arg n_characters} is a read-only attribute."))
      }
    },
    #' @field inactive A numeric vector indicating which characters to mark as inactive.
    inactive = function (value) {
      if (missing(value)) {
        if (any(private$.is_inactive)) {
          return(which(private$.is_inactive))
        }
        return(NULL)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_numeric(value, min.len = 1, lower = 1, upper = self$n_characters, unique = TRUE, any.missing = FALSE),
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

#' Concatenate character matrices
#'
#' @description
#' Concatenate character matrix objects into a list.
#'
#' @param ... Character matrix objects.
#' @importFrom checkmate check_list test_true
#' @importFrom cli cli_abort
#' @export
c.AbstractCharacterMatrix <- function (...) {
  objs <- list(...)
  val_check <- check_list(objs, types = "AbstractCharacterMatrix")
  if (!test_true(val_check)) {
    cli_abort(c("All objects must inherit from class {.cls AbstractCharacterMatrix}."),
              "x" = val_check)
  }
  class(objs) <- c("MultiCharacterMatrix", "list")
  return(objs)
}
