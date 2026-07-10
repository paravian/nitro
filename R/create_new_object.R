#' Create a New R6 Object by Name
#'
#' @description
#' Matches a user-supplied name (with partial matching) against a set of
#' available R6 class names and instantiates the corresponding object.
#'
#' This function is not exported. It is used internally by factory functions
#' such as [new_tree_search()].
#'
#' @param name \[`character(1)`\]\cr
#'   The name of the object to create. Partial unambiguous matches are
#'   accepted (case-insensitive, underscores ignored).
#' @param object_choice \[`character`\]\cr
#'   A character vector of valid R6 class names to match against.
#' @param ... Additional named arguments passed to the matched class's
#'   `$new()` method.
#'
#' @return An R6 object of the matched class.
#'
#' @keywords internal
#' @importFrom checkmate check_choice test_true
#' @importFrom cli cli_abort
#' @importFrom stringr str_remove_all str_replace_all str_to_lower
create_new_object <- function(name, object_choice, ...) {
  name <- str_to_lower(name) %>%
    str_remove_all("_")

  arg_choice <- str_to_lower(object_choice)
  idx <- pmatch(name, arg_choice)

  arg_match <- name
  if (!is.na(idx)) {
    arg_match <- arg_choice[idx]
  }

  val_check <- check_choice(arg_match, arg_choice)
  if (!test_true(val_check)) {
    val_check <- str_replace_all(val_check, "([{}])", "\\1\\1")
    cli_abort(c("{.arg name} must be a valid method name.",
      x = val_check
    ))
  }

  object_class <- get(object_choice[idx])

  search_args <- list(...)

  search_obj <- do.call(object_class$new, search_args)
  search_obj
}
