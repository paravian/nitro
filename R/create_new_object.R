#' Create a new object
#'
#' @description
#' Create a new R6 object from a set of possible options.
#' @param name The name of the object to create.
#' @param object_choice The possible options for object creation.
#' @importFrom checkmate check_choice
#' @importFrom cli cli_abort
#' @importFrom stringr str_remove_all str_replace_all str_to_lower
create_new_object <- function(name, object_choice, ...) {
  name <- str_to_lower(name)

  arg_choice <- str_to_lower(object_choice)
  idx <- pmatch(name, arg_choice)

  arg_match <- name
  if (!is.na(idx)) {
    arg_match <- arg_choice[idx]
  }

  val_check <- check_choice(arg_match, arg_choice)
  if (!test_true(val_check)) {
    val_check <- str_replace_all(val_check, "([\\{\\}])", "\\1\\1")
    cli_abort(c("{.arg name} must be a valid method name.",
                x = val_check))
  }

  object_class <- str_remove_all(object_choice[idx], "_") %>%
    paste("Options", sep = "") %>%
    get()

  search_args <- list(...)

  search_obj <- do.call(object_class$new, search_args)
  return(search_obj)
}
