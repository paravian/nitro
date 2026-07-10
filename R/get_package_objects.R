#' Get package objects
#'
#' @importFrom magrittr %>%
#' @keywords internal
get_package_objects <- function() {
  fn_name <- sys.call() %>%
    as.character()
  pkg_name <- get(fn_name) %>%
    environment() %>%
    environmentName()
  pkg_objs <- getNamespace(pkg_name) %>%
    ls()

  pkg_objs
}
