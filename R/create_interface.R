#' Create a TNT Interface
#'
#' @description
#' Create a [TntInterface] object by supplying the path to a TNT executable.
#'
#' On Unix, this function will interactively prompt you to accept TNT's
#' licence agreement if it has not been accepted previously. On Windows,
#' the licence must be accepted by opening the TNT executable manually
#' before calling this function.
#'
#' @param path \[`character(1)`\]\cr
#'   The path to the TNT executable.
#'
#' @return A [TntInterface] object, ready to be passed to
#'   [execute_analysis()].
#'
#' @seealso
#' * [execute_analysis()] — runs a [TreeAnalysis] using the interface.
#' * [TntInterface] — the interface class.
#'
#' @examples
#' \dontrun{
#' interface <- create_interface("/usr/local/bin/tnt")
#' }
#'
#' @importFrom R6 R6Class
#' @export
create_interface <- function(path) {
  tnt_interface <- TntInterface$new(path)

  tnt_interface
}
