#' Start TNT process
#'
#' @description
#' \code{tnt_process_start} starts an instance of TNT.
#' @param .envir The environment that TNT has been attached to.
#' @importFrom checkmate check_environment check_string test_list test_string
#'   test_true
#' @importFrom cli cli_abort
#' @importFrom processx process
#' @importFrom stringr str_extract str_match
tnt_process_start <- function (.envir) {
  val_check <- check_environment(.envir)
  if (!test_true(val_check)) {
    cli_abort(c("{.arg .envir} must be an environment.",
                "x" = val_check))
  }

  tnt_info <- try(get("tnt_info", .envir), silent = TRUE)
  if (!test_list(tnt_info)) {
    cli_abort(c("Could not find attached TNT process in {format(.envir)}"))
  }

  if (!test_string(tnt_info$path)) {
    cli_abort(c("Could not find path to TNT process."))
  }

  if (tnt_info$platform == "unix") {
    tnt_process <- process$new(
      tnt_info$path,
      pty = TRUE,
      env = c(COLUMNS = "10000", TERM = "xterm", HOME = Sys.getenv("HOME"))
    )
  } else {
    cli_abort("Can't run subprocesses on non UNIX-like platforms.",
              "x" = "{.pkg nitro} does not currently support running on non UNIX-like platforms (i.e., Windows).")
  }

  return(tnt_process)
}
