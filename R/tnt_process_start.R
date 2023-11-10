#' Start TNT process
#'
#' @description
#' \code{tnt_process_start} starts an instance of TNT.
#' @param .envir The environment that TNT has been attached to.
#' @param queue An optional \code{CommandQueue} object.
#' @importFrom checkmate check_environment check_string test_list test_string
#'   test_true
#' @importFrom cli cli_abort
#' @importFrom processx process
#' @importFrom stringr str_extract str_match
tnt_process_start <- function (.envir, queue = NULL) {
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

  proc_args <- list(
    command = tnt_info$path,
    stdin = NULL,
    stdout = "|",
    stderr = "|",
    env = c("current", TERM = "xterm", HOME = Sys.getenv("HOME"))
  )
  
  if (!test_null(queue)) {
    tnt_tempfile <- tempfile(pattern = "nitro-", fileext = ".tnt")
    all_cmds <- as.character(queue)
    writeLines(all_cmds, tnt_tempfile)
    proc_args$args <- glue("proc {tnt_tempfile};")
  } else {
    if (tnt_info$platform == "unix") {
      proc_args$pty <- TRUE
      proc_args$stdout <- NULL
      proc_args$stderr <- NULL
    }
  }
  
  tnt_process <- do.call(process$new, proc_args)

  tnt_info$process <- tnt_process
  assign("tnt_info", tnt_info, envir = .envir)
  return(invisible(TRUE))
}
