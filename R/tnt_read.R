#' Read output from TNT
#'
#' @param process A process object running TNT.
#' @param .envir The environment that TNT has been attached to.
tnt_read <- function (process, .envir) {
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

  # Check that the TNT subprocess is alive
  if (!process$is_alive()) {
    cli_abort(c("Can't read to executable process.",
                "x" = "The executable process has terminated."))
  }

  output <- NULL
  output_type <- NULL
  buffer <- NULL

  # Poll the TNT subprocess for output
  read_next <- TRUE
  # cli_text("Start reading output")
  while (process$is_alive()) {
    if (read_next) {
      proc_poll <- process$poll_io(5000)
    }

    if (proc_poll[1] == "ready") {
      proc_out <- process$read_output()
      eos_type <- tnt_info$parser$eos_detect(proc_out)
      # print(proc_out)
      buffer <- c(buffer, proc_out)
      if (is.null(eos_type)) {
        # cli_text("Reading more output")
        read_next <- FALSE
        next
      }
      # cli_text("Read complete")
    } else {
      # cli_text("Polling process for more output")
      read_next <- TRUE
      next
    }

    clean <- tnt_info$parser$clean(buffer)
    content_type <- tnt_info$parser$content_detect(clean)
    output <- list(value = clean, eos_type = eos_type, content_type = content_type)
    # buffer <- NULL
    # }
    break
  }

  return(output)
}
