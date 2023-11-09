#' Register TNT executable
#'
#' @description
#' \code{tnt_attach} attaches an instance of TNT to an R environment.
#' @param path The location of the TNT command line executable.
#' @param .envir The environment that TNT has been attached to.
#' @importFrom checkmate check_choice check_environment check_string test_true
#' @importFrom cli cli_abort cli_alert_danger cli_alert_warning cli_alert_info
#'   cli_alert_success cli_text
#' @importFrom glue glue
#' @importFrom stats na.omit
#' @importFrom stringr str_extract str_match
#' @export
tnt_attach <- function (path, .envir = parent.frame()) {
  val_check <- check_string(path)
  if (!test_true(val_check)) {
    cli_abort(c("{.arg path} must be a string.",
                "x" = val_check))
  }

  val_check <- check_environment(.envir)
  if (!test_true(val_check)) {
    cli_abort(c("{.arg .envir} must be an environment.",
                "x" = val_check))
  }

  platform <- .Platform$OS.type
  
  if ("tnt_info" %in% ls(envir = .envir)) {
    try(tnt_info$process$kill(), silent = TRUE)
    rm(tnt_info, envir = .envir)
  }
  
  check_tnt_info <- list(
    path = normalizePath(path),
    platform = platform,
    parser = OutputParser$new()
  )
  assign("tnt_info", check_tnt_info, envir = .envir)

  # Prepare TNT for interactive use
  tnt_process_start(.envir)
  check_tnt_info <- get("tnt_info", envir = .envir)
  tnt_process <- check_tnt_info$process

  prompts <- list(
    yesno = "(press 'y' to accept, 'n' to decline)",
    agree = "Do you agree to all the terms and conditions?",
    done = "tnt\\*>|(enter \"help;\" for help)"
  )
  
  
  ready <- FALSE
  pul_warned <- FALSE
  proc_read <- TRUE
  buffer <- character()
  while (!ready) {
    if (proc_read) {
      proc_poll <- tnt_process$poll_io(5000)
      
      if (proc_poll[2] != "ready") {
        next
      }
      
      proc_out <- tnt_process$read_error() %>%
        check_tnt_info$parser$clean()
      
      if (length(proc_out) == 0) {
        next
      }
      
      prompt_match <- sapply(prompts, str_detect, string = proc_out) %>%
        apply(2, any) %>%
        names(prompts)[.]
    }
    
    
    if ("yesno" %in% prompt_match) {
      if (!pul_warned) {
        cli_alert_warning("You must agree to TNT's licence before using {.pkg nitro}.")
        if (platform == "windows") {
          cli_abort(c("nitro can't interactively agree to TNT's licence terms on Windows.",
                      "i" = "Please open {.val {check_tnt_info$path}} manually and agree to the licence terms, then run {.arg tnt_attach} again."))
        }
        cli_alert_info("Please read and follow the instructions below.")
        pul_warned <- TRUE
      }
      
      if (proc_read) {
        cli_text(proc_out)
      }
      
      response <- readline(prompt = "Please enter [y]es or [n]o: ")
      valid_response <- check_choice(response, c("y", "n"))
      if (!test_true(valid_response)) {
        cli_alert_danger("You must enter either [y]es or [n]o.")
        proc_read <- FALSE
        next
      }
      tnt_process$write_input(response)
    } else if ("agree" %in% prompt_match) {
      if (proc_read) {
        cli_text(proc_out)
      }
      
      response <- readline(prompt = "Please enter \"I agree\": ")
      valid_response <- check_choice(response, c("I agree"))
      
      if (!test_true(valid_response)) {
        cli_alert_danger("You must enter \"I agree\".")
        proc_read <- FALSE
        next
      }
      tnt_process$write_input(glue("{response}"))
      cli_alert_info("Please wait while TNT starts...")
    } else if ("done" %in% prompt_match) {
      ready <- TRUE
      break
    }

    proc_read <- TRUE
  }

  if (platform == "windows") {
    version_re <- "Version\\s+(?<number>[0-9]+\\.[0-9]+)\\s+(?<date>[A-Za-z]+ [0-9]{4})"
  } else {
    version_re <- "Version (?<number>[0-9]+\\.[0-9]+) - [0-9]+ bits - \\((?<date>[A-Za-z]+ [0-9]{4})\\)"
  }
  
  tnt_info <- str_extract(proc_out, version_re) %>%
    na.omit() %>%
    str_match(version_re) %>%
    extract(1,-1) %>%
    as.list()
  tnt_info <- c(check_tnt_info, tnt_info)
  cli_alert_info("Found TNT version {tnt_info$number} ({tnt_info$date})")

  assign("tnt_info", tnt_info, envir = .envir)
  cli_alert_success("TNT executable verified and attached to {.val {format(.envir)}}.")
  invisible(tnt_process$kill())
}
