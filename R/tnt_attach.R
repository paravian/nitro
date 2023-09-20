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

  check_tnt_info <- list(
    platform = .Platform$OS.type,
    path = normalizePath(path)
  )
  check_tnt_info$parser <- OutputParser$new(check_tnt_info$platform)

  # Prepare TNT for interactive use
  assign("tnt_info", check_tnt_info, envir = .envir)
  tnt_process <- tnt_process_start(.envir)
  startup <- tnt_read(tnt_process, .envir)

  ready <- FALSE
  pul_warned <- FALSE
  proc_read <- TRUE
  while (!ready) {
    if (startup$eos_type == "licence") {
      if (!pul_warned) {
        cli_alert_warning("You must agree to TNT's licence before using {.pkg nitro}.")
        cli_alert_info("Please read and follow the instructions below.")
        pul_warned <- TRUE
      }

      if (proc_read) {
        cli_text(paste(startup$value, collapse = "\n"))
      }
      response <- readline(prompt = "Please enter [y]es or [n]o: ")
      valid_response <- check_choice(response, c("y", "n"))
      if (!test_true(valid_response)) {
        cli_alert_danger("You must enter either [y]es or [n]o.")
        proc_read <- FALSE
        next
      }
      tnt_process$write(response)
    } else if (startup$eos_type == "agree") {
      if (proc_read) {
        cli_text(paste(startup$value, collapse = "\n"))
      }
      response <- readline(prompt = "Please enter \"I agree\": ")
      valid_response <- check_choice(response, c("I agree"))
      if (!test_true(valid_response)) {
        cli_alert_danger("You must enter \"I agree\".")
        proc_read <- FALSE
        next
      }
      tnt_process$write(glue("{response}"))
      cli_alert_info("Please wait while TNT starts...")
    } else if (startup$eos_type == "tnt_prompt") {
      ready <- TRUE
      break
    }

    proc_read <- TRUE

    if (proc_read) {
      startup <- tnt_process$read()
      if (is.null(startup)) {
        next
      }
    }
  }

  version_re <- "T. N. T.  Version (?<number>[0-9]+\\.[0-9]+) - (?<bits>[0-9]+) bits - \\((?<date>[A-Za-z]+ [0-9]{4})\\)"
  tnt_info <- str_extract(startup$value, version_re) %>%
    na.omit() %>%
    str_match(version_re) %>%
    extract(1,-1) %>%
    as.list()
  tnt_info <- c(check_tnt_info, tnt_info)
  cli_alert_info("Found TNT version {tnt_info$number} ({tnt_info$date}, {tnt_info$bits} bit)")

  assign("tnt_info", tnt_info, envir = .envir)
  cli_alert_success("TNT executable verified and registered.")
  invisible(tnt_process$kill())
}
