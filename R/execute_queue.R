#' Execute CommandQueue
#'
#' @importFrom checkmate check_class check_environment test_true
#' @importFrom cli cli_abort cli_progress_bar cli_progress_done
#'   cli_progress_update pb_bar pb_eta_str pb_name pb_percent
#' @importFrom stringr str_replace str_replace_all str_split str_trim str_wrap
#' @param queue A \code{"\link{CommandQueue}"} object.
#' @param .envir The environment that TNT has been attached to.
execute_queue <- function (queue, .envir) {
  val_check <- check_class(queue, c("CommandQueue", "R6"))
  if (!test_true(val_check)) {
    cli_abort(c("{.arg queue} must be a {.arg CommandQueue} object.",
                "x" = val_check))
  }

  val_check <- check_environment(.envir)
  if (!test_true(val_check)) {
    cli_abort(c("{.arg .envir} must be an environment.",
                "x" = val_check))
  }

  tnt_process_start(.envir)
  startup <- tnt_read(.envir)

  tnt_info <- get("tnt_info", .envir)

  n_cmds <- queue$length()
  command <- queue$read_next()
  all_output <- c()
  res <- list()

  # bar <- cli_progress_bar("Tree search")
  cli_alert_info("Starting tree analysis...")
  envir <- parent.frame()
  cli_progress_bar(
    name = "Progress",
    total = 100,
    .envir = envir
  )

  status <- "parsing"
  repl_input <- NULL
  expected <- character()

  while (status != "done") {
    if (status == "parsing") {
      command <- queue$read_next()

      repl_input <- c(command$name, command$arguments)
      if (length(repl_input) == 2) {
        repl_input <- paste(repl_input, collapse = " ")
      }
      n_lines <- length(repl_input)

      repl_input[n_lines] <- repl_input[n_lines] %>%
        str_replace(";*$", "")
      if (command$name == "xread") {
        repl_input <- c(repl_input, ";")
      } else {
        repl_input[n_lines] <- paste(repl_input[n_lines], ";", sep = "")
      }

      repl_input <- str_wrap(repl_input, width = 254) %>%
        str_split("\n") %>%
        unlist()

      if (any(nchar(repl_input) > 254)) {
        repl_input <- str_replace_all(repl_input, "(.{254})", "\\1\n") %>%
          str_split("\n") %>%
          unlist()
      }

      expected <- c("tnt_prompt", command$name)
      status <- "writing"
    } else if (status == "writing") {
      if (length(repl_input) > 0) {
        tnt_info$process$write_input(glue("{repl_input[1]}\r"))
        # cli_text("Writing: {repl_input[1]}")
      }
      status <- "waiting"
    } else if (status == "waiting") {
      output <- tnt_read(.envir)
      # cli_text("Reading: ({output$eos_type}; {output$content_type}) {output$value}")

      if (is.null(output)) {
        cli_abort(c("No output received from TNT process.",
                    "i" = "Attempted to execute {.arg {repl_input[1]}}"))
      }

      if (output$content_type == "error") {
        tnt_error <- paste(output$value, collapse = " ") %>%
          str_split("\n") %>%
          unlist() %>%
          str_trim()
        names(tnt_error) <- rep("x", length(tnt_error))
        cli_abort(c("Encountered an error while executing {.val {command$name}}",
                    tnt_error))
      } else if (output$content_type != "text") {
        res[[output$content_type]] <- tnt_info$parser$transform(output$value, output$content_type)
      }

      if (output$eos_type %in% c("resample_progress", "search_progress")) {
        frac <- tnt_info$parser$progress(command$name, command$arguments, output)
        # cli_text("{frac}/100")
        cli_progress_update(set = frac, .envir = envir)
      } else if (output$eos_type %in% expected) {
        if (output$eos_type == "tnt_prompt") {
          if (queue$length() == 0) {
            status <- "done"
          } else {
            status <- "parsing"
          }
        } else {
          repl_input <- repl_input[-1]
          status <- "writing"
        }
      } else {
        cli_abort(c("Unexpected output received from TNT. Cannot proceed.",
                    "i" = "Output received: {.val {output$value}}"))
      }
    }
  }

  cli_progress_done(.envir = envir)
  cli_alert_success("Tree analysis complete.")
  return(res)
}
