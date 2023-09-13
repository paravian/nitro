#' TNT executable interface object
#'
#' @description
#' \code{TNTProcess} is an R6 class that starts and manages an instance of TNT.
#' @importFrom checkmate check_choice
#' @importFrom cli cli_abort cli_alert_danger cli_alert_info cli_alert_success
#'   cli_alert_warning cli_dl cli_text col_green col_grey cli_progress_bar
#'   cli_progress_done cli_progress_update col_red pb_name pb_bar pb_percent
#'   pb_eta_str
#' @importFrom glue glue
#' @importFrom magrittr %>% extract
#' @importFrom processx process
#' @importFrom R6 R6Class
#' @importFrom stringr str_extract str_match str_pad str_remove_all
#'   str_replace_all str_split str_trim
#' @export
TNTProcess <- R6Class("TNTProcess",
  private = list(
    parser = NULL,
    process = NULL,
    read = function (...) {
      # Check that the TNT subprocess is alive
      if (!private$process$is_alive()) {
        cli_abort(c("Can't read to executable process.",
                    "x" = "The executable process has terminated."))
      }

      output_type <- NULL
      buffer <- NULL

      output <- NULL

      # Poll the TNT subprocess for output
      read_next <- TRUE
      # cli_text("Start reading output")
      while (private$process$is_alive()) {
        if (read_next) {
          proc_poll <- private$process$poll_io(5000)
        }

        if (proc_poll[1] == "ready") {
          proc_out <- private$process$read_output()
          eos_type <- private$parser$eos_detect(proc_out)
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

        clean <- private$parser$clean(buffer)
        content_type <- private$parser$content_detect(clean)
        output <- list(value = clean, eos_type = eos_type, content_type = content_type)
        # buffer <- NULL
        # }
        break
      }

      return(output)
    },
    start_process = function (path) {
      if (self$platform == "unix") {
        tnt_proc <- process$new(path, pty = TRUE,
                                env = c(COLUMNS = "10000", TERM = "xterm",
                                        HOME = Sys.getenv("HOME")))
      } else (
        cli_abort("Can't run subprocesses on non UNIX-like platforms.",
                  "x" = "{.pkg} does not currently support running on non UNIX-like platforms (i.e., Windows).")
      )
      private$process <- tnt_proc
      startup <- NULL
      while (is.null(startup)) {
        startup <- private$read()
      }
      return(startup)
    },
    write = function (value) {
      if (missing(value)) {
        cli_abort(c("Must provide a character vector.",
                    "x" = "{.arg value}` is missing."))
      }
      private$process$write_input(glue("{value}\r"))
    },
    version = NULL
  ),
  public = list(
    #' @param queue A \link{"\code{CommandQueue}"} object.
    execute_queue = function (queue) {
      val_check <- check_class(queue, c("CommandQueue", "R6"))
      if (!isTRUE(val_check)) {
        cli_abort(c("{.arg queue} must be a {.arg CommandQueue} object.",
                    "x" = val_check))
      }

      private$start_process(private$version$path)

      n_cmds <- queue$length()
      command <- queue$read_next()
      all_output <- c()
      res <- list()
      write_next <- TRUE

      # bar <- cli_progress_bar("Tree search")
      # cli_progress_bar(name = "Executing command",
      #                  total = 100,
      #                  format = "{pb_name} {n_cmds - queue$length()}/{n_cmds} ({.val {command$name}}) {pb_bar} {pb_percent} | {pb_eta_str}")

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
            str_replace(";*$", ";")

          if (command$name == "xread") {
            repl_input <- str_replace_all(repl_input, "(.{254})", "\\1\n")
          } else {
            repl_input <- str_wrap(repl_input, width = 254)
          }
          repl_input <- str_split(repl_input, "\n") %>%
            as.list()

          expected <- c("tnt_prompt", command$name)
          status <- "writing"
        } else if (status == "writing") {
          if (length(repl_input) > 0) {
            private$write(repl_input[[1]])
            # cli_text("Writing: {repl_input[[1]]}")
          }
          status <- "waiting"
        } else if (status == "waiting") {
          output <- private$read()
          # cli_text("Reading: ({output$eos_type}; {output$content_type}) {output$value}")

          if (is.null(output)) {
            cli_abort(c("No output received from TNT process.",
                        "i" = "Attempted to execute {.arg {repl_input[[1]]}}"))
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
            res[[output$content_type]] <- private$parser$transform(output$value, output$content_type)
          }

          if (output$eos_type %in% c("resample_progress", "search_progress")) {
            frac <- private$parser$progress(command$name, command$arguments, output)
            print(frac)
            # cli_progress_update(set = frac$numerator, total = frac$denominator, id = bar)
            write_next <- FALSE
          } else if (output$eos_type %in% expected) {
            if (output$eos_type == "tnt_prompt") {
              if (queue$length() == 0) {
                status <- "done"
              } else {
                status <- "parsing"
              }
            } else {
              repl_input[[1]] <- NULL
              status <- "writing"
            }
          } else {
            cli_abort(c("Unexpected output received from TNT. Cannot proceed.",
                        "i" = "Output received: {.val {output$value}}"))
          }
        }
        # cli_progress_update()
      }

      # writeLines(all_output, "out-debug.txt")
      return(res)
    },
    #' @param path The location of the TNT command line executable.
    initialize = function (path) {
      self$platform <- .Platform$OS.type
      private$parser <- OutputParser$new(self$platform)
      abs_path <- normalizePath(path)

      # Prepare TNT for interactive use
      startup <- private$start_process(abs_path)

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
          if (!isTRUE(valid_response)) {
            cli_alert_danger("You must enter either [y]es or [n]o.")
            proc_read <- FALSE
            next
          }
          private$write(response)
        } else if (startup$eos_type == "agree") {
          if (proc_read) {
            cli_text(paste(startup$value, collapse = "\n"))
          }
          response <- readline(prompt = "Please enter \"I agree\": ")
          valid_response <- check_choice(response, c("I agree"))
          if (!isTRUE(valid_response)) {
            cli_alert_danger("You must enter \"I agree\".")
            proc_read <- FALSE
            next
          }
          private$write(glue("{response}"))
          cli_alert_info("Please wait while TNT starts...")
        } else if (startup$eos_type == "tnt_prompt") {
          ready <- TRUE
          break
        }

        proc_read <- TRUE

        if (proc_read) {
          startup <- private$read()
          if (is.null(startup)) {
            next
          }
        }
      }

      version_re <- "T. N. T.  Version (?<number>[0-9]+\\.[0-9]+) - (?<bits>[0-9]+) bits - \\((?<date>[A-Za-z]+ [0-9]{4})\\)"
      version_info <- str_extract(startup$value, version_re) %>%
        na.omit() %>%
        str_match(version_re) %>%
        extract(1,-1) %>%
        as.list()
      version_info$path <- abs_path
      private$version <- version_info
      cli_alert_success("TNT executable verified and ready.")
      private$process$kill()
    },
    #' @param ... Ignored.
    is_active = function (...) {
      return(private$process$is_alive())
    },
    #' @field platform The platform the TNT executable is running on.
    platform = NULL,
    #' @param ... Ignored.
    print = function (...) {
      details <- data.frame(
        Value = with(private$version, c(path, glue("{number} ({date}, {bits} bit)")))
      )
      rownames(details) <- c("Path:", "Version:")
      colnames(details) <- NULL

      cli_text("{col_grey(\"# A TNT process\")}")
      if (nrow(details) > 0) {
        print(details)
      }
    }
  )
)
