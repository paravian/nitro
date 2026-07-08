#' TNT Interface
#'
#' @description
#' An [R6][R6::R6Class] class that manages a TNT subprocess and provides the
#' low-level interface for sending commands and receiving output.
#'
#' `TntInterface` is created via [create_interface()] and passed to
#' [execute_analysis()] to run a configured [TreeAnalysis]. Users do not
#' interact with the subprocess directly — command execution is handled
#' internally by `$execute()`.
#'
#' @details
#' ## Initialisation
#' When a `TntInterface` object is created, it:
#' 1. Locates and validates the TNT executable at `$path`.
#' 2. Detects the operating system platform (`"unix"` or `"windows"`).
#' 3. Starts a TNT subprocess and handles the interactive licence agreement
#'    prompt (Unix only — on Windows, the licence must be accepted manually
#'    before calling [create_interface()]).
#' 4. Extracts and stores the TNT version string.
#' 5. Kills the subprocess (a fresh one is started for each analysis via
#'    `$execute()`).
#'
#' ## Execution model
#' `$execute()` writes all queued commands to a temporary `.tnt` script
#' file and starts a new TNT subprocess with that file as its argument.
#' Output is read incrementally until the TNT prompt is detected, then
#' parsed and dispatched to each command's `$transform()` method. The
#' result is returned as a [TreeAnalysisResults] object.
#'
#' ## Platform differences
#' On Unix, TNT is run in a pseudo-terminal (PTY) to handle interactive
#' prompts. On Windows, stdout and stderr are captured via pipes. The
#' `$platform` field is set automatically from `.Platform$OS.type`.
#'
#' ## Write-once fields
#' `$path`, `$platform`, and `$version` are write-once — they are set
#' during initialisation and cannot be changed afterwards. `$process` is
#' mutable and holds the current [processx::process] object.
#'
#' @seealso
#' * [create_interface()] — recommended way to create a `TntInterface`.
#' * [execute_analysis()] — runs a [TreeAnalysis] using this interface.
#' * [TreeAnalysis] — the analysis configuration object.
#' * [TreeAnalysisResults] — the object returned by `$execute()`.
#'
#' @keywords internal
#' @importFrom checkmate check_character check_choice check_environment check_flag check_null check_string test_list test_null test_string test_true
#' @importFrom cli cli_abort cli_alert_danger cli_alert_info cli_alert_success cli_alert_warning cli_text col_grey col_red style_italic
#' @importFrom glue glue
#' @importFrom magrittr %>% extract extract2
#' @importFrom processx process
#' @importFrom R6 R6Class
#' @importFrom stringr str_detect str_extract str_match str_replace_all str_split_1 str_trim str_wrap
#' @importFrom utils head tail
TntInterface <- R6Class(
  "TntInterface",
  private = list(
    newline = NULL,
    prompts = NULL,
    .path = NULL,
    .platform = NULL,
    .process = NULL,
    .version = NULL,
    #' @description
    #' Strip and trim raw TNT output into a clean character vector.
    #'
    #' @param value \[`character`\]\cr
    #'   Raw output from the TNT subprocess.
    #'
    #' @return A character vector of trimmed, non-empty lines.
    clean = function(value) {
      val_check <- check_character(value, min.len = 1, any.missing = FALSE)
      if (!isTRUE(val_check)) {
        cli_abort(c("A character vector must be supplied.",
                    "x" = val_check))
      }

      cleaned <- str_split_1(value, private$newline) %>%
        str_trim() %>%
        .[nchar(.) > 0]
      return(cleaned)
    },
    #' @description
    #' Start a TNT subprocess.
    #'
    #' @param path \[`character(1)`\]\cr
    #'   Path to the TNT executable.
    #' @param argument \[`character(1)` or `NULL`\]\cr
    #'   Optional argument to pass to the executable (e.g., a script path).
    process_start = function(path, argument = NULL) {
      val_check <- check_string(path, min.chars = 1)
      if (!test_true(val_check)) {
        cli_abort(c("{.arg path} must be a string."),
                  "x" = val_check)
      }

      if (!file.exists(path)) {
        cli_abort(c("Could not find path to TNT executable."))
      }

      coll <- makeAssertCollection()
      assert(
        check_null(argument),
        check_string(argument, min.chars = 1),
        combine = "or",
        add = coll
      )

      if (!coll$isEmpty()) {
        val_check <- coll$getMessages()
        cli_abort(c("{.arg argument} must be either {.val NULL} or a valid string.",
                    "x" = val_check))
      }

      proc_args <- list(
        command = path,
        stdin = NULL,
        stdout = "|",
        stderr = "|",
        env = c("current", TERM = "xterm", HOME = Sys.getenv("HOME"))
      )

      if (!test_null(argument)) {
        proc_args$args <- argument
      }

      if (self$platform == "unix") {
        proc_args$pty <- TRUE
        proc_args$stdout <- NULL
        proc_args$stderr <- NULL
      }

      self$process <- do.call(process$new, proc_args)

      invisible(TRUE)
    }
  ),
  active = list(
    #' @field path \[`character(1)`\]\cr
    #'   *(Write-once.)* The absolute path to the TNT executable.
    path = function(value) {
      label <- "path"
      if (missing(value)) {
        return(private$.path)
      } else {
        if (test_null(private$.path)) {
          val_check <- check_string(value)

          if (!test_true(val_check)) {
            cli_abort(c("{.arg {label}} must be a string.",
                        "x" = val_check))
          }

          private$.path <- value
        } else {
          cli_abort(c("{.arg {label}} is a read-only attribute."))
        }
      }
    },
    #' @field platform \[`character(1)`\]\cr
    #'   *(Write-once.)* The operating system platform. One of `"unix"` or
    #'   `"windows"`. Set automatically from `.Platform$OS.type`.
    platform = function(value) {
      label <- "platform"
      if (missing(value)) {
        return(private$.platform)
      } else {
        if (test_null(private$.platform)) {
          val_check <- check_choice(value, c("unix", "windows"))

          if (!test_true(val_check)) {
            cli_abort(c("{.arg {label}} must be a valid option.",
                        "x" = val_check))
          }
          private$.platform <- value
        } else {
          cli_abort(c("{.arg {label}} is a read-only attribute."))
        }
      }
    },
    #' @field process \[`process`\]\cr
    #'   The current [processx::process] object representing the TNT
    #'   subprocess. Replaced each time `$execute()` is called.
    process = function(value) {
      label <- "process"
      if (missing(value)) {
        return(private$.process)
      } else {
        val_check <- check_class(value, "process")
        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be an object of class {.cls process}.",
                      "x" = val_check))
        }

        private$.process <- value
      }
    },
    #' @field version \[`character(1)`\]\cr
    #'   *(Write-once.)* The version string of the TNT executable, in the
    #'   form `"{number} ({month} {year})"`. Set automatically during
    #'   initialisation.
    version = function(value) {
      label <- "version"
      if (missing(value)) {
        return(private$.version)
      } else {
        if (test_null(private$.version)) {
          val_check <- check_string(value, min.chars = 1)

          if (!test_true(val_check)) {
            cli_abort(c("{.arg {label}} must be a string.",
                        "x" = val_check))
          }

          private$.version <- value
        } else {
          cli_abort(c("{.arg {label}} is a read-only attribute."))
        }
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `TntInterface` object.
    #'
    #' Locates the TNT executable, handles the licence agreement prompt,
    #' extracts the version string, and prepares the interface for use.
    #' On Windows, the licence must be accepted manually before calling
    #' this constructor. Use [create_interface()] rather than calling this
    #' directly.
    #'
    #' @param path \[`character(1)`\]\cr
    #'   Path to the TNT executable.
    #'
    #' @return A new `TntInterface` object.
    initialize = function(path) {
      val_check <- check_string(path)
      if (!test_true(val_check)) {
        cli_abort(c("{.arg path} must be a string.",
                    "x" = val_check))
      }

      self$path <- normalizePath(path)
      self$platform <- .Platform$OS.type
      if (self$platform == "unix") {
        private$newline <- "\n"
      } else {
        private$newline <- "\r\n"
      }

      private$process_start(self$path)

      private$prompts <- list(
        yesno = "(press 'y' to accept, 'n' to decline)",
        agree = "Do you agree to all the terms and conditions?",
        done = "tnt\\*>|(enter \"help;\" for help)",
        cmd = "COMMAND: (?<cmdname>[A-Z]+)",
        err = "\a+(.+)"
      )

      ready <- FALSE
      pul_warned <- FALSE
      proc_read <- TRUE
      buffer <- character()
      while (!ready) {
        if (proc_read) {
          proc_poll <- self$process$poll_io(5000)

          conn_id <- names(proc_poll)[proc_poll == "ready"]

          proc_out <- character(2)
          if ("output" %in% conn_id) {
            proc_out[1] <- self$process$read_output()
          }
          if ("error" %in% conn_id) {
            proc_out[2] <- self$process$read_error()
          }
          buffer <- paste(buffer, proc_out, sep = "", collapse = "")

          if (length(proc_out) == 0) {
            next
          }

          prompt_match <- sapply(private$prompts[1:3], str_detect, string = buffer)

          if (!any(prompt_match)) {
            next
          }

          prompt_match <- names(private$prompts[1:3])[prompt_match]
        }

        buffer <- private$clean(buffer)

        if ("yesno" %in% prompt_match) {
          if (!pul_warned) {
            cli_alert_warning("You must agree to TNT's licence before using {.pkg nitro}.")
            if (self$platform == "windows") {
              cli_abort(c("nitro can't interactively agree to TNT's licence terms on Windows.",
                          "i" = "Please open {.val {path}} manually and agree to the licence terms, then run {.arg tnt_attach} again."))
            }
            cli_alert_info("Please read and follow the instructions below.")
            pul_warned <- TRUE
          }

          if (proc_read) {
            cli_text(buffer)
          }

          response <- readline(prompt = "Please enter [y]es or [n]o: ")
          valid_response <- check_choice(response, c("y", "n"))
          if (!test_true(valid_response)) {
            cli_alert_danger("You must enter either [y]es or [n]o.")
            proc_read <- FALSE
            next
          }
          self$process$write_input(response)
        } else if ("agree" %in% prompt_match) {
          if (proc_read) {
            cli_text(buffer)
          }

          response <- readline(prompt = "Please enter \"I agree\": ")
          valid_response <- check_choice(response, c("I agree"))

          if (!test_true(valid_response)) {
            cli_alert_danger("You must enter \"I agree\".")
            proc_read <- FALSE
            next
          }
          self$process$write_input(glue("{response}"))
          cli_alert_info("Please wait while TNT starts...")
        } else if ("done" %in% prompt_match) {
          ready <- TRUE
          break
        }

        buffer <- character()
        proc_read <- TRUE
      }

      if (self$platform == "windows") {
        version_re <- "Version\\s+(?<number>[0-9]+\\.[0-9]+)\\s+(?<date>[A-Za-z]+ [0-9]{4})"
      } else {
        version_re <- "Version (?<number>[0-9]+\\.[0-9]+) - [0-9]+ bits - \\((?<date>[A-Za-z]+ [0-9]{4})\\)"
      }

      tnt_info <- str_extract(buffer, version_re) %>%
        na.omit() %>%
        str_match(version_re) %>%
        extract(1, -1) %>%
        as.list()

      self$version <- glue("{tnt_info$number} ({tnt_info$date})")

      cli_alert_info("Found TNT version {self$version}")

      self$process$kill()

      cli_alert_success("{.pkg nitro} is ready for use!")
      invisible(TRUE)
    },
    #' @description
    #' Execute a [CommandQueue] and return the results.
    #'
    #' Writes all commands to a temporary TNT script file, starts a fresh
    #' TNT subprocess, reads output incrementally until the TNT prompt is
    #' detected, dispatches output to each command's `$transform()` method,
    #' and returns a [TreeAnalysisResults] object.
    #'
    #' @param queue \[`CommandQueue`\]\cr
    #'   A [CommandQueue] object produced by [execute_analysis()].
    #'
    #' @return A [TreeAnalysisResults] object.
    execute = function(queue) {
      cli_alert_info("Starting tree analysis...")

      all_cmds <- character(0)

      write_proc <- function (out) {
        tnt_tempfile <- tempfile(pattern = "nitro-", fileext = ".tnt")
        writeLines(out, tnt_tempfile)
        glue("proc {tnt_tempfile};")
      }

      queue_cmds <- queue$commands()

      for (cmd in queue_cmds) {
        if (!test_null(cmd$optional)) {
          resolve_command(cmd, queue_cmds, "optional")
        }
        cmd_render <- cmd$render()

        if (cmd$inline) {
          cmd_render <- write_proc(cmd_render)
        }

        all_cmds <- c(
          all_cmds,
          cmd_render
        )
      }

      proc_arg <- write_proc(all_cmds)

      private$process_start(self$path, proc_arg)

      raw_output <- character(0)

      done <- FALSE
      while (!done) {
        proc_poll <- self$process$poll_io(5000)

        conn_id <- names(proc_poll)[proc_poll == "ready"]

        if (length(conn_id) > 0) {
          proc_out <- character(2)
          if ("output" %in% conn_id) {
            proc_out[1] <- self$process$read_output()
          }
          if ("error" %in% conn_id) {
            proc_out[2] <- self$process$read_error()
          }

          proc_out <- paste(proc_out, collapse = "")

          err_check <- str_detect(proc_out, private$prompts$err)
          if (any(err_check)) {
            self$process$kill()
            tnt_err <- str_match(proc_out[which(err_check)], private$prompts$err) %>%
              extract(2)
            cli_abort(c("A TNT error occurred, cannot continue.",
                        "x" = tnt_err))
          }

          raw_output <- c(raw_output, proc_out)

          done_check <- str_detect(proc_out, private$prompts$done)
          if (any(done_check)) {
            break
          }
        }
      }

      cli_alert_success("Tree analysis complete.")

      raw_output <- paste(raw_output, collapse = "")
      cmd_names <- str_match_all(raw_output, private$prompts$cmd) %>%
        extract2(1) %>%
        extract(, 2) %>%
        str_to_lower()

      cmd_out <- str_split_1(raw_output, private$prompts$cmd) %>%
        tail(-1)

      if (length(cmd_names) != length(cmd_out)) {
        cli_abort(c("Length mismatch between command names and output, this shouldn't happen"))
      }

      names(cmd_out) <- cmd_names
      cmd_out <- cmd_out[cmd_names != "procedure"]
      cmd_names <- names(cmd_out)

      output <- list()

      cmd_idx <- 1
      for (cmd_name in unique(cmd_names)) {
        while (TRUE) {
          cmd <- queue_cmds[[cmd_idx]]

          if (cmd$name == cmd_name) {
            cmd_match <- cmd_out[cmd$name == cmd_names] %>%
              paste(collapse = "")
            output_type <- str_replace(cmd$outputs, " ", "_")
            cmd_res <- cmd$transform(cmd_match)

            if (output_type %in% names(output)) {
              output[[output_type]] <- c(
                output[[output_type]],
                cmd_res
              )
            } else {
              output[[output_type]] <- cmd_res
            }

            break
          }
          cmd_idx <- cmd_idx + 1
        }
      }

      res <- do.call(TreeAnalysisResults$new, output)
      res
    },
    #' @description
    #' Format the interface as a summary table.
    #'
    #' @param ... Not used.
    #'
    #' @return A `data.frame` showing the path, platform, and version.
    format = function(...) {
      options <- data.frame(
        c(
          "Path:",
          "Platform:",
          "Version:"
        ),
        c(
          self$path,
          self$platform,
          self$version
        )
      )
      names(options) <- c("", "Current value")
      options
    },
    #' @description
    #' Print a brief summary of the interface.
    #'
    #' @param ... Not used.
    print = function(...) {
      cli_text(col_grey("# A ", style_italic(col_red("nitro")), " TNT interface object"))

      options <- format(self)
      names(options) <- NULL

      print(options, row.names = FALSE)
    },
    #' @description
    #' Send a single command to an active TNT subprocess and return its
    #' output.
    #'
    #' This is a lower-level method used for interactive subprocess
    #' communication. In normal use, commands are sent in batch via
    #' `$execute()`.
    #'
    #' @param command \[`BasicCommand`\]\cr
    #'   An object inheriting from [BasicCommand].
    #'
    #' @return A character vector of output lines from TNT.
    send_command = function(command) {
      all_commands <- command$render()
      all_commands <- sapply(all_commands, str_wrap, width = 255,
                             whitespace_only = FALSE)

      all_output <- character(0)

      for (cmd_line in all_commands) {
        self$process$write_input(paste(cmd_line, private$newline, sep = ""))

        proc_poll <- self$process$poll_io(5000)

        pipe_ready <- proc_poll == "ready"
        if (any(pipe_ready)) {
          for (pipe_idx in which(pipe_ready)) {
            pipe_type <- names(proc_poll)[pipe_idx]

            if (pipe_type == "output") {
              cmd_output <- self$process$read_output()
            }

            cmd_output <- str_split_1(cmd_output, private$newline) %>%
              str_trim()
            all_output <- c(all_output, cmd_output)
          }
        }

        Sys.sleep(0.05)
      }

      all_output
    }
  )
)
