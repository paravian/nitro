#' Execute CommandQueue
#'
#' @importFrom checkmate check_class check_environment test_true
#' @importFrom cli cli_abort cli_progress_bar cli_progress_done
#'   cli_progress_update pb_bar pb_eta_str pb_name pb_percent
#' @importFrom stringr str_replace str_replace_all str_split str_trim str_wrap
#' @importFrom utils head tail
#' @param tree_analysis A \code{"\link{TreeAnalysis}"} object.
#' @param .envir The environment that TNT has been attached to.
execute_queue <- function (tree_analysis, .envir) {
  val_check <- check_class(tree_analysis, c("TreeAnalysis", "R6"))
  if (!test_true(val_check)) {
    cli_abort(c("{.arg tree_analysis} must be a {.arg TreeAnalysis} object.",
                "x" = val_check))
  }

  val_check <- check_environment(.envir)
  if (!test_true(val_check)) {
    cli_abort(c("{.arg .envir} must be an environment.",
                "x" = val_check))
  }
  
  target <- list()
  if (test_class(tree_analysis$method, "DrivenSearchOptions")) {
    progress_re <- "(?<round>[0-9]+) +[A-Z]+ +(?<count>[0-9]+)"
    target$rounds <- tree_analysis$method$hits
  } else if (test_class(tree_analysis$method, "ResampleBaseOptions")) {
    progress_re <- "[A-Za-z]+ \\(rep\\. (?<count>[0-9]+) of [0-9]+\\)"
    target$count <- tree_analysis$method$replications
  } else {
    progress_re <- "(?<count>[0-9]+) +[A-Z]+ +[0-9]+ of [0-9]+"
    if (test_class(tree_analysis$method, "RatchetOptions")) {
      target$count <- tree_analysis$method$iterations
    } else {
      target$count <- tree_analysis$method$replications
    }
  }
  
  pb_envir <- parent.frame()
  cli_progress_bar(
    name = "Progress",
    total = 100,
    .envir = pb_envir
  )

  cli_alert_info("Starting tree analysis...")
  
  tnt_process_start(.envir, tree_analysis$queue())
  
  tnt_info <- get("tnt_info", .envir)
  tnt_process <- tnt_info$process
  
  read_next <- TRUE
  cmd_name <- "start"
  buffer <- character()
  cmd_re <- "COMMAND: (?<cmdname>[A-Z]+)"
  
  raw_out <- c()
  
  done <- FALSE
  while (tnt_process$is_alive() | !done) {
    proc_poll <- tnt_process$poll_io(5000)
    
    if (proc_poll[2] == "ready") {
      proc_out <- tnt_process$read_error()
      
      # print(proc_out)
      
      has_cmd <- str_detect(proc_out, cmd_re)
      has_counter <- str_detect(proc_out, progress_re)
      
      if (has_cmd) {
        # Merge the buffer with the process output; split by command and write
        # last command back to the buffer
        proc_out <- paste(buffer, proc_out, sep = "")
        cmd_out <- str_split_1(proc_out, cmd_re)
        
        raw_out <- c(raw_out, head(cmd_out, -1))
        buffer <- tail(cmd_out, 1)
        
        buff_size <- nchar(str_trim(buffer))
        if (!tnt_process$is_alive() & buff_size == 0) {
          done <- TRUE
        }
      } else if (has_counter) {
        progress <- str_match(proc_out, progress_re) %>%
          extract(1,) %>%
          extract(-1) %>%
          as.list() %>%
          lapply(as.numeric)
        
        if ("round" %in% names(progress)) {
          denominator <- with(target, round * count)
          numerator <- progress$round * target$count + (progress$count - 1)
          if ((numerator / denominator) > 1) {
            numerator <- (progress$round - 1) * target$count + (progress$count - 1)
          }
        } else {
          denominator <- target$count
          numerator <- progress$count
        }
        percent <- numerator / denominator * 100
        cli_progress_update(set = percent, .envir = pb_envir)
      } else {
        buffer <- paste(buffer, proc_out, sep = "")
      }
    }
  }

  cli_progress_done(.envir = pb_envir)
  cli_alert_success("Tree analysis complete.")
 
  output_type <- sapply(raw_out, tnt_info$parser$content_detect)
  
  output <- list()
  not_text <- output_type != "text"
  if (any(not_text)) {
    raw_out <- raw_out[not_text]
    output_type <- output_type[not_text]
    for (n in seq(sum(not_text))) {
      obj <- tnt_info$parser$transform(raw_out[[n]], output_type[n])
      output[[output_type[n]]] <- obj
    }
  }

  return(output)
}
