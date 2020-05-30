#' Executes TNT commands
#'
#' @importFrom ape write.nexus.data
#' @importFrom processx run
#' @importFrom progress progress_bar
#' @importFrom utils tail
#' @description Executes commands on a supplied phylogenetic matrix in the TNT
#'   command line binary.
#' @param obj an object that inherits from \code{NitroBase} (i.e.,
#'   \code{IterativeEnum}, \code{BranchSwap}, \code{Ratchet} or \code{Driven}.
#' @param tnt_path the location of the TNT command line binary.
#' @param hold an integer indicating the number of trees to hold in TNTs tree
#'   buffer.
#' @param max_ram a numeric indicating the number of (binary) megabytes to
#'   allocate for use by TNT.
#' @return An object containing the TNT parameters and the \code{phyDat} matrix
#'   used in the analysis, and a \code{multiPhylo} containing the trees foundas()
#'   during the search.
#' @export
tnt <- function (obj, tnt_path, hold, max_ram = 16) {
  if (!inherits(obj, "NitroBase")) {
    stop("'obj' must inherit class NitroBase")
  }
  if (!is.numeric(hold)) {
    stop("'hold' must be of class numeric")
  } else if (hold < 0 | hold %% 1 != 0) {
    stop("'hold must be an integer greater than zero")
  }
  # Read matrix; convert NAs to "?" and write to temporary minimal nexus file
  tnt_tempfile <- tempfile("nitro", fileext = ".tnt")
  tnt_matrix <- obj@matrix
  tnt_matrix[is.na(tnt_matrix)] <- "?"
  write.nexus.data(tnt_matrix, file=tnt_tempfile, interleaved = FALSE,
                   format = "standard")

  # Prepare command line arguments for the TNT binary
  platform <- .Platform$OS.type
  tnt_arg <- c(paste0("mxram", max_ram / 1000 * 1024 ),
               paste0("proc ", tnt_tempfile,
                      ifelse(platform == "windows", ":", ",")))

  tnt_block <- c("BEGIN TNT;", "log stdout;", "tables =;")

    }
  }

  tnt_block <- c(tnt_block,
    paste0("collapse ", collapse(obj), ";"),
    paste0("hold ", as.integer(hold), ";"),
    paste0("outgroup ", obj@outgroup - 1, ";")
  )

  char_codes <- c()
  if (any(ordered_characters(obj))) {
    char_codes <- c(char_codes, "+",
                    which(ordered_characters(obj)) - 1)
  }
  if (any(inactive_characters(obj))) {
    char_codes <- c(char_codes, "]",
                    which(inactive_characters(obj)) - 1)
  }
  if (length(char_codes)) {
    tnt_block <- c(tnt_block, paste(c("ccode", char_codes, ";"), collapse=" "))
  }

  if (any(inactive_taxa(obj))) {
    tnt_block <- c(tnt_block,
                   paste(c("taxcode -",
                           which(inactive_taxa(obj)) - 1, ";"), collapse=" "))
  }

  tnt_block <- c(tnt_block, tnt_cmd(obj), "condense;", "tplot *;", "length;",
                 "minmax;")

  if (inherits(obj, "NitroImpliedWeights")) {
    tnt_block <- c(tnt_block, "score;")
  }

  tnt_block <- c(tnt_block, "END;")

  write(tnt_block, file=tnt_tempfile, append=TRUE)

  # Initialise progress bar
  pb_setup <- list()
  pb_setup$re <- "\\r([0-9]+) +[A-Z]+ +(?:[0-9]+ of )*([0-9]+) +(?:[0-9\\.]+|-+) +([0-9\\.]+|-+) +[0-9:]+ +[0-9,]+"
  pb_setup$score <- 3L
  pb_setup$ratio_num <- 1L
  pb_setup$hits <- 1L
  if (class(obj@tree_search) == "NitroBranchSwap") {
    pb_setup$total <- replications(obj@tree_search)
    pb_setup$format <- "Branch swapping: [:bar] :current/:total reps | Best score: :score"
  } else if (class(obj@tree_search) == "NitroRatchet") {
    pb_setup$total <- iterations(obj@tree_search)
    pb_setup$format <- "Ratcheting: [:bar] :current/:total iters | Best score: :score"
  } else if (class(obj@tree_search) == "NitroDriven") {
    pb_setup$hits <- hits(obj@tree_search)
    pb_setup$ratio_num <- 2L
    pb_setup$total <- replications(obj@tree_search)
    pb_setup$format <- paste0("Driven search: [:bar]", ifelse(hits(obj@tree_search) > 1, " run :run", ""), " | :current/:total reps | Best score: :score")
  } else if (class(obj@tree_search) == "NitroImplicitEnum") {
    pb_setup$re <- "Searching \\(score ([0-9]+)\\) ([X=]+)\r"
    pb_setup$score <- 1L
    pb_setup$total <- 30L
    pb_setup$format <- "Implicit enumeration: [:bar] | Best score :score"
  }

  pb <- progress_bar$new(format = pb_setup$format, total = pb_setup$total)
  pb$tick(0)

  # Define callback function for process line-by-line TNT output
  callback <- function (out, proc) {
    prog_re <- regexec(pb_setup$re, out)
    if(length(attr(prog_re[[1]], "match.length")) != 1) {
      do_update = TRUE
      prog_m <- tail(regmatches(out, prog_re)[[1]], -1)
      score <- prog_m[pb_setup$score]
      tnt_prog_re <- regexec("(X*)=+", prog_m[pb_setup$ratio_num])[[1]]
      if (attr(tnt_prog_re, "match.length")[1] > -1) {
        ratio_num <- attr(tnt_prog_re, "match.length")[2]
      } else {
        ratio_num <- as.numeric(prog_m[pb_setup$ratio_num])
      }
      ratio <- ratio_num / pb_setup$total
      if (ratio > 1) {
        do_update <- FALSE
      }
      tokens <- list(score = score)
      if (pb_setup$hits > 1) {
        tokens$run <- as.numeric(prog_m[1]) + 1
      }
      if (do_update & !pb$finished) {
        pb$update(ratio, tokens = tokens)
      }
    }
    return(out)
  }

  # Run TNT binary and collect output
  tnt_output <- run(normalizePath(tnt_path), tnt_arg,
                    stderr_callback = callback)

  # Terminate progress bar and split TNT stdout
  if (!pb$finished) {
    pb$update(1)
    pb$terminate
  }

  err_out <- strsplit(tnt_output$stderr, "[\n\r]+")[[1]]

  # Check if tree buffer filled during analysis
  buf_re <- sapply(regmatches(err_out, regexec("(?:tree buffer full|Increase max.trees)", err_out)),
                   length)
  if (any(buf_re)) {
    stop("Tree buffer filled during analysis; consider increasing 'hold' value")
  }

  # Check if insufficient RAM for tree buffer size
  ram_re <- sapply(regmatches(err_out, regexec("Cannot make tree space", err_out)), length)
  if (any(ram_re)) {
    stop(paste("Tree buffer cannot hold", as.integer(hold),
               "trees; consider increasing 'max_ram' value"))
  }

  # Check for overflowed replications in branch swap analyses
  overflow_re <- sapply(regmatches(err_out, regexec("some replications overflowed", err_out)),
                        length)
  if (any(overflow_re)) {
    message("Warning: Some replications overflowed")
  }

  # Check for other error messages and display if found
  err_re <- sapply(regmatches(err_out, regexec("^Error reading ", err_out)),
                   length)
  if (any(err_re)) {
    stop(sub("^\a", "", err_out[which(err_re > 0)[1] - 1]))
  }

  tnt_output <- strsplit(tnt_output$stdout, "[\n\r]+")[[1]]
  trees <- tnt_output_parse(tnt_output, rownames(tnt_matrix))
  return(trees)
}
