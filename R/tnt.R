#' Execute a TNT search
#'
#' This function generates the commands required to conduct a tree search
#' using the matrix and parameters specified within a
#' \code{"\linkS4class{NitroTreeSearch}"} object.
#'
#' @details The function goes through the following stages, in order:
#'
#' \enumerate{
#' \item A temporary TNT-compatible nexus file is created from the input
#'   matrix.
#' \item A TNT block is appended to this file containing a script that will
#'   execute in order each step required to perform the phylogenetic analysis
#'   and return the set of most parsimonious trees generated.
#'
#'   The script generation step itself involves the following steps (not all of
#'   which will necessarily be present depending on the specifications of the
#'   given analysis):
#'
#'   \enumerate{
#'   \item set implied weighting constants and parameters;
#'   \item set zero-length branch collapse rule;
#'   \item set maximum number of trees to hold in TNT's buffer;
#'   \item set the outgroup taxon;
#'   \item defining ordered and inactive characters;
#'   \item defining inactive taxa;
#'   \item defining and setting constraints on monophyly;
#'   \item reading in starting trees;
#'   \item setting parameters for and executing tree searches;
#'   \item condensing trees according to current collapse rule;
#'   \item retrieving trees as character strings; and
#'   \item retrieving tree lengths/scores and minimum-maximum scores.
#'   }
#' \item This file will then be executed by a subprocess of TNT created using
#'   \code{processx}, and the output generated from the \code{stdout} stream
#'   of this process is captured.
#' \item Tree strings and metrics are parsed from the output stream and
#'   converted into a \code{multiPhylo} object. During this step, the ensemble
#'   consistency, retention and rescaled consistency indices (CI, RI, RC) are
#'   calculated from the output. These values, together with the tree
#'   lengths/scores, are saved with each \code{phylo} tree.
#' \item The \code{multiPhylo} object is saved in the
#'   \code{"\linkS4class{NitroResults}"} slot of the original
#'   \code{"\linkS4class{NitroTreeSearch}"} object, which is then
#'   returned to the user.
#' }
#' @importFrom ape write.nexus.data
#' @importFrom processx run
#' @importFrom progress progress_bar
#' @importFrom utils tail
#' @description Executes commands on a supplied phylogenetic matrix in the TNT
#'   command line binary.
#' @param obj an object that inherits \code{"\linkS4class{NitroTreeSearch}"}.
#' @param tnt_path the location of the TNT command line binary.
#' @param hold an integer indicating the number of trees to hold in TNTs tree
#'   buffer.
#' @param max_ram a numeric indicating the number of (binary) megabytes to
#'   allocate for use by TNT.
#' @param read_trees a logical value indicating whether to read in trees from
#'   a \code{"\linkS4class{NitroResults}"} object prior to analysis.
#' @return the original \code{"\linkS4class{NitroTreeSearch}"} object containing a
#'   \code{"\linkS4class{NitroResults}"} object containing the trees
#'   found during the search.
#' @export
tnt <- function (obj, tnt_path, hold, max_ram = 16, read_trees = FALSE) {
  if (!inherits(obj, "NitroTreeSearch")) {
    stop("'obj' must inherit class NitroTreeSearch")
  }
  if (!is.numeric(hold)) {
    stop("'hold' must be of class numeric")
  } else if (hold < 0 | hold %% 1 != 0) {
    stop("'hold must be an integer greater than zero")
  }

  # Remove inactive taxa and characters from matrix, if required
  tnt_matrix <- obj@matrix
  ord_chars <- c()
  if (any(obj@inactive_characters)) {
    if (any(obj@ordered_characters)) {
      ord_chars <- which(obj@ordered_characters[!obj@inactive_characters])
    }
    tnt_matrix <- tnt_matrix[,!obj@inactive_characters]
  }
  if (any(obj@inactive_taxa)) {
    tnt_matrix <- tnt_matrix[!obj@inactive_taxa,]
  }

  # Read matrix; convert NAs to "?" and write to temporary minimal nexus file
  tnt_tempfile <- tempfile("nitro", fileext = ".tnt")
  tnt_matrix[is.na(tnt_matrix)] <- "?"
  write.nexus.data(tnt_matrix, file=tnt_tempfile, interleaved = FALSE,
                   format = "standard")

  # Prepare command line arguments for the TNT binary
  tnt_arg <- c(paste("mxram", max_ram / 1000 * 1024),
               paste("proc", tnt_tempfile))

  tnt_block <- c("BEGIN TNT;", "log stdout;", "tables =;")

  if (inherits(obj@weights, "NitroImpliedWeights")) {
    iw_opts <- tnt_cmd(obj@weights)
    tnt_arg <- c(iw_opts[1], tnt_arg)
    if (obj@weights@multi_k) {
      tnt_block <- c(tnt_block, iw_opts[2])
    }
  }

  if (inherits(obj@method, "NitroResampleBase")) {
    obj@results <- new("NitroResults", trees = c(obj@method@phy))
    read_trees <- TRUE
  }

  tnt_block <- c(tnt_block,
    paste0("collapse ", collapse(obj), ";"),
    paste0("hold ", as.integer(hold), ";"),
    paste0("outgroup ", obj@outgroup - 1, ";")
  )

  if (length(ord_chars) > 0) {
    tnt_block <- c(tnt_block,
                   paste(c("ccode +", ord_chars - 1, ";"),
                         collapse = " "))
  }

  if (length(obj@constraints)) {
    tnt_block <- c(tnt_block, tnt_cmd(obj@constraints))
  }

  if (read_trees) {
    tnt_block <- c(tnt_block, tnt_cmd(obj@results, tnt_matrix))
  }

  tnt_block <- c(tnt_block, tnt_cmd(obj@method), "condense;", "tplot *;", "length;",
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
  if (class(obj@method) == "NitroBranchSwap") {
    pb_setup$total <- replications(obj@method)
    pb_setup$format <- "Branch swapping: [:bar] :current/:total reps | Best score: :score"
  } else if (class(obj@method) == "NitroRatchet") {
    pb_setup$total <- iterations(obj@method)
    pb_setup$format <- "Ratcheting: [:bar] :current/:total iters | Best score: :score"
  } else if (class(obj@method) == "NitroDriven") {
    pb_setup$hits <- hits(obj@method)
    pb_setup$ratio_num <- 2L
    pb_setup$total <- replications(obj@method)
    pb_setup$format <- paste0("Driven search: [:bar]", ifelse(hits(obj@method) > 1, " run :run", ""), " | :current/:total reps | Best score: :score")
  } else if (class(obj@method) == "NitroImplicitEnum") {
    pb_setup$re <- "Searching \\(score ([0-9]+)\\) ([X=]+)\r"
    pb_setup$score <- 1L
    pb_setup$total <- 30L
    pb_setup$format <- "Implicit enumeration: [:bar] | Best score :score"
  } else if (class(obj@method) == "NitroBranchBreak") {
    pb_setup$re <- "\\r-+ +[A-Z]+ +([0-9]+) of ([0-9]+) +(?:[0-9\\.]+|-+) +([0-9\\.]+|-+) +[0-9:]+ +[0-9,]+"
    pb_setup$format <- "Branch breaking | Trees found: :current | Best score: :score"
    pb_setup$total <- .Machine$integer.max
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
  obj@results <- new("NitroResults", trees = trees)
  return(obj)
}
