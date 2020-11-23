#' Execute a TNT search
#'
#' This function uses \code{\link{write_tnt_nexus}} to generate a nexus file
#' containing the commands required to conduct a tree search using the matrix
#' and parameters specified within a \code{"\link{NitroTreeSearch}"}
#' object.
#' @importFrom ape .compressTipLabel
#' @importFrom magrittr %>%
#' @importFrom processx run
#' @importFrom progress progress_bar
#' @importFrom TreeTools TNTText2Tree
#' @importFrom utils tail
#' @description Executes commands on a supplied phylogenetic matrix in the TNT
#'   command line binary.
#' @param obj An object that inherits \code{"\link{NitroTreeSearch}"}.
#' @param tnt_path The location of the TNT command line binary.
#' @param read_trees A logical value indicating whether to read in trees from
#'   the \code{start_trees} slot of \code{obj} prior to analysis.
#' @param character_fits A logical value indicating whether to return scores
#'   for individual characters and minimum and maximum possible character
#'   lengths required for calculation of character fit indices.
#' @return A \code{"\link{NitroTrees}"} object containing the original
#'   \code{"\link{NitroTreeSearch}"} object and a \code{multiPhylo} of
#'   the trees found during the search.
#' @export
tnt <- function (obj, tnt_path, read_trees = FALSE, character_fits = FALSE) {
  if (!inherits(obj, "NitroTreeSearch")) {
    stop("'obj' must inherit class NitroTreeSearch")
  }

  temp_filename <- tempfile("nitro", fileext = ".tnt")
  write_tnt_nexus(obj, temp_filename, read_trees, character_fits)

  # Prepare command line arguments for the TNT binary
  tnt_arg <- c(paste("mxram", obj$max_ram / 1000 * 1024),
               paste("proc", temp_filename))

  if (inherits(obj$weights, "NitroImpliedWeights")) {
    iw_opts <- obj$weights$tnt_cmd()
    tnt_arg <- c(iw_opts[1], tnt_arg)
  }

  # Initialise progress bar
  pb_setup <- list()
  pb_setup$re <- "([0-9]+) +[A-Z]+ +(?:[0-9]+ of )*([0-9]+) +(?:[0-9\\.]+|-+) +([0-9\\.]+|-+) +[0-9:]+ +[0-9,]+"
  pb_setup$score <- 3L
  pb_setup$ratio_num <- 1L
  pb_setup$hits <- 1L
  if (inherits(obj$method, "NitroBranchSwap")) {
    pb_setup$total <- obj$method$replications
    pb_setup$format <- "Branch swapping: [:bar] :current/:total reps | Best score: :score"
  } else if (inherits(obj$method, "NitroRatchet")) {
    pb_setup$total <- obj$method$iterations
    pb_setup$format <- "Ratcheting: [:bar] :current/:total iters | Best score: :score"
  } else if (inherits(obj$method, "NitroDriven")) {
    pb_setup$hits <- obj$method$hits
    pb_setup$ratio_num <- 2L
    pb_setup$total <- obj$method$replications
    pb_setup$format <- paste0("Driven search: [:bar]", ifelse(obj$method$hits > 1, " run :run |", ""), " :current/:total reps | Best score: :score")
  } else if (inherits(obj$method, "NitroImplicitEnum")) {
    pb_setup$re <- "Searching \\(score ([0-9]+)\\) ([X=]+)\r"
    pb_setup$score <- 1L
    pb_setup$total <- 30L
    pb_setup$format <- "Implicit enumeration: [:bar] | Best score :score"
  } else if (inherits(obj$method, "NitroBranchBreak")) {
    pb_setup$re <- "-+ +[A-Z]+ +([0-9]+) of ([0-9]+) +(?:[0-9\\.]+|-+) +([0-9\\.]+|-+) +[0-9:]+ +[0-9,]+"
    pb_setup$format <- "Branch breaking | Trees found: :current | Best score: :score"
    pb_setup$total <- .Machine$integer.max
  } else if (inherits(obj$method, "NitroResampleBase")) {
    pb_setup$re <- "([A-Za-z]+) \\(rep\\. ([0-9]+) of ([0-9]+)\\) X*=+"
    pb_setup$format <- "[:bar] :current/:total reps"
    pb_setup$total <- obj$method$replications
    pb_setup$ratio_num <- 2L
  }

  pb <- progress_bar$new(format = pb_setup$format, total = pb_setup$total)
  pb$tick(0)

  # Define callback function for process line-by-line TNT output
  callback <- function (out, proc) {
    out <- tail(strsplit(out, "[\n\r]+")[[1]], 1)
    prog_re <- regexec(pb_setup$re, out)
    if(attr(prog_re[[1]], "match.length")[1] != -1) {
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
    warning(paste("Tree buffer cannot hold", as.integer(obj$hold),
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

  trees <- tnt_output_parse(tnt_output, rownames(obj$matrix))

  if (inherits(obj$method, "NitroResampleBase")) {
    rstree_file <- sub("(\\..+|$)", ".tre", temp_filename)
    rstree_tnt <- readLines(rstree_file)[2] %>%
      gsub("=(/ )*", "", .)
    rstree_phy <- list()
    attr_names <- c("length", "weighted_score", "score", "CI", "RI", "RC",
                    "char_scores", "minsteps", "maxsteps")
    rstree_tnt <- gsub("\\[([0-9]+)\\]", "-\\1", rstree_tnt)
    rstree_phy[[1]] <- gsub("(\\)[0-9-]+)\\/[0-9-]+", "\\1", rstree_tnt)
    rstree_phy[[2]] <- gsub("\\)[0-9-]+\\/([0-9-]+)", ")\\1", rstree_tnt)

    tree_attrs <- names(attributes(trees[[1]]))
    tree_attrs <- attr_names[match(tree_attrs, attr_names, 0L)]

    rstree_phy <- lapply(rstree_phy, function (p) {
      phy <- TNTText2Tree(p)
      phy$tip.label <- trees[[1]]$tip.label
      for (tree_attr in tree_attrs) {
        attr(phy, tree_attr) <- attr(trees[[1]], tree_attr)
      }
      phy
    })
    attr(rstree_phy[[1]], "resampling_freqs") <- "absolute"
    attr(rstree_phy[[2]], "resampling.freqs") <- "group present/contradicted"

    trees <- .compressTipLabel(rstree_phy)
  }
  res <- NitroTrees$new(tree_search = obj, trees = trees)
  return(res)
}
