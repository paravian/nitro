#' Write a TNT-compatible nexus file
#'
#' Function to write a nexus file from a \code{"\linkS4class{NitroTreeSearch}"}
#' object, containing a phylogenetic matrix and TNT tree search commands
#' required as required for an analysis in \code{nitro}. This is used
#' internally by \code{\link{tnt}}.
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
#' \item A \code{"\linkS4class{NitroTrees}"} object is returned which contains
#'   a \code{multiPhylo} object in the \code{trees} slot and the original
#'   \code{"\linkS4class{NitroTreeSearch}"} object in the \code{tree_search}
#'   slot.
#' }
#' @param obj an object that inherits \code{"\linkS4class{NitroTreeSearch}"}.
#' @param filename a character vector indicating the name for the new file.
#' @param read_trees a logical value indicating whether to read in trees from
#'   the \code{start_trees} slot of \code{obj} prior to analysis.
#' @param character_fits a logical value indicating whether to return scores
#'   for individual characters and minimum and maximum possible character
#'   lengths required for calculation of character fit indices.
#' @rdname writeTNTNexus
#' @export
writeTNTNexus <- function (obj, filename, read_trees = FALSE,
                           character_fits = FALSE) {
  # Read matrix; convert NAs to "?" and write to minimal nexus file
  tnt_matrix <- obj@matrix
  tnt_matrix[is.na(tnt_matrix)] <- "?"
  write.nexus.data(tnt_matrix, file=filename, interleaved = FALSE,
                   format = "standard")


  tnt_block <- c("BEGIN TNT;", "log stdout;", "tables =;")

  if (inherits(obj@weights, "NitroImpliedWeights")) {
    iw_opts <- tnt_cmd(obj@weights)
    if (obj@weights@multi_k) {
      tnt_block <- c(tnt_block, iw_opts[2])
    }
  }

  if (inherits(obj@method, c("NitroRatchet", "NitroResampleBase"))) {
    if (!read_trees) {
      read_trees <- TRUE
    }
    if (inherits(obj@method, "NitroResampleBase")) {
      obj@start_trees <- c(obj@method@phy)
    }
  }

  tnt_block <- c(tnt_block,
                 paste0("collapse ", collapse(obj), ";"),
                 paste0("hold ", as.integer(obj@hold), ";"),
                 paste0("outgroup ", obj@outgroup - 1, ";")
  )

  char_codes <- c()
  if (any(obj@ordered_characters)) {
    char_codes <- c(char_codes, "+",
                    which(obj@ordered_characters) - 1)
  }
  if (any(obj@inactive_characters)) {
    char_codes <- c(char_codes, "]",
                    which(obj@inactive_characters) - 1)
  }
  if (length(char_codes)) {
    tnt_block <- c(tnt_block, paste(c("ccode", char_codes, ";"), collapse=" "))
  }
  if (any(obj@inactive_taxa)) {
    tnt_block <- c(tnt_block,
                   paste(c("taxcode -",
                           which(obj@inactive_taxa) - 1, ";"), collapse=" "))
  }

  if (length(obj@constraints)) {
    tnt_block <- c(tnt_block, tnt_cmd(obj@constraints))
  }

  if (read_trees) {
    if (!length(obj@start_trees)) {
      if (inherits(obj@method, "NitroRatchet")) {
        tnt_block <- c(tnt_block, "mult= wagner replic 10;")
      } else {
        stop("start_trees contains no trees")
      }
    }
    res <- new("NitroTrees", tree_search = obj, trees = obj@start_trees)
    tnt_block <- c(tnt_block, tnt_cmd(res))
  }

  if (inherits(obj@method, "NitroResampleBase")) {
    rstree_file <- sub("(\\..+|$)", ".tre", filename)
    tnt_block <- c(tnt_block, "ttags =;", tnt_cmd(obj@method), "ttags );",
                   paste("tsave *", rstree_file, sep = ""), "save *;",
                   "tsave /;")
  } else {
    tnt_block <- c(tnt_block, tnt_cmd(obj@method), "condense;")
    if (obj@combine) {
      res <- new("NitroTrees", tree_search = obj, trees = obj@start_trees)
      tnt_block <- c(tnt_block, tnt_cmd(res), "unique;")
    }
  }

  tnt_block <- c(tnt_block, "tplot *;", "length;", "minmax;")

  if (character_fits) {
    tnt_block <- c(tnt_block, "cscores;")
  }

  if (inherits(obj, "NitroImpliedWeights")) {
    tnt_block <- c(tnt_block, "score;")
  }

  tnt_block <- c(tnt_block, "END;")

  write(tnt_block, file=filename, append=TRUE)
}
