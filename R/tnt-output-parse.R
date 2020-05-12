if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}

#' Extracts trees from TNT output.
#'
#' @importFrom magrittr %>%
#' @importFrom ape read.tree .compressTipLabel
#' @param tnt_output a character vector containing raw TNT output from the
#'   \code{implicit.enum}, \code{branchswap}, \code{ratchet} or \code{driven}
#'   functions.
#' @param tip_labels a list or vector containing the names of the taxa from the
#'   analysed matrix.
#' @return a \code{multiPhylo} object.
tnt_output_parse <- function (tnt_output, tip_labels) {
  # Get start lines for tree strings and lengths output
  first_trees <- grep("Tread 'set of [0-9]+ trees' ", tnt_output) + 1
  tree_lengths <- grep("Tree lengths ", tnt_output)
  weighted_scores <- grep("Adjusted homoplasy ", tnt_output)

  # Convert TNT's tree strings into newick format
  newick_trees <- paste(tnt_output[first_trees:(tree_lengths - 2)],
                        collapse = "") %>%
    gsub(" [*;] +", ";*", .) %>%
    strsplit("\\*") %>%
    unlist %>%
    gsub(" ", ",", .) %>%
    gsub(",\\)", ")", .) %>%
    gsub("\\)\\(","),(", .)

  # Get total min and max steps
  minsteps_match <- grep("Minimum possible steps \\(total = ([0-9]+)\\) ",
                         tnt_output)
  maxsteps_match <- grep("Maximum possible steps \\(total = ([0-9]+)\\) ",
                         tnt_output)
  minsteps <- as.numeric(gsub(".* ([0-9]+)\\) ", "\\1",
                              tnt_output[minsteps_match]))
  maxsteps <- as.numeric(gsub(".* ([0-9]+)\\) ", "\\1",
                              tnt_output[maxsteps_match]))

  tableParse <- function (table) {
    return(table[c(TRUE, rep(FALSE, 2))] %>%
      gsub("^ +", "", .) %>%
      strsplit(" +") %>%
      unlist
    )
  }

  # Get all tree lengths
  lengths <- tableParse(tnt_output[(tree_lengths+3):(minsteps_match-3)])

  tree_attrs <- cbind(newick_trees, lengths)

  scores <- NULL
  if (length(weighted_scores)) {
    scores <- tableParse(tnt_output[(weighted_scores+3):length(tnt_output)])
    tree_attrs <- cbind(tree_attrs, scores)
  }

  # Create phylo objects from newick trees; calculate statistics
  trees <- apply(tree_attrs, 1, function (tree_data) {
    tree <- read.tree(text=tree_data[1])
    tree$tip.label <- tip_labels[as.numeric(tree$tip.label) + 1]
    tree$length <- as.numeric(tree_data[2])
    if (length(tree_data) > 2) {
      tree$weighted_score <- as.numeric(tree_data[3])
    }
    tree$CI <- minsteps / tree$length
    tree$RI <- (maxsteps - tree$length) / (maxsteps - minsteps)
    tree$RC <- tree$CI * tree$RI
    return(tree)
  })
  return(.compressTipLabel(trees))
}
