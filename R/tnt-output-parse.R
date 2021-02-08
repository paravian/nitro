if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}

#' Extracts trees from TNT output.
#'
#' @importFrom magrittr %>%
#' @importFrom ape read.tree .compressTipLabel
#' @importFrom TreeTools TNTText2Tree
#' @param tnt_output a character vector containing raw TNT output from the
#'   \code{implicit.enum}, \code{branchswap}, \code{ratchet} or \code{driven}
#'   functions.
#' @param tip_labels a list or vector containing the names of the taxa from the
#'   analysed matrix.
#' @return a \code{multiPhylo} object.
tnt_output_parse <- function (tnt_output, tip_labels) {
  # Get start lines for tree strings and lengths output
  trees_match <- grep("Tread 'set of [0-9]+ trees' ", tnt_output) + 1
  lengths_match <- grep("Tree lengths ", tnt_output)
  scores_match <- grep("Adjusted homoplasy ", tnt_output)

  # Split TNT's tree strings
  tree_attrs <-
    data.frame(trees = paste(tnt_output[trees_match:(lengths_match - 2)],
                             collapse = "") %>%
    gsub(" [*;] +", ";*", .) %>%
    strsplit("\\*") %>%
    unlist)

  # Get total min and max steps
  minsteps_match <- grep("Minimum possible steps \\(total = ([0-9]+)\\) ",
                         tnt_output)
  maxsteps_match <- grep("Maximum possible steps \\(total = ([0-9]+)\\) ",
                         tnt_output)
  cscores_match <- grep(paste("Tree [0-9]+, total length [0-9]+ "),
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
  tree_attrs$lengths <- as.numeric(tableParse(
    tnt_output[(lengths_match+3):(minsteps_match-3)]))

  if (length(cscores_match)) {
    minsteps <- as.numeric(tableParse(
      tnt_output[(minsteps_match+3):(maxsteps_match-3)]))

    maxsteps <- as.numeric(tableParse(
      tnt_output[(maxsteps_match+3):(cscores_match[1]-3)]))

    tree_attrs$cscores <- lapply(1:length(cscores_match), function (t) {
      if (t < length(cscores_match)) {
        return(as.numeric(tableParse(
          tnt_output[(cscores_match[t]+3):(cscores_match[t+1]-3)])))
      }
      return(as.numeric(tableParse(
          tnt_output[(cscores_match[t]+3):length(tnt_output)])))
    })
  }

  scores <- NULL
  if (length(scores_match)) {
    tree_attrs$scores <- as.numeric(tableParse(
      tnt_output[(scores_match+3):length(tnt_output)]))
  }

  # Create phylo objects from newick trees; calculate statistics
  sum_minsteps <- sum(minsteps)
  sum_maxsteps <- sum(maxsteps)
  trees <- lapply(1:nrow(tree_attrs), function (tree_row) {
    tree_data <- tree_attrs[tree_row,]
    tree <- TNTText2Tree(tree_data["trees"])
    tree$tip.label <- tip_labels[as.numeric(tree$tip.label) + 1]
    attr(tree, "length") <- tree_data[["lengths"]]
    if (length(scores_match)) {
      attr(tree, "weighted_score") <- as.numeric(tree_data[["scores"]])
    }
    attr(tree, "CI") <- sum_minsteps / tree_data[["lengths"]]
    attr(tree, "RI") <- (sum_maxsteps - tree_data[["lengths"]]) /
      (sum_maxsteps - sum_minsteps)
    attr(tree, "RC") <- attr(tree, "CI") * attr(tree, "RI")
    if (length(cscores_match)) {
      attr(tree, "char_scores") <- tree_data["cscores"]
      attr(tree, "minsteps") <- minsteps
      attr(tree, "maxsteps") <- maxsteps
    }
    return(tree)
  })
  return(.compressTipLabel(trees))
}
