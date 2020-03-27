if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("."))
}

#' Extracts trees from TNT output.
#'
#' @importFrom magrittr %>%
#' @importFrom ape read.tree .compressTipLabel
#' @param tnt.output a character vector containing raw TNT output from the
#'   \code{implicit.enum}, \code{branchswap}, \code{ratchet} or \code{driven}
#'   functions.
#' @param tip.labels a list or vector containing the names of the taxa from the
#'   analysed matrix.
#' @return a \code{multiPhylo} object.
tntTreeParse <- function (tnt.output, tip.labels) {
  # Get start lines for tree strings and lengths output
  first.trees <- grep("Tread 'set of [0-9]+ trees' ", tnt.output) + 1
  tree.lengths <- grep("Tree lengths ", tnt.output)
  weighted.scores <- grep("Adjusted homoplasy ", tnt.output)

  # Convert TNT's tree strings into newick format
  newick.trees <- paste(tnt.output[first.trees:(tree.lengths - 2)],
                        collapse = "") %>%
    gsub(" [*;] +", ";*", .) %>%
    strsplit("\\*") %>%
    unlist %>%
    gsub(" ", ",", .) %>%
    gsub(",\\)", ")", .) %>%
    gsub("\\)\\(","),(", .)

  # Get total min and max steps
  minsteps.match <- grep("Minimum possible steps \\(total = ([0-9]+)\\) ",
                         tnt.output)
  maxsteps.match <- grep("Maximum possible steps \\(total = ([0-9]+)\\) ",
                         tnt.output)
  minsteps <- as.numeric(gsub(".* ([0-9]+)\\) ", "\\1",
                              tnt.output[minsteps.match]))
  maxsteps <- as.numeric(gsub(".* ([0-9]+)\\) ", "\\1",
                              tnt.output[maxsteps.match]))

  tableParse <- function (table) {
    return(table[c(TRUE, rep(FALSE, 2))] %>%
      gsub("^ +", "", .) %>%
      strsplit(" +") %>%
      unlist
    )
  }

  # Get all tree lengths
  lengths <- tableParse(tnt.output[(tree.lengths+3):(minsteps.match-3)])

  tree.attrs <- cbind(newick.trees, lengths)

  scores <- NULL
  if (length(weighted.scores)) {
    scores <- tableParse(tnt.output[(weighted.scores+3):length(tnt.output)])
    tree.attrs <- cbind(tree.attrs, scores)
  }

    # Create phylo objects from newick trees; calculate statistics
  trees <- apply(tree.attrs, 1, function (tree.data) {
    tree <- read.tree(text=tree.data[1])
    tree$tip.label <- tip.labels[as.numeric(tree$tip.label) + 1]
    tree$length <- as.numeric(tree.data[2])
    if (length(tree.data) > 2) {
      tree$weighted.score <- as.numeric(tree.data[3])
    }
    tree$CI <- minsteps / tree$length
    tree$RI <- (maxsteps - tree$length) / (maxsteps - minsteps)
    tree$RC <- tree$CI * tree$RI
    return(tree)
  })
  return(.compressTipLabel(trees))
}
