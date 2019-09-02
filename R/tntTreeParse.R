#' Extracts trees from TNT output.
#'
#' @importFrom magrittr %>%
#' @importFrom ape read.tree .compressTipLabel
#' @param tnt.output TNT output.
#' @param tip.labels A list or vector containing the names of the tips.
#' @return A \code{multiphylo} object.
tntTreeParse <- function (tnt.output, tip.labels) {
  # Get start lines for tree strings and lengths output
  first.trees <- grep("Tread 'set of [0-9]+ trees' ", tnt.output) + 1
  tree.lengths <- grep("Tree lengths ", tnt.output)

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

  # Get all tree lengths
  lengths <- tnt.output[(tree.lengths+3):(minsteps.match-3)] %>%
    .[c(TRUE, rep(FALSE, 2))] %>%
    gsub("^ +", "", .) %>%
    strsplit(" +") %>%
    unlist

  # Create phylo objects from newick trees; calculate statistics
  trees <- apply(cbind(newick.trees, lengths), 1, function (tree.data) {
    tree <- read.tree(text=tree.data[1])
    tree$tip.label <- tip.labels[as.numeric(tree$tip.label) + 1]
    tree$score <- as.numeric(tree.data[2])
    tree$CI <- minsteps / tree$score
    tree$RI <- (maxsteps - tree$score) / (maxsteps - minsteps)
    tree$RC <- tree$CI * tree$RI
    return(tree)
  })
  return(.compressTipLabel(trees))
}
