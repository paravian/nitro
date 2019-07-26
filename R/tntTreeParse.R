#' Extracts trees from TNT output.
#'
#' @importFrom magrittr %>%
#' @importFrom ape read.tree
#' @param tnt.output TNT output.
#' @param tip.labels A list or vector containing the names of the tips.
#' @return A \code{multiphylo} object.
tntTreeParse <- function (tnt.output, tip.labels) {
  linum <- 0
  while (linum < length(tnt.output)) {
    linum <- linum + 1
    tread.match <- regexec("Tread 'set of ([0-9]+) trees' ", tnt.output[linum])
    scores.match <- regexec("Tree lengths ", tnt.output[linum])
    minsteps.match <- regexec("Minimum possible steps \\(total = ([0-9]+)\\) ",
                              tnt.output[linum])
    maxsteps.match <- regexec("Maximum possible steps \\(total = ([0-9]+)\\) ",
                              tnt.output[linum])
    if(attr(tread.match[[1]], "match.length")[1] > -1) {
      n.mpt <- as.numeric(regmatches(tnt.output[linum],
                                     tread.match)[[1]][2])
      mpt.str <- tail(tnt.output, -linum) %>%
        head(n.mpt)
      linum <- linum + n.mpt
    }
    if(attr(minsteps.match[[1]], "match.length")[1] > -1) {
      minsteps <- as.numeric(regmatches(tnt.output[linum],
                                        minsteps.match)[[1]][2])
    }
    if(attr(maxsteps.match[[1]], "match.length")[1] > -1) {
      maxsteps <- as.numeric(regmatches(tnt.output[linum],
                                        maxsteps.match)[[1]][2])
    }
    if(attr(scores.match[[1]], "match.length")[1] > -1) {
      scores <- tail(tnt.output, -(linum + 2)) %>%
        head(ceiling(n.mpt / 10 * 3)) %>%
        .[c(TRUE, rep(FALSE, 2))] %>%
        gsub("^ +", "", .) %>%
        strsplit(" +") %>%
        unlist
    }
    if(length(grep("Error reading .*", tnt.output[linum]))) {
      stop(sub("\a", "", tnt.output[linum - 1]))
    }
  }

  trees <- apply(cbind(mpt.str, scores), 1, function (tree.data) {
    newick <- gsub(" [*;] +", ";", tree.data[1]) %>%
      gsub(" ", ",", .) %>%
      gsub(",\\)", ")", .) %>%
      gsub("\\)\\(","),(", .)
    tree <- read.tree(text=newick)
    tree$tip.label <- tip.labels[as.numeric(tree$tip.label) + 1]
    tree$score <- as.numeric(tree.data[2])
    tree$CI <- minsteps / tree$score
    tree$RI <- (maxsteps - tree$score) / (maxsteps - minsteps)
    tree$RC <- tree$CI * tree$RI
    return(tree)
  })
  class(trees) <- "multiPhylo"
  return(trees)
}
