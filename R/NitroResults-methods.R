# setMethod("show", "NitroResults", function (object) {
#   show(object@trees)
# })

#' @importFrom TreeTools as.Newick
#' @rdname tnt_cmd
#' @export
setMethod("tnt_cmd", "NitroTrees", function (n) {
  mtx_tips <- rownames(n@tree_search@matrix)

  tnt_trees <- sapply(n@trees, function (phy) {
    res_tips <- phy$tip.label
    tips_match <- match(res_tips, mtx_tips)
    res_parens <- strsplit(as.Newick(phy), "[0-9]+")[[1]]
    rbind(c("", as.character(tips_match - 1)), res_parens) %>%
      as.vector %>%
      paste(collapse = "") %>%
      gsub("([0-9]+)", "\\1 ", .) %>%
      gsub("[,;]", "", .) %>%
      gsub("\\) \\(", "\\)\\(", .)
  })
  tnt_trees <- paste(tnt_trees, "*")
  tnt_trees[length(tnt_trees)] <- sub("\\*$", ";", tnt_trees[length(tnt_trees)])
  return(c("tread", tnt_trees))
})

#' Get trees
#'
#' A function to return the trees from a \code{"\linkS4class{NitroTrees}"}
#' object.
#' @param n a \code{NitroTrees} object
#' @rdname trees
#' @export
setGeneric("trees", function (n) standardGeneric("trees"))

#' @rdname trees
setMethod("trees", "NitroTrees", function (n) n@trees)
