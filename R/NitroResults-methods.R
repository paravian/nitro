# setMethod("show", "NitroResults", function (object) {
#   show(object@trees)
# })

#' @importFrom TreeTools as.Newick
setMethod("tnt_cmd", "NitroResults", function (n, matrix) {
  mtx_tips <- rownames(matrix)

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

setGeneric("trees", function (n) standardGeneric("trees"))

setMethod("trees", "NitroResults", function (n) n@trees)

setGeneric("trees<-", function (n, phy) standardGeneric("trees<-"))

#' @importFrom methods validObject
setMethod("trees<-", "NitroResults", function (n, phy) {
  n@trees <- phy
  validObject(n)
  n
})
