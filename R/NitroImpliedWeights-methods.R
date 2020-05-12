#' @importFrom methods callNextMethod
setMethod("initialize", "NitroImpliedWeights",
          function (.Object, matrix, tree_search, ordered_characters,
                    inactive_taxa, inactive_characters, collapse, outgroup, k,
                    multi_k) {
  .Object@k <- k
  .Object@multi_k <- multi_k
  .Object <- callNextMethod(.Object, matrix = matrix, tree_search = tree_search,
    ordered_characters = ordered_characters, inactive_taxa = inactive_taxa,
    inactive_characters = inactive_characters, collapse = collapse,
    outgroup = outgroup)
  validObject(.Object)
  .Object
})

setMethod("tnt_cmd", "NitroImpliedWeights", function (n) {
  tnt_cmd(n@tree_search)
})
