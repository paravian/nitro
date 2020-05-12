#' @importFrom methods callNextMethod
setMethod("initialize", "NitroEqualWeights",
          function (.Object, matrix, tree_search, ordered_characters,
                    inactive_taxa, inactive_characters, collapse, outgroup) {
  .Object <- callNextMethod(.Object, matrix = matrix, tree_search = tree_search,
    ordered_characters = ordered_characters, inactive_taxa = inactive_taxa,
    inactive_characters = inactive_characters, collapse = collapse,
    outgroup = outgroup)
  validObject(.Object)
  .Object
})

setMethod("tnt_cmd", "NitroEqualWeights", function (n) {
  tnt_cmd(n@tree_search)
})
