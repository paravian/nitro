#' Construct an implicit enumeration analysis
#'
#' @importFrom methods new
#' @importFrom TreeTools PhyDatToMatrix
#' @param matrix an object of class \code{phyDat}.
#' @templateVar isgeneric FALSE
#' @template ordered_characters-template
#' @template inactive_taxa-template
#' @template inactive_characters-template
#' @template outgroup-template
#' @template collapse-template
#' @template weighting-template
#' @template k-template
#' @template multi_k-template
#' @export
NitroImplicitEnum <- function (matrix, ordered_characters = numeric(),
                          inactive_taxa = character(),
                          inactive_characters = numeric(), outgroup = NULL,
                          collapse = 3, weighting = c("equal", "implied"),
                          k = 3, multi_k = FALSE) {
  weighting <- match.arg(weighting)
  tree_search <- new("NitroImplicitEnum")
  if (weighting == "equal") {
    obj <- new("NitroEqualWeights", matrix, tree_search, ordered_characters,
               inactive_taxa, inactive_characters, collapse, outgroup)
  } else {
    obj <- new("NitroImpliedWeights", matrix, tree_search, ordered_characters,
               inactive_taxa, inactive_characters, collapse, outgroup, k,
               multi_k)
  }
  obj
}

#' @rdname tnt_cmd
setMethod("tnt_cmd", "NitroImplicitEnum", function (n) {
  return("ienum;")
})
