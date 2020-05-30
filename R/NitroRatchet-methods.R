#' Construct a parsimony ratchet analysis
#'
#' @importFrom methods new
#' @importFrom TreeTools PhyDatToMatrix
#' @param matrix an object of class \code{phyDat}.
#' @param iterations an integer value indicating the number of iterations.
#' @param replacements an integer value indicating the number of replacements
#'   (i.e., accepted tree rearrangements) to perform in each perturbation
#'   phase.
#' @param prob_up an integer value indicating the probability of upweighting a
#'   character.
#' @param prob_down an integer value indicating the probability of
#'   downweighting a character.
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
NitroRatchet <- function (matrix, iterations, replacements = 40, prob_up = 4,
                          prob_down = 4, ordered_characters = numeric(),
                          inactive_taxa = character(),
                          inactive_characters = numeric(), outgroup = NULL,
                          collapse = 3, weighting = c("equal", "implied"),
                          k = 3, multi_k = FALSE) {
  weighting <- match.arg(weighting)
  tree_search <- new("NitroRatchet", iterations = iterations,
                     replacements = replacements, prob_up = prob_up,
                     prob_down = prob_down)
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

#' @importFrom methods callNextMethod
setMethod("initialize", "NitroRatchet",
  function (.Object, iterations, replacements, prob_up, prob_down) {
    if (class(iterations) == "numeric") {
      iterations <- as.integer(iterations)
    }
    if (class(replacements) == "numeric") {
      replacements <- as.integer(replacements)
    }
    if (class(prob_up) == "numeric") {
      prob_up <- as.integer(prob_up)
    }
    if (class(prob_down) == "numeric") {
      prob_down <- as.integer(prob_down)
    }
    .Object <- callNextMethod(.Object, iterations = iterations,
                              replacements = replacements,
                              prob_up = prob_up, prob_down = prob_down)
    .Object
  })

setMethod("show", "NitroRatchet", function (object) {
  cat("Parameters for parsimony ratchet:\n\n")
  cat(paste("Iterations:                 ", object@iterations, "\n"))
  cat(paste("Replacements:               ", object@replacements, "\n"))
  cat(paste("Upweighting probability:    ", object@prob_up, "\n"))
  cat(paste("Downweighting probability:  ", object@prob_down, "\n"))
})

#' Iterations
#'
#' Function to return or set the number of iterations for a parsimony ratchet
#' phylogenetic analysis.
#' @param n an object of either \code{NitroBranchSwap} or \code{NitroDriven}.
#' @return a numeric vector indicating the number of iterations for the
#' analysis.
#' @export
#' @rdname iterations
setGeneric("iterations", function (n, value) standardGeneric("iterations"))

#' @rdname iterations
setMethod("iterations", signature("NitroRatchet", "missing"), function (n) { n@iterations })

#' @param value a numeric vector indicating the number of iterations for the
#' analysis.
#' @export
#' @rdname iterations
setGeneric("iterations<-", function (n, value) standardGeneric("iterations<-"))

.iterations_body <- function (n, value) {
  n@iterations <- as.integer(value)
  validObject(n)
  n
}

#' @rdname iterations
setMethod("iterations<-", signature("NitroRatchet", "numeric"), .iterations_body)

#' @rdname tnt_cmd
setMethod("tnt_cmd", "NitroRatchet", function (n) {
  return(c(
    "mult= wagner replic 10;",
    paste0("ratchet= iter ", n@iterations,
      " numsubs ", n@replacements,
      " upfactor ", n@prob_up,
      " downfact ", n@prob_down, ";")
  ))
})
