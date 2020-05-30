#' Validate Nitro S4 objects
#'
#' @param object a prospective object that inherits \code{NitroBase}.
#' @return a validated object.
check_NitroBase <- function (object) {
  if (nrow(object@matrix) < 5) {
    return("matrix must contain at least 4 taxa")
  }
  if (!inherits(object@tree_search, "NitroTreeSearch")) {
    return("tree_search must be an object of class NitroTreeSearch")
  }
  if (object@collapse < 1 | object@collapse > 4) {
    return("collapse must be an integer between 1 and 4")
  }
  if (length(object@outgroup) != 1) {
    return("outgroup must be an integer of length 1")
  }
  if (object@outgroup < 1 | object@outgroup > nrow(object@matrix)) {
    return("outgroup is not a taxon listed in matrix")
  }
  if (object@outgroup %in% rownames(object@matrix)[object@inactive_taxa]) {
    return("outgroup cannot be inactive")
  }
  if (length(object@ordered_characters) != ncol(object@matrix)) {
    return("length of ordered_characters must equal columns of matrix")
  }
  if (length(object@inactive_taxa) != nrow(object@matrix)) {
    return("length of inactive_taxa must equal rows of matrix")
  }
  if (length(object@ordered_characters) != ncol(object@matrix)) {
    return("length of inactive_characters must equal columns of matrix")
  }
  return(TRUE)
}

check_NitroBranchSwap <- function (object) {
  if (object@replications < 1) {
    return("replications must be an integer > 0")
  }
  if (object@hold_rep < 1) {
    return("hold_rep must be an integer > 0")
  }
  return(TRUE)
}

check_NitroRatchet <- function (object) {
  if (object@iterations < 1) {
    return("iterations must be an integer > 0")
  }
  if (object@replacements < 1) {
    return("replacements must be an integer > 0")
  }
  if (object@prob_up < 1) {
    return("prob_up must be an integer > 0")
  }
  if (object@prob_down < 1) {
    return("prob_down must be an integer > 0")
  }
}

check_NitroDriven <- function (object) {
  if (object@hits < 1 ) {
    return("hits must be an integer > 0")
  }
  if (object@replications < 1 ) {
    return("replications must be an integer > 0")
  }
  if (object@ratchet_cycles < 0 ) {
    return("ratchet_cycles must be zero or a positive integer")
  }
  if (object@drifting_cycles < 0 ) {
    return("drifting cycles must be zero or a positive integer")
  }
  if (object@fusing_rounds < 0 ) {
    return("fusing_rounds must be zero or a positive integer")
  }
  if (object@consense_times < 0 ) {
    return("consense_times must be zero or a positive integer")
  }
}

check_NitroImpliedWeights <- function (object) {
  if (object@k <= 0 | object@k > 1000) {
    return("k must be a number between 0 and 1000")
  }
}
