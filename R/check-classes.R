#' Validate Nitro S4 objects
#'
#' @param object a prospective object that inherits
#' \code{"\linkS4class{NitroTreeSearch}"}.
#' @return a validated object.
check_NitroTreeSearch <- function (object) {
  if (nrow(object@matrix) < 5) {
    return("matrix must contain at least 4 taxa")
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
  if (!inherits(object@constraints, "NitroConstraintsBase")) {
    return("tree_search must be an object of class NitroTreeSearch")
  }
  if (!inherits(object@method, "NitroMethodsBase")) {
    return("tree_search must be an object of class NitroTreeSearch")
  }
  if (!inherits(object@weights, "NitroWeightsBase")) {
    return("tree_search must be an object of class NitroTreeSearch")
  }
  return(TRUE)
}

#' Validate Nitro S4 objects
#'
#' @param object a prospective object that inherits \code{NitroBranchSwap}.
#' @return a validated object.
check_NitroBranchSwap <- function (object) {
  if (object@replications < 1) {
    return("replications must be an integer > 0")
  }
  if (object@hold_rep < 1) {
    return("hold_rep must be an integer > 0")
  }
  return(TRUE)
}

#' Validate Nitro S4 objects
#'
#' @param object a prospective object that inherits \code{NitroRatchet}.
#' @return a validated object.
check_NitroRatchet <- function (object) {
  if (object@iterations < 0) {
    return("iterations must be an integer >= 0")
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

check_NitroBranchBreak <- function (object) {
  if (object@swapper < 1 | object@swapper > 2) {
    return("swapper must be either 1 or 2")
  }
  if (object@cluster_size < 0) {
    return("cluster must be an integer >= 0")
  }
}

check_NitroSectorialSearch <- function (object) {
  if (object@slack < 0) {
    return("slack must be an integer >= 0")
  }
}

check_NitroRandomSectorialSearch <- function (object) {
  if (object@min_size < 5) {
    return("min_size must be an integer 5 or greater")
  }
  if (object@max_size < 5) {
    return("max_size must be an integer 5 or greater")
  }
  if (object@max_size < object@min_size) {
    return("max_size must be equal to or larger than min_size")
  }
}

check_NitroConstraintSectorialSearch <- function (object) {
  if (object@rounds < 0) {
    return("rounds must be an integer >= 0")
  }
  if (object@max_fork < object@min_fork) {
    return("max_fork must be equal to or larger than min_fork")
  }
}

check_NitroTreeFuse <- function (object) {
  if (object@rounds < 0 | object@rounds > 100) {
    return("rounds must be an integer between 0 and 100")
  }
}

check_NitroTreeHybridize <- function (object) {
  if (object@rounds < 0) {
    return("rounds must be an integer >= 0")
  }
  if (object@sample_factor < 0) {
    return("sample_factor must be an integer >= 0")
  }
}

check_NitroTreeDrift <- function (object) {
  if (object@iterations < 0) {
    return("iterations must be an integer >= 0")
  }
  if (object@substitutions < 1) {
    return("substitutions must be an integer > 0")
  }
  if (object@max_abs_fit_diff < 0) {
    return("max_abs_fit_diff must be a number > 0")
  }
  if (object@max_rel_fit_diff < 0) {
    return("max_rel_fit_diff must be a number > 0")
  }
  if (object@reject_factor < 0) {
    return("reject_factor must be a number > 0")
  }
  if (object@autoconstrain_cycles < 0) {
    return("autoconstrain_cycles must be an integer >= 0")
  }
}

#' Validate Nitro S4 objects
#'
#' @param object a prospective object that inherits \code{NitroDriven}.
#' @return a validated object.
check_NitroDriven <- function (object) {
  if (object@hits < 1 ) {
    return("hits must be an integer > 0")
  }
  if (object@replications < 1 ) {
    return("replications must be an integer > 0")
  }
  if (object@consense_times < 0 ) {
    return("consense_times must be zero or a positive integer")
  }
  if (any(!sapply(object@sectorial_search, inherits, "NitroSectorialSearch"))) {
    return("sectorial_search must contain objects inheriting class NitroSectorialSearch")
  }
  if (any(duplicated(sapply(object@sectorial_search, class)))) {
    return("sectorial_search must not contain duplicated search classes")
  }
}

#' Validate Nitro S4 objects
#'
#' @param object a prospective object that inherits \code{NitroImpliedWeights}.
#' @return a validated object.
check_NitroImpliedWeights <- function (object) {
  if (object@k <= 0 | object@k > 1000) {
    return("k must be a number between 0 and 1000")
  }
  if (object@proportion < 0 | object@proportion > 1) {
    return("proportion must be a number between 0 and 1")
  }
  if (object@max_ratio < 1 | object@max_ratio > 1000) {
    return("max_ratio must be a number between 1 and 1000")
  }
}

check_NitroConstrainedSearch <- function (object) {
  print("Validating constraints")
  for (constraint in c(object@positive, object@negative)) {
    c_names <- names(constraint) == c("fixed", "floating")
    if (!all(c_names) | !length(c_names)) {
      return("All constraints must contain named 'fixed' and 'floating' attributes")
    }
  }
}

check_NitroConstraintsBase <- function (object) {
  if (!all(sapply(object@constraints, inherits, "NitroConstraint"))) {
    return("constraints must be a list of NitroConstraint objects")
  }
}

check_NitroConstraint <- function (object) {
  if (sum(object@fixed) == 0 & sum(object@floating) > 0) {
    return("Cannot have floating constraints without fixed constraints")
  }
  if (any(object@fixed & object@floating)) {
    return("A constraint in floating cannot also be fixed")
  }
}

check_NitroTrees <- function (object) {
  if (class(object@trees) != "multiPhylo") {
    return("trees must be of class multiPhylo")
  }
}
