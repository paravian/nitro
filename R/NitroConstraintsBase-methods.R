#' Add new constraint
#'
#' Function to add a new constraint to a constraint group.
#' @param group an object of type \code{\link{NitroConstraint}}
#' @param fixed_taxa a character vector of taxa to set as fixed constraints
#' @param floating_taxa a character vector of taxa to set as floating
#' constraints
#' @export
#' @rdname addConstraint
addConstraint <- function(tree_search, fixed_taxa, floating_taxa = character(), type = c("positive", "negative")) {
  is_positive <- match.arg(type) == "positive"
  mtx_taxa <- rownames(tree_search@matrix)
  all_c <- c(fixed_taxa, floating_taxa)
  if (!all(all_c %in% mtx_taxa)) {
    stop(paste("Could not find constraint taxa:", paste(all_c[!all_c %in% mtx_taxa], sep = ", ")))
  }
  fixed_taxa <- mtx_taxa %in% fixed_taxa
  floating_taxa <- mtx_taxa %in% floating_taxa
  constraint <- new("NitroConstraint", is_positive = is_positive,
                    fixed = fixed_taxa, floating = floating_taxa)
  newConstraint(tree_search@constraints) <- constraint
  tree_search
}

setMethod("length", "NitroConstraintsBase", function (x) {
  return(length(x@constraints))
})

setMethod("show", "NitroConstraintsBase", function (object) {
  cat("\nConstraints on monophyly:\n\n")
  is_pos <- sapply(object@constraints, slot, "is_positive")
  if (any(is_pos)) {
    cat(paste("Positive constraints:       ", sum(is_pos), "\n"))
  }
  if (any(!is_pos)) {
    cat(paste("Negative constraints:       ", sum(!is_pos), "\n"))
  }
})

setMethod("tnt_cmd", "NitroConstraintsBase", function (n) {
  return(c(paste("force ", sapply(n@constraints, tnt_cmd), ";", sep = ""),
           "constrain =;"))
})

#' Add constraint
#'
#' Function to add a new \code{\link{NitroConstraint}} to a
#' \code{\link{NitroConstraintBase}} object.
#' @param value an object that inherits from \code{\link{NitroConstraint}}
#' @export
#' @rdname newConstraint
setGeneric("newConstraint<-", function (n, value) standardGeneric("newConstraint<-"))

#' @rdname newConstraint
setMethod("newConstraint<-", c("NitroConstraintsBase", "NitroConstraint"), function (n, value) {
  n@constraints <- c(n@constraints, value)
  validObject(n)
  n
})
