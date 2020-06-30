#' Define parameters for tree drift operations
#'
#' @importFrom methods new
#' @param iterations an integer value indicating the number of tree-drifting
#'   cycles to perform.
#' @param substitutions an integer value indicating the number of replacements
#' (i.e., accepted tree rearrangements) to perform in the perturbation phase.
#' @param max_abs_fit_diff a numeric value indicating the maximum absolute fit
#'   difference.
#' @param max_rel_fit_diff a numeric value indicating the maximum relative fit
#'   difference.
#' @param reject_factor a numeric value indicating the rejection factor for
#'   suboptimal trees.
#' @param autoconstrain_cycles an integer value indicating the number of
#'   autoconstrained cycles to perform.
#' @export
NitroTreeDrift <- function (iterations = 30, substitutions = 60,
                            max_abs_fit_diff = 1, max_rel_fit_diff = 0.2,
                            reject_factor = 3, autoconstrain_cycles = 0) {
  new("NitroTreeDrift", iterations, substitutions, max_abs_fit_diff,
      max_rel_fit_diff, reject_factor, autoconstrain_cycles)
}

#' @importFrom methods callNextMethod
setMethod("initialize", "NitroTreeDrift",
  function (.Object, iterations, substitutions, max_abs_fit_diff,
            max_rel_fit_diff, reject_factor, autoconstrain_cycles) {
  if (class(iterations) == "numeric") {
    iterations <- as.integer(iterations)
  }
  if (class(substitutions) == "numeric") {
    substitutions <- as.integer(substitutions)
  }
  if (class(autoconstrain_cycles) == "numeric") {
    autoconstrain_cycles <- as.integer(autoconstrain_cycles)
  }
  .Object <- callNextMethod(.Object, iterations = iterations,
    substitutions = substitutions, max_abs_fit_diff = max_abs_fit_diff,
    max_rel_fit_diff = max_rel_fit_diff, reject_factor = reject_factor,
    autoconstrain_cycles = autoconstrain_cycles)
  .Object
})

#' @rdname tnt_cmd
setMethod("tnt_cmd", "NitroTreeDrift", function (n) {
  env <- parent.frame()
  fuse_cmd <- c()
  if (is(env$n)[[1]] == c("NitroDriven")) {
    cmd_suffix = ":"
  } else {
    cmd_suffix = "="
    fuse_cmd <- c("mult= wagner replic 10;")
  }
  return(paste("drift", cmd_suffix, " iterations ", n@iterations,
               " numsubs ", n@substitutions,
               " fitdiff ", n@max_abs_fit_diff,
               " rfitdiff ", n@max_rel_fit_diff,
               " xfactor ", n@reject_factor,
               ifelse(n@autoconstrain_cycles == 0, " noautoconst",
                      paste(" autoconst", n@autoconstrain_cycles)),
               ";", sep = ""))
})

setMethod("show", "NitroTreeDrift", function (object) {
  cat("Parameters for tree-drifting:\n\n")
  cat(paste("Iterations:                 ", object@iterations, "\n"))
  cat(paste("Substitutions:              ", object@substitutions, "\n"))
  cat(paste("Max. abs. fit difference:   ", object@max_abs_fit_diff, "\n"))
  cat(paste("Max. rel. fit difference:   ", object@max_rel_fit_diff, "\n"))
  cat(paste("Rejection factor:           ", object@reject_factor, "\n"))
  cat(paste("Autoconstrain cycles:       ", object@autoconstrain_cycles, "\n"))
})
