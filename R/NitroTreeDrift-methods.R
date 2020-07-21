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
  objs <- ls()
  args <- as.list(environment())[objs]
  do.call("new", c("NitroTreeDrift", args))
}

#' @importFrom methods callNextMethod
setMethod("initialize", "NitroTreeDrift",
  function (.Object, iterations = 30, substitutions = 40, max_abs_fit_diff = 1,
            max_rel_fit_diff = 0.2, reject_factor = 3,
            autoconstrain_cycles = 0) {
  objs <- ls()
  mf <- match.call()
  m <- match(c(".Object", objs), names(mf), 0L)
  mf <- mf[m]

  args <- as.list(mf)
  if (class(args$iterations) == "numeric") {
    args$iterations <- as.integer(args$iterations)
  }
  if (class(substitutions) == "numeric") {
    args$substitutions <- as.integer(args$substitutions)
  }
  if (class(autoconstrain_cycles) == "numeric") {
    args$autoconstrain_cycles <- as.integer(args$autoconstrain_cycles)
  }
  do.call("callNextMethod", args)
})

#' @rdname tnt_cmd
setMethod("tnt_cmd", "NitroTreeDrift", function (n) {
  set_only <- any(sapply(sys.frames(),
                         function (f) inherits(f$n, "NitroDriven")))
  if (set_only) {
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
