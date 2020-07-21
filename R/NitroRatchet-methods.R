#' Define parameters for a parsimony ratchet analysis
#'
#' @importFrom methods new
#' @param iterations an integer value indicating the number of iterations.
#' @param replacements an integer value indicating the number of replacements
#'   (i.e., accepted tree rearrangements) to perform in each perturbation
#'   phase.
#' @param prob_up an integer value indicating the probability of upweighting a
#'   character.
#' @param prob_down an integer value indicating the probability of
#'   downweighting a character.
#' @export
NitroRatchet <- function (iterations = 50, replacements = 40, prob_up = 4,
                          prob_down = 4) {
  objs <- ls()
  args <- as.list(environment())[objs]
  do.call("new", c("NitroRatchet", args))
}

#' @importFrom methods callNextMethod
setMethod("initialize", "NitroRatchet",
  function (.Object, iterations = 50, replacements = 40, prob_up = 4,
            prob_down = 4) {
  objs <- ls()
  mf <- match.call()
  m <- match(c(".Object", objs), names(mf), 0L)
  mf <- mf[m]

  args <- as.list(mf)

  if (class(args$iterations) == "numeric") {
    args$iterations <- as.integer(args$iterations)
  }
  if (class(args$replacements) == "numeric") {
    args$replacements <- as.integer(args$replacements)
  }
  if (class(args$prob_up) == "numeric") {
    args$prob_up <- as.integer(args$prob_up)
  }
  if (class(args$prob_down) == "numeric") {
    args$prob_down <- as.integer(args$prob_down)
  }
  do.call("callNextMethod", args)
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
  ratchet_cmd <- c()
  set_only <- any(sapply(sys.frames(),
                         function (f) inherits(f$n, c("NitroDriven", "NitroRatchet"))))
  if (set_only) {
    cmd_suffix = ":"
  } else {
    cmd_suffix = "="
  }
  ratchet_cmd <- c(paste0("ratchet", cmd_suffix, " iter ", n@iterations,
                          " numsubs ", n@replacements,
                          " upfactor ", n@prob_up,
                          " downfact ", n@prob_down, ";"))
  return(ratchet_cmd)
})
