#' Define parameters for tree hybridizing operations
#'
#' @importFrom methods new
#' @param rounds an integer value indicating the number of rounds of
#'   tree-hybridizing to perform.
#' @param hybridizations an integer value indicating the number of
#'   hybridizations to perform in each round.
#' @param best_trees an integer value indicating the number of best trees from
#'   the previous round of hybridizing to use in the next round.
#' @param replace a logical value indicating whether to replace the source tree
#'   with a better tree produced by hybridizing.
#' @param sample_factor an integer value indicating the number of times to
#'   increase the size of initial tree set by. The corresponding number of trees
#'   to retain will be proportional to the inverse of this value.
#' @export
NitroTreeHybridize <- function (rounds = 1, hybridizations = 1000,
                                best_trees = 50, replace = TRUE,
                                sample_factor = 15) {
  objs <- ls()
  args <- as.list(environment())[objs]
  do.call("new", c("NitroTreeHybridize", args))
}

#' @importFrom methods callNextMethod
setMethod("initialize", "NitroTreeHybridize",
  function (.Object, rounds = 1, hybridizations = 100, best_trees = 50,
            replace = TRUE, sample_factor = 15) {
  objs <- ls()
  mf <- match.call()
  m <- match(c(".Object", objs), names(mf), 0L)
  mf <- mf[m]

  args <- as.list(mf)
  if (class(args$rounds) == "numeric") {
    args$rounds <- as.integer(args$rounds)
  }
  if (class(args$hybridizations) == "numeric") {
    args$hybridizations <- as.integer(args$hybridizations)
  }
  if (class(args$best_trees) == "numeric") {
    args$best_trees <- as.integer(args$best_trees)
  }
  if (class(args$sample_factor) == "numeric") {
    args$sample_factor <- as.integer(args$sample_factor)
  }
  do.call("callNextMethod", args)
})

#' @rdname tnt_cmd
setMethod("tnt_cmd", "NitroTreeHybridize", function (n) {
  return(paste("tfuse: hybrid ", n@rounds, "*", n@hybridizations, "/",
               n@best_trees,
               ifelse(n@replace, " replace", " noreplace"),
               ifelse(n@sample_factor > 0,
                      paste(" clog", n@sample_factor), " noclog"), ";",
               sep = ""))
})

setMethod("show", "NitroTreeHybridize", function (object) {
  cat("Parameters for tree-fusing:\n\n")
  cat(paste("Rounds:                     ", object@rounds, "\n"))
  cat(paste("Number of hybridizations:   ", object@hybridizations, "\n"))
  cat(paste("Number of best start trees: ", object@best_trees, "\n"))
  cat(paste("Replace source trees:       ", object@replace, "\n"))
  cat(paste("Sampling factor:            ", object@sample_factor, "\n"))
})
