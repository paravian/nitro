#' Define parameters for tree fusing operations
#'
#' @importFrom methods new
#' @param rounds an integer value indicating the number of tree-fusing rounds
#'   to perform.
#' @param exchange_equal a logical value indicating whether to accept exchanges
#'   of equal score.
#' @param start_best a logical value indicating whether to use the best tree to
#'   start tree-fusing.
#' @param keep_all a logical value indicating whether to keep all trees found
#'   instead of only the best trees.
#' @param accept_all a logical value indicating whether to accept all exchanges
#'   rather than only those that improve the best score.
#' @param swap a logical value indicating whether to perform tree-bisection
#'   reconnection swapping after exchanging clades.
#' @export
NitroTreeFuse <- function (rounds = 5, exchange_equal = FALSE,
                           start_best = TRUE, keep_all = TRUE,
                           accept_all = TRUE, swap = TRUE) {
  new("NitroTreeFuse", rounds = rounds, exchange_equal = exchange_equal,
      start_best = start_best, keep_all = keep_all, accept_all = accept_all,
      swap = swap)
}

#' @importFrom methods callNextMethod
setMethod("initialize", "NitroTreeFuse",
  function (.Object, rounds, exchange_equal, start_best, keep_all,
            accept_all, swap) {
    if (class(rounds) == "numeric") {
      rounds <- as.integer(rounds)
    }
    .Object <- callNextMethod(.Object, rounds = rounds,
      exchange_equal = exchange_equal, start_best = start_best,
      keep_all = keep_all, accept_all = accept_all, swap = swap)
    .Object
  })

#' @rdname tnt_cmd
setMethod("tnt_cmd", "NitroTreeFuse", function (n) {
  return(paste("tfuse: rounds ", n@rounds,
               ifelse(n@exchange_equal, " equals", " noequals"),
               ifelse(n@start_best, " beststart", " nobeststart"),
               ifelse(n@keep_all, " keepall", " nokeepall"),
               ifelse(n@accept_all, " norepeat", " repeat"),
               ifelse(n@swap, " swap", " noswap"), ";", sep = ""))
})

setMethod("show", "NitroTreeFuse", function (object) {
  cat("Parameters for tree-fusing:\n\n")
  cat(paste("Rounds:                     ", object@rounds, "\n"))
  cat(paste("Exchange equal score trees: ", object@exchange_equal, "\n"))
  cat(paste("Start with best tree:       ", object@start_best, "\n"))
  cat(paste("Keep all trees:             ", object@keep_all, "\n"))
  cat(paste("Accept all exchanges:       ", object@accept_all, "\n"))
  cat(paste("Swap after exchanges:       ", object@swap, "\n"))
})
