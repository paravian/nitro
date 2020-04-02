#' Run an analysus using implied weighting
#'
#' @param params the parameters for an analysis as specified from the output
#'   of the \code{implicit.enum}, \code{branchswap}, \code{ratchet}, or
#'   \code{driven} commands.
#' @param k an integer value indicating the concavity constant to apply.
#' @param multi.k a logical value indicating whether each character will be
#'   given an independent concavity constant based on the value of \code{k}.
#' @export
implied.weighting <- function(params, k=3, multi.k=FALSE) {
  if (is.numeric(k) == FALSE | length(k) != 1) {
    stop("'k' must be a numeric")
  } else if (k <= 0 | k > 1000) {
    stop("'k' must be a number between 0 and 1000")
  }
  if (is.logical(multi.k) == FALSE) {
    stop("'multi.k' must be a logical")
  }

  params$tnt.params$k <- k
  params$tnt.params$multi.k <- multi.k

  params$tnt.params$iw.cmd <- paste0("piwe =", params$tnt.params$k, ";")
  if (params$tnt.params$multi.k) {
    params$tnt.params$eiw.cmd <- "xpiwe (*;"
  }

  return(params)
}
