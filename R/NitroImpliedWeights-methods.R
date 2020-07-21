#' Define implied weights parameters
#'
#' @param k n numeric value indicating the concavity constant to apply during
#'   an implied weights analysis. When \code{multi_k} is \code{TRUE}, \code{k}
#'   represents the concavity for a character with no missing entries and
#'   serves as the basis for calculating per-site concavity constants.
#' @param multi_k a logical value indicating whether each character will be
#'   given an independent concavity constant based on the value of \code{k}
#'   during an implied weights analysis.
#' @param proportion an numeric value indicating the proportion of homoplasy
#'   missing values are assumed to have under implied weighting. This parameter
#'   is only valid when \code{multi_k} is \code{TRUE}. A proportion of 0 is
#'   equivalent to standard implied weighting, with \code{multi_k} set as
#'   \code{FALSE}.
#' @param max_ratio a numeric value indicating the maximum acceptable ratio
#'   between two k values when multiple concavity constants are used (i.e.,
#'   when \code{multi_k} is \code{TRUE}). A maximum ratio of 1 is equivalent
#'   to standard implied weighting, with \code{multi_k} set as \code{FALSE}.
#' @export
NitroImpliedWeights <- function (k = 3, multi_k = FALSE, proportion = 0.5,
                                 max_ratio = 5) {
  objs <- ls()
  args <- as.list(environment())[objs]
  do.call("new", c("NitroImpliedWeights", args))
}

#' @importFrom methods callNextMethod
setMethod("initialize", "NitroImpliedWeights",
  function (.Object, k = 3, multi_k = FALSE, proportion = 0.5, max_ratio = 5) {
  objs <- ls()
  mf <- match.call()
  m <- match(c(".Object", objs), names(mf), 0L)
  mf <- mf[m]

  args <- as.list(mf)
  do.call("callNextMethod", args)
})

#' @rdname tnt_cmd
setMethod("tnt_cmd", "NitroImpliedWeights", function (n) {
  cmds <- c(paste0("piwe =", n@k))
  if (n@multi_k) {
    cmds <- c(cmds, paste("xpiwe ( *", n@proportion, " <", n@max_ratio,
                          " /", n@k, ";", sep = ""))
  }
  return(cmds)
})

setMethod("show", "NitroImpliedWeights", function (object) {
  cat("\nParameters for implied weights:\n\n")
  cat(paste("Concavity constant (k):     ", object@k, "\n"))
  cat(paste("Multiple constants:         ", object@multi_k, "\n"))
  if (object@multi_k) {
    cat(paste("Proportion:                 ", object@proportion, "\n"))
    cat(paste("Maximum ratio:              ", object@max_ratio, "\n"))
  }
})
