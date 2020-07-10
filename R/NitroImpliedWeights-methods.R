#' @importFrom methods callNextMethod
setMethod("initialize", "NitroImpliedWeights",
          function (.Object, k, weights, multi_k, proportion, max_ratio) {
  .Object <- callNextMethod(.Object, k = k, weights = weights,
                            multi_k = multi_k, proportion = proportion,
                            max_ratio = max_ratio)
  validObject(.Object)
  .Object
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
