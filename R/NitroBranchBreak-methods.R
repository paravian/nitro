#' Define parameters for branch breaking
#'
#' @importFrom methods new
#' @param swapper a character vector indicating the branch swapping algorithm
#'   to use. Options are either '\code{tbr}' (the default) or '\code{spr}'.
#' @param cluster_size an integer value indicating the number of nodes
#'   (clusters) to use during swapping. Applied only when \code{swapper} is
#'   set to \code{tbr}. Larger clusters will result in faster swapping as
#'   matrix size increases. If left as 0 (the default), cluster size will be
#'   automatically determined.
#' @param safe_unclip a logical value indicating whether to use a safer but
#'   slower method for updating buffers when clipping to find a better tree.
#'   Applied only when \code{swapper} is set to \code{tbr}.
#' @param fill_only a logical value indicating whether to stop swapping when
#'   the tree buffer is full.
#' @param save_multiple a logical value indicating whether to save multiple
#'   trees during swapping.
#' @param random_clip a logical value indicating whether to randomize the
#'   tree clipping sequence.
#' @export
NitroBranchBreak <- function (swapper = c("tbr", "spr"), cluster_size = 0,
                              safe_unclip = FALSE, fill_only = FALSE,
                              save_multiple = TRUE, random_clip = FALSE) {
  swapper <- which(match.arg(swapper) == c("tbr", "spr"))
  new("NitroBranchBreak", swapper = swapper, cluster_size = cluster_size,
      safe_unclip = safe_unclip, fill_only = fill_only,
      save_multiple = save_multiple, random_clip = random_clip)
}

#' @importFrom methods callNextMethod
setMethod("initialize", "NitroBranchBreak",
  function (.Object, swapper, cluster_size, safe_unclip, fill_only,
            save_multiple, random_clip) {
  if (class(cluster_size) == "numeric") {
    cluster_size <- as.integer(cluster_size)
  }
  .Object <- callNextMethod(.Object, swapper = swapper,
    cluster_size = cluster_size, safe_unclip = safe_unclip,
    fill_only = fill_only, save_multiple = save_multiple,
    random_clip = random_clip)
  .Object
})

#' @rdname tnt_cmd
#' @include NitroTreeSearch-methods.R
setMethod("tnt_cmd", signature("NitroBranchBreak"), function (n) {
  swappers <- c("tbr", "spr")
  bbreak_cmd <- c()
  if (n@cluster_size > 0) {
    bbreak_cmd <- c(bbreak_cmd,
                    paste("bbreak: clusters ", n@cluster_size, ";", sep = ""))
  }
  bbreak_cmd <- c(bbreak_cmd,
                  paste("bbreak= ", swappers[n@swapper],
                        ifelse(n@safe_unclip, " safe", " nosafe"),
                        ifelse(n@fill_only, " fillonly", " nofillonly"),
                        ifelse(n@save_multiple, " mulpars", " nomulpars"),
                        ifelse(n@random_clip, " randclip", " norandclip"),
                        ";", sep = ""))
  return(bbreak_cmd)
})

setMethod("show", signature("NitroBranchBreak"), function (object) {
  swappers <- c("TBR", "SPR")
  cat("Parameters for branch-breaking:\n\n")
  cat(paste("Swapper:                    ", swappers[object@swapper], "\n"))
  if (object@swapper == 1) {
    cat(paste("Number of nodes to swap:    ",
              ifelse(object@cluster_size, object@cluster_size, "Auto"), "\n"))
  }
  cat(paste("Use safe unclipping:        ", object@safe_unclip, "\n"))
  cat(paste("Stop when tree buffer full: ", object@fill_only, "\n"))
  cat(paste("Save multiple trees:        ", object@save_multiple, "\n"))
  cat(paste("Randomize node clips:       ", object@random_clip, "\n"))
})
