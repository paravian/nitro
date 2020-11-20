#' Define parameters for branch breaking
#'
#' @description
#' \code{NitroBranchBreak} is an R6 class that defines the set of parameters
#' required to perform branch breaking.
#' @importFrom checkmate asInt assertChoice assertInt assertLogical
#' @importFrom R6 R6Class
#' @export
NitroBranchBreak <- R6Class("NitroBranchBreak",
  inherit = NitroMethodsBase,
  private = list(
    .swapper = NULL,
    .cluster_size = NULL,
    .safe_unclip = NULL,
    .fill_only = NULL,
    .save_multiple = NULL,
    .random_clip = NULL
  ),
  active = list(
    #' @field swapper A character vector indicating the branch swapping algorithm
    #'   to use. Options are either '\code{tbr}' (the default) or '\code{spr}'.
    swapper = function (value) {
      if (missing(value)) {
        private$.swapper
      } else {
        assertChoice(value, c("spr", "tbr"))
        private$.swapper <- value
      }
    },
    #' @field cluster_size An integer value indicating the number of nodes
    #'   (clusters) to use during swapping. Applied only when \code{swapper} is
    #'   set to \code{tbr}. Larger clusters will result in faster swapping as
    #'   matrix size increases. If left as 0 (the default), cluster size will be
    #'   automatically determined.
    cluster_size = function (value) {
      if (missing(value)) {
        private$.cluster_size
      } else {
        assertInt(value, lower = 0)
        value <- asInt(value)
        private$.cluster_size <- value
      }
    },
    #' @field safe_unclip A logical value indicating whether to use a safer but
    #'   slower method for updating buffers when clipping to find a better tree.
    #'   Applied only when \code{swapper} is set to \code{tbr}.
    safe_unclip = function (value) {
      if (missing(value)) {
        private$.safe_unclip
      } else {
        assertLogical(value, len = 1)
        private$.safe_unclip <- value
      }
    },
    #' @field fill_only A logical value indicating whether to stop swapping when
    #'   the tree buffer is full.
    fill_only = function (value) {
      if (missing(value)) {
        private$.fill_only
      } else {
        assertLogical(value, len = 1)
        private$.fill_only <- value
      }
    },
    #' @field save_multiple A logical value indicating whether to save multiple
    #'   trees during swapping.
    save_multiple = function (value) {
      if (missing(value)) {
        private$.save_multiple
      } else {
        assertLogical(value, len = 1)
        private$.save_multiple <- value
      }
    },
    #' @field random_clip A logical value indicating whether to randomize the
    #'   tree clipping sequence.
    random_clip = function (value) {
      if (missing(value)) {
        private$.random_clip
      } else {
        assertLogical(value, len = 1)
        private$.random_clip <- value
      }
    }
  ),
  public = list(
    #' @param swapper A character vector indicating the branch swapping algorithm
    #'   to use. Options are either '\code{tbr}' (the default) or '\code{spr}'.
    #' @param cluster_size An integer value indicating the number of nodes
    #'   (clusters) to use during swapping. Applied only when \code{swapper} is
    #'   set to \code{tbr}. Larger clusters will result in faster swapping as
    #'   matrix size increases. If left as 0 (the default), cluster size will be
    #'   automatically determined.
    #' @param safe_unclip A logical value indicating whether to use a safer but
    #'   slower method for updating buffers when clipping to find a better tree.
    #'   Applied only when \code{swapper} is set to \code{tbr}.
    #' @param fill_only A logical value indicating whether to stop swapping when
    #'   the tree buffer is full.
    #' @param save_multiple A logical value indicating whether to save multiple
    #'   trees during swapping.
    #' @param random_clip A logical value indicating whether to randomize the
    #'   tree clipping sequence.
    initialize = function (swapper = "tbr", cluster_size = 0,
                           safe_unclip = FALSE, fill_only = FALSE,
                           save_multiple = TRUE, random_clip = FALSE) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cat("<NitroBranchBreak>\n")
      cat(paste("* Swapper type:", toupper(private$.swapper), "\n"))
      if (private$.swapper == "tbr") {
        cat(paste("* Number of nodes to swap:",
                  ifelse(private$.cluster_size, private$.cluster_size, "auto"), "\n"))
      }
      cat(paste("* Use safe unclipping:",
                ifelse(private$.safe_unclip, "yes", "no"), "\n"))
      cat(paste("* Stop when tree buffer full:",
                ifelse(private$.fill_only, "yes", "no"), "\n"))
      cat(paste("* Save multiple trees:",
                ifelse(private$.save_multiple, "yes", "no"), "\n"))
      cat(paste("* Randomize node clips:",
                ifelse(private$.random_clip, "yes", "no"), "\n"))
    },
    #' @param ... Ignored.
    tnt_cmd = function (...) {
      bbreak_cmd <- c()
      if (private$.cluster_size > 0) {
        bbreak_cmd <- c(bbreak_cmd,
                        paste("bbreak: clusters ", private$.cluster_size, ";", sep = ""))
      }
      bbreak_cmd <- c(bbreak_cmd,
                      paste("bbreak= ", private$.swapper,
                            ifelse(private$.safe_unclip, " safe", " nosafe"),
                            ifelse(private$.fill_only, " fillonly", " nofillonly"),
                            ifelse(private$.save_multiple, " mulpars", " nomulpars"),
                            ifelse(private$.random_clip, " randclip", " norandclip"),
                            ";", sep = ""))
      bbreak_cmd
    }
  )
)
