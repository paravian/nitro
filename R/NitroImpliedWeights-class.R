#' Define implied weights analysis
#'
#' @description
#' \code{NitroImpliedWeights} is an R6 class that serves as the basis for
#' classes that define parameters for implied weights phylogenetic analyses.
#' @importFrom checkmate assertLogical assertNumber
#' @importFrom R6 R6Class
#' @export
NitroImpliedWeights <- R6Class("NitroImpliedWeights",
  inherit = NitroWeightsBase,
  private = list(
    .k = NULL,
    .multi_k = NULL,
    .proportion = NULL,
    .max_ratio = NULL
  ),
  active = list(
    #' @field k A numeric value indicating the concavity constant to apply
    #'   during an implied weights analysis. When \code{multi_k} is
    #'   \code{TRUE}, \code{k} represents the concavity for a character with
    #'   no missing entries and serves as the basis for calculating per-site
    #'   concavity constants.
    k = function (value) {
      if (missing(value)) {
        private$.k
      } else {
        assertNumber(value, lower = 0)
        private$.k <- value
        self
      }
    },
    #' @field multi_k A logical value indicating whether each character will be
    #'   given an independent concavity constant based on the value of \code{k}
    #'   during an implied weights analysis.
    multi_k = function (value) {
      if (missing(value)) {
        private$.multi_k
      } else {
        assertLogical(value, len = 1)
        private$.multi_k <- value
        self
      }
    },
    #' @field proportion A numeric value indicating the proportion of homoplasy
    #'   missing values are assumed to have under implied weighting. This
    #'   parameter is only valid when \code{multi_k} is \code{TRUE}. A
    #'   proportion of 0 is equivalent to standard implied weighting, with
    #'   \code{multi_k} set as \code{FALSE}.
    proportion =  function (value) {
      if (missing(value)) {
        private$.proportion
      } else {
        assertNumber(value, lower = 0)
        private$.proportion <- value
        self
      }
    },
    #' @field max_ratio A numeric value indicating the maximum acceptable ratio
    #'   between two k values when multiple concavity constants are used (i.e.,
    #'   when \code{multi_k} is \code{TRUE}). A maximum ratio of 1 is
    #'   equivalent to standard implied weighting, with \code{multi_k} set as
    #'   \code{FALSE}.
    max_ratio =  function (value) {
      if (missing(value)) {
        private$.max_ratio
      } else {
        assertNumber(value, lower = 0)
        private$.max_ratio <- value
        self
      }
    }
  ),
  public = list(
    #' @param k A numeric value indicating the concavity constant to apply during
    #'   an implied weights analysis. When \code{multi_k} is \code{TRUE}, \code{k}
    #'   represents the concavity for a character with no missing entries and
    #'   serves as the basis for calculating per-site concavity constants.
    #' @param multi_k A logical value indicating whether each character will be
    #'   given an independent concavity constant based on the value of \code{k}
    #'   during an implied weights analysis.
    #' @param proportion A numeric value indicating the proportion of homoplasy
    #'   missing values are assumed to have under implied weighting. This parameter
    #'   is only valid when \code{multi_k} is \code{TRUE}. A proportion of 0 is
    #'   equivalent to standard implied weighting, with \code{multi_k} set as
    #'   \code{FALSE}.
    #' @param max_ratio A numeric value indicating the maximum acceptable ratio
    #'   between two k values when multiple concavity constants are used (i.e.,
    #'   when \code{multi_k} is \code{TRUE}). A maximum ratio of 1 is equivalent
    #'   to standard implied weighting, with \code{multi_k} set as \code{FALSE}.
    initialize = function (k = 3, multi_k = FALSE, proportion = 0.5,
                           max_ratio = 5) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cat("<NitroImpliedWeights>\n")
      cat(paste("* Concavity constant (k):     ", private$.k, "\n"))
      cat(paste("* Multiple constants:         ", private$.multi_k, "\n"))
      if (private$.multi_k) {
        cat(paste("* Proportion:                 ", private$.proportion, "\n"))
        cat(paste("* Maximum ratio:              ", private$.max_ratio, "\n"))
      }
    },
    #' @param ... Ignored.
    tnt_cmd = function (...) {
      cmds <- c(paste0("piwe =", private$.k))
      if (private$.multi_k) {
        cmds <- c(cmds, paste("xpiwe ( *", private$.proportion, " <", private$.max_ratio,
                              " /", private$.k, ";", sep = ""))
      }
      return(cmds)
    }
  )
)
