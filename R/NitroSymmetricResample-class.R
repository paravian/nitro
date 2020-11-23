#' Define symmetric resampling analysis
#'
#' @description
#' \code{NitroSymmetricResample} is an R6 class that defines parameters for
#' symmetric resampling analyses.
#' @importFrom checkmate asInt assertInt
#' @importFrom R6 R6Class
#' @export
NitroSymmetricResample <- R6Class("NitroSymmetricResample",
  inherit = NitroResampleBase,
  private = list(
    .probability = NULL,
    .cutoff = NULL
  ),
  active = list(
    #' @field probability An integer value indicating the change probability.
    probability = function (value) {
      if (missing(value)) {
        private$.probability
      } else {
        assertInt(value, lower = 0, upper = 99)
        private$.probability <- asInt(value)
      }
    },
    #' @field cutoff An integer value indicating the cutoff value for
    #'   frequencies.
    cutoff = function (value) {
      if (missing(value)) {
        private$.cutoff
      } else {
        assertInt(value, lower = 0, upper = 99)
        private$.cutoff <- asInt(value)
      }
    }
  ),
  public = list(
    #' @param probability An integer value indicating the change probability.
    #' @param cutoff An integer value indicating the cutoff value for
    #'   frequencies.
    #' @param search_method An object inheriting class
    #'   \code{"\link{NitroMethodsBase}"}.
    #' @param phy A tree of class \code{phylo}. Resampling values will be
    #'   calculated using this topology.
    #' @param replications an integer value indicating the number of resampling
    #'   replications to perform.
    initialize = function (probability = 36, cutoff = 0, search_method = NULL,
                           phy = NULL, replications = 100) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cat("<NitroSymmetricResample>\n")
      cat(paste("* Removal probability:", private$.probability, "\n"))
      cat(paste("* Frequency cutoff:", private$.cutoff, "\n"))
      cat(paste("* Replications:", private$.replications, "\n"))
    },
    #' @param ... Ignored.
    tnt_cmd = function (...) {
      paste("resample= sym from 0 replications ", private$.replications,
            " probability ", private$.probability,
            " cut ", private$.cutoff,
            " gc frequency [ ", paste(private$.search_method$tnt_cmd(), collapse = " "),
            " ];", sep = "")
    }
  )
)
