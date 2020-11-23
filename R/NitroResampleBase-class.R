#' Define resampling parameters
#'
#' @description
#' \code{NitroResampleBase} is an R6 class that defines parameters for
#' resampling analyses.
#' @importFrom checkmate asInt assertClass assertInt assertLogical
#' @importFrom R6 R6Class
#' @export
NitroResampleBase <- R6Class("NitroResampleBase",
  inherit = NitroMethodsBase,
  private = list(
    .search_method = NULL,
    .phy = NULL,
    .replications = NULL,
    .abs_freq_summary = NULL,
    .freq_diff_summary = NULL
  ),
  active = list(
    #' @field search_method An object inheriting class
    #'   \code{"\link{NitroMethodsBase}"}.
    search_method = function (value) {
      if (missing(value)) {
        private$.search_method
      } else {
        assertClass(value, "NitroMethodsBase")
        private$.search_method <- value
      }
    },
    #' @field phy A tree of class \code{phylo}. Resampling values will be
    #'   calculated using this topology.
    phy = function (value) {
      if (missing(value)) {
        private$.phy
      } else {
        assertClass(value, "phylo")
        private$.phy <- value
      }
    },
    #' @field replications An integer value indicating the number of resampling
    #'   replications to perform.
    replications = function (value) {
      if (missing(value)) {
        private$.replications
      } else {
        assertInt(value, lower = 1)
        value <- asInt(value)
        private$.replications <- value
      }
    },
    #' @field abs_freq_summary A logical value indicating whether to summarize
    #'   supports using absolute frequencies.
    abs_freq_summary = function (value) {
      if (missing(value)) {
        private$.abs_freq_summary
      } else {
        assertLogical(value, len = 1)
        private$.abs_freq_summary <- value
      }
    },
    #' @field freq_diff_summary A logical value indicating whether to summarize
    #'   supports using frequency differences (i.e.,
    #'   Group supported/Contradicted).
    freq_diff_summary = function (value) {
      if (missing(value)) {
        private$.freq_diff_summary
      } else {
        assertLogical(value, len = 1)
        private$.freq_diff_summary <- value
      }
    }
  ),
  public = list(
    #' @param search_method An object inheriting class
    #'   \code{"\link{NitroMethodsBase}"}.
    #' @param phy A tree of class \code{phylo}. Resampling values will be
    #'   calculated using this topology.
    #' @param replications An integer value indicating the number of resampling
    #'   replications to perform.
    initialize = function (search_method, phy, replications = 100) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    }
  )
)
