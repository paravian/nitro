#' Define a bootstrap resampling analysis
#'
#' @description
#' \code{NitroBootstrap} is an R6 class that defines parameters for
#' bootstrap resampling analyses.
#' @importFrom R6 R6Class
#' @importFrom checkmate assertInt
#' @export
NitroBootstrap <- R6Class("NitroBootstrap",
  inherit = NitroResampleBase,
  private = list(
    .cutoff = NULL
  ),
  #' @field cutoff An integer value indicating the cutoff value for
  #'   frequencies.
  active = list(
    cutoff = function (value) {
      if (missing(value)) {
        private$.cutoff
      } else {
        assertInt(value, lower = 0, upper = 99)
        private$.cutoff <- asInt(value)
        self
      }
    }),
  public = list(
    #' @param cutoff An integer value indicating the cutoff value for
    #'   frequencies.
    #' @param search_method An object inheriting class
    #'   \code{"\link{NitroMethodsBase}"}.
    #' @param phy A tree of class \code{phylo}. Resampling values will be
    #'   calculated using this topology.
    #' @param replications An integer value indicating the number of resampling
    #'   replications to perform.
    initialize = function (cutoff = 0, search_method = NULL, phy = NULL,
                           replications = 100) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cat("<NitroBootstrap>\n")
      cat(paste("* Replications:", private$.replications, "\n"))
      cat(paste("* Frequency cutoff:", private$.cutoff, "\n"))
    },
    #' @param ... Ignored.
    tnt_cmd = function (...) {
      paste("resample= boot from 0 replications ", private$.replications,
            " cut ", private$.cutoff,
            " gc frequency [ ", paste(private$.search_method$tnt_cmd(), collapse = " "),
            " ];", sep = "")
    }
  )
)
