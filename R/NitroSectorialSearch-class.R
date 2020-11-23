#' Define a sectorial search analysis
#'
#' @description
#' \code{NitroSectorialSearch} is an R6 class that defines the set of
#' parameters required to perform sectorial searching analyses in
#' \code{nitro}.
#' @importFrom checkmate assertInt assertLogical
#' @importFrom R6 R6Class
#' @export
NitroSectorialSearch <- R6Class("NitroSectorialSearch",
  inherit = NitroMethodsBase,
  private = list(
    .buffer = NULL,
    .slack = NULL
  ),
  active = list(
    #' @field buffer A logical value indicating whether to use an independent
    #    memory buffer for analysis of sectors.
    buffer = function (value) {
      if (missing(value)) {
        private$.buffer
      } else {
        assertLogical(value, len = 1)
        private$.buffer <- value
      }
    },
    #' @field slack An integer value indicating the percentage to increase the
    #'   available memory during searches.
    slack = function (value) {
      if (missing(value)) {
        private$.slack
      } else {
        assertInt(value, lower = 0)
        private$.slack <- value
      }
    }
  ),
  public = list(
    #' @param buffer A logical value indicating whether to use an independent
    #    memory buffer for analysis of sectors.
    #' @param slack An integer value indicating the percentage to increase the
    #'   available memory during searches.
    initialize = function (buffer = TRUE, slack = 0) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    }
  )
)
