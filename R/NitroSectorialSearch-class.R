#' Set options common to sectorial searches
#'
#' @description
#' \code{SectorialSearchBaseOptions} is an R6 class that defines the set of
#'   options for using sectorial searches in \code{nitro}.
#' @importFrom checkmate check_int check_flag
#' @importFrom cli cli_abort
#' @importFrom R6 R6Class
SectorialSearchBaseOptions <- R6Class("SectorialSearchBaseOptions",
  private = list(
    .buffer = NULL,
    .slack = NULL
  ),
  active = list(
    #' @field buffer A logical value indicating whether to use an independent
    #    memory buffer for analysis of sectors.
    buffer = function (value) {
      if (missing(value)) {
        return(private$.buffer)
      } else {
        val_check <- check_flag(value)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg buffer} must be a logical.",
                      "x" = val_check))
        }
        private$.buffer <- value
      }
    },
    #' @field slack An integer value indicating the percentage to increase the
    #'   available memory during searches.
    slack = function (value) {
      if (missing(value)) {
        return(private$.slack)
      } else {
        val_check <- check_int(value, lower = 0)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg slack} must be an integer.",
                      "x" = val_check))
        }
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
