#' Define a random sectorial search analysis
#'
#' @description
#' \code{NitroRandomSectorialSearch} is an R6 class that defines the set of
#' parameters required to perform sectorial searching analyses in
#' \code{nitro}.
#' @importFrom checkmate assert assertInt checkInt checkTRUE
#' @importFrom R6 R6Class
#' @export
NitroRandomSectorialSearch <- R6Class("NitroRandomSectorialSearch",
  inherit = NitroSectorialSearch,
  private = list(
    .min_size = NULL,
    .max_size = NULL,
    .selection_factor = NULL,
    .increase = NULL,
    .small_starts = NULL
  ),
  active = list(
    #' @field min_size An integer value indicating the minimum size of random
    #'   selections. If left as 0 (the default), this value will be
    #'   automatically set according to the number of taxa in the matrix when a
    #'   new \code{"\link{NitroTreeSearch}"} object is initialized.
    min_size = function (value) {
      if (missing(value)) {
        private$.min_size
      } else {
        assert(
          checkTRUE(value == 0),
          checkInt(value, lower = 5)
        )
        if (!is.null(private$.max_size)) {
          assertInt(value, upper = private$.max_size)
        }
        value <- asInt(value)
        private$.min_size <- value
      }
    },
    #' @field max_size An integer value indicating the maximum size of random
    #'   selections. If left at 0 (the default), this value will be
    #'   automatically set according to the number of taxa in the matrix when a
    #'   new \code{"\link{NitroTreeSearch}"} object is initialized.
    max_size = function (value) {
      if (missing(value)) {
        private$.max_size
      } else {
        assert(
          checkTRUE(value == 0),
          checkInt(value, lower = 5)
        )
        if (!is.null(private$.min_size)) {
          assertInt(value, lower = private$.min_size)
        }
        value <- asInt(value)
        private$.max_size <- value
      }
    },
    #' @field selection_factor An integer value indicating the maximum number
    #'   of random selections for the currently active taxa.
    selection_factor = function (value) {
      if (missing(value)) {
        private$.selection_factor
      } else {
        assertInt(value, lower = 0)
        private$.selection_factor <- asInt(value)
      }
    },
    #' @field increase An integer value indicating the factor to increase the
    #'   size of random selections if enough selection of the current size have
    #'   been completed.
    increase = function (value) {
      if (missing(value)) {
        private$.increase
      } else {
        assertInt(value, lower = 0)
        private$.increase <- asInt(value)
      }
    },
    #' @field small_starts An integer value indicating the number of random
    #'   addition sequence plus tree-bisection reconnection swaps to perform
    #'   for random selections below \code{min_size}.
    small_starts = function (value) {
      if (missing(value)) {
        private$.small_starts
      } else {
        assertInt(value, lower = 1)
        private$.small_starts <- asInt(value)
      }
    }
  ),
  public = list(
    #' @param min_size An integer value indicating the minimum size of random
    #'   selections. If left as 0 (the default), this value will be
    #'   automatically set according to the number of taxa in the matrix when a
    #'   new \code{"\link{NitroTreeSearch}"} object is initialized.
    #' @param max_size An integer value indicating the maximum size of random
    #'   selections. If left at 0 (the default), this value will be
    #'   automatically set according to the number of taxa in the matrix when a
    #'   new \code{"\link{NitroTreeSearch}"} object is initialized.
    #' @param selection_factor An integer value indicating the maximum number
    #'   of random selections for the currently active taxa.
    #' @param increase An integer value indicating the factor to increase the
    #'   size of random selections if enough selection of the current size have
    #'   been completed.
    #' @param small_starts An integer value indicating the number of random
    #'   addition sequence plus tree-bisection reconnection swaps to perform
    #'   for random selections below \code{min_size}.
    #' @param buffer A logical value indicating whether to use an independent
    #    memory buffer for analysis of sectors.
    #' @param slack An integer value indicating the percentage to increase the
    #'   available memory during searches.
    initialize = function (min_size = 0, max_size = 0, selection_factor = 50,
                           increase = 100, small_starts = 3, buffer = TRUE,
                           slack = 0) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cat("<NitroRandomSectorialSearch>\n")
      cat(paste("* Minimum selection size:", private$.min_size, "\n"))
      cat(paste("* Maximum selection size:", private$.max_size, "\n"))
      cat(paste("* Maximum random selections:", private$.selection_factor, "\n"))
      cat(paste("* Selection increase factor:", private$.increase, "\n"))
      cat(paste("* Use independent buffer:", private$.buffer, "\n"))
      cat(paste("* Percentage memory increase:", private$.slack, "\n"))
    },
    #' @param set_only A logical indicating whether to instruct the command to
    #'   execute immediately (\code{TRUE}) or set the variables for future
    #'   execution \code{FALSE}.
    tnt_cmd = function (set_only = FALSE) {
      assertLogical(set_only, len = 1)
      cmd_flag <- ifelse(set_only, ":", "=")
      sect_cmd <- c(" minsize ", private$.min_size,
                    " maxsize ", private$.max_size,
                    " selfact ", private$.selection_factor,
                    " increase ", private$.increase,
                    " starts ", private$.small_starts,
                    " slack ", private$.slack,
                    paste0(ifelse(private$.buffer, " ", " no"), "xbuf"))
      sect_cmd <- paste(c("sectsch", cmd_flag, " rss", sect_cmd, ";"), collapse = "", sep = "")
      sect_cmd
    }
  )
)
