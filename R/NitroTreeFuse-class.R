#' Define tree fusing properties
#'
#' @description
#' \code{NitroTreeFuse} is an R6 class that defines the set of parameters
#' required for performing tree fusing operations in \code{nitro}.
#' @importFrom checkmate asInt assertInt assertLogical
#' @importFrom R6 R6Class
#' @export
NitroTreeFuse <- R6Class("NitroTreeFuse",
  inherit = NitroMethodsBase,
  private = list(
    .rounds = NULL,
    .exchange_equal = NULL,
    .start_best = NULL,
    .keep_all = NULL,
    .accept_all = NULL,
    .swap = NULL
  ),
  active = list(
    #' @field rounds An integer value indicating the number of tree-fusing rounds
    #'   to perform.
    rounds = function (value) {
      if (missing(value)) {
        private$.rounds
      } else {
        assertInt(value, lower = 0, upper = 100)
        value <- asInt(value)
        private$.rounds <- value
      }
    },
    #' @field exchange_equal A logical value indicating whether to accept exchanges
    #'   of equal score.
    exchange_equal = function (value) {
      if (missing(value)) {
        private$.exchange_equal
      } else {
        assertLogical(value, len = 1)
        private$.exchange_equal <- value
      }
    },
    #' @field start_best A logical value indicating whether to use the best tree to
    #'   start tree-fusing.
    start_best = function (value) {
      if (missing(value)) {
        private$.start_best
      } else {
        assertLogical(value, len = 1)
        private$.start_best <- value
      }
    },
    #' @field keep_all A logical value indicating whether to keep all trees found
    #'   instead of only the best trees.
    keep_all = function (value) {
      if (missing(value)) {
        private$.keep_all
      } else {
        assertLogical(value, len = 1)
        private$.keep_all <- value
      }
    },
    #' @field accept_all A logical value indicating whether to accept all exchanges
    #'   rather than only those that improve the best score.
    accept_all = function (value) {
      if (missing(value)) {
        private$.accept_all
      } else {
        assertLogical(value, len = 1)
        private$.accept_all <- value
      }
    },
    #' @field swap A logical value indicating whether to perform tree-bisection
    #'   reconnection swapping after exchanging clades.
    swap = function (value) {
      if (missing(value)) {
        private$.swap
      } else {
        assertLogical(value, len = 1)
        private$.swap <- value
      }
    }
  ),
  public = list(
    #' @param rounds An integer value indicating the number of tree-fusing rounds
    #'   to perform.
    #' @param exchange_equal A logical value indicating whether to accept exchanges
    #'   of equal score.
    #' @param start_best A logical value indicating whether to use the best tree to
    #'   start tree-fusing.
    #' @param keep_all A logical value indicating whether to keep all trees found
    #'   instead of only the best trees.
    #' @param accept_all A logical value indicating whether to accept all exchanges
    #'   rather than only those that improve the best score.
    #' @param swap A logical value indicating whether to perform tree-bisection
    #'   reconnection swapping after exchanging clades.
    initialize = function (rounds = 5, exchange_equal = FALSE,
                           start_best = TRUE, keep_all = TRUE,
                           accept_all = TRUE, swap = TRUE) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cat("<NitroTreeFuse>\n")
      cat(paste("* Rounds:", private$.rounds, "\n"))
      cat(paste("* Exchange equal score trees:", private$.exchange_equal, "\n"))
      cat(paste("* Start with best tree:", private$.start_best, "\n"))
      cat(paste("* Keep all trees:", private$.keep_all, "\n"))
      cat(paste("* Accept all exchanges:", private$.accept_all, "\n"))
      cat(paste("* Swap after exchanges:", private$.swap, "\n"))
    },
    #' @param ... Ignored.
    tnt_cmd = function (...) {
      return(paste("tfuse: rounds ", private$.rounds,
                   ifelse(private$.exchange_equal, " equals", " noequals"),
                   ifelse(private$.start_best, " beststart", " nobeststart"),
                   ifelse(private$.keep_all, " keepall", " nokeepall"),
                   ifelse(private$.accept_all, " norepeat", " repeat"),
                   ifelse(private$.swap, " swap", " noswap"), ";", sep = ""))
    }
  )
)
