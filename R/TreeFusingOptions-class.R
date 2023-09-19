#' Set options for tree fusing
#'
#' @description
#' \code{TreeFusingOptions} is an R6 class that defines the set of parameters
#' required for performing tree fusing operations in \code{nitro}.
#' @importFrom checkmate asInt check_int check_flag test_true
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom dplyr if_else
#' @importFrom R6 R6Class
#' @export
TreeFusingOptions <- R6Class("TreeFusingOptions",
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
        return(private$.rounds)
      } else {
        val_check <- check_int(value, lower = 0, upper = 100)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg rounds} must be an integer.",
                      "x" = val_check))
        }
        value <- asInt(value)
        private$.rounds <- value
      }
    },
    #' @field exchange_equal A logical value indicating whether to accept exchanges
    #'   of equal score.
    exchange_equal = function (value) {
      if (missing(value)) {
        return(private$.exchange_equal)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg iterations} must be a logical.",
                      "x" = val_check))
        }
        private$.exchange_equal <- value
      }
    },
    #' @field start_best A logical value indicating whether to use the best tree to
    #'   start tree-fusing.
    start_best = function (value) {
      if (missing(value)) {
        return(private$.start_best)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg start_best} must be a logical.",
                      "x" = val_check))
        }
        private$.start_best <- value
      }
    },
    #' @field keep_all A logical value indicating whether to keep all trees found
    #'   instead of only the best trees.
    keep_all = function (value) {
      if (missing(value)) {
        return(private$.keep_all)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg keep_all} must be a logical.",
                      "x" = val_check))
        }
        private$.keep_all <- value
      }
    },
    #' @field accept_all A logical value indicating whether to accept all exchanges
    #'   rather than only those that improve the best score.
    accept_all = function (value) {
      if (missing(value)) {
        return(private$.accept_all)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg accept_all} must be a logical.",
                      "x" = val_check))
        }
        private$.accept_all <- value
      }
    },
    #' @field swap A logical value indicating whether to perform tree-bisection
    #'   reconnection swapping after exchanging clades.
    swap = function (value) {
      if (missing(value)) {
        return(private$.swap)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg swap} must be a logical.",
                      "x" = val_check))
        }
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
      cli_text("{col_grey(\"# A TNT tree fusing configuration\")}")

      options <- c("Exchange equal score trees:" = self$exchange_equal,
                   "Start with best tree:" = self$start_best,
                   "Keep all trees:" = self$keep_all,
                   "Accept all exchanges:" = self$accept_all,
                   "Swap after exchanges:" = self$swap) %>%
        ifelse("yes", "no")

      options <- c("Rounds:" = as.character(self$rounds), options) %>%
        data.frame()

      colnames(options) <- NULL
      print(options)
    },
    #' @param ... Ignored.
    queue = function (...) {
      queue <- CommandQueue$new()
      fuse_args <- glue(": rounds {self$rounds} ",
        "{ifelse(self$exchange_equal, \"\", \"no\")}equals ",
        "{ifelse(self$start_best, \"\", \"no\")}beststart ",
        "{ifelse(self$keep_all, \"\", \"\")}keepall ",
        "{ifelse(self$accept_all, \"no\", \"\")}repeat ",
        "{ifelse(self$swap, \"\", \"no\")}swap ",
      )
      queue$add("tfuse", fuse_args)
      return(queue)
    }
  )
)
