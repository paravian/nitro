#' Define tree hybridization properties
#'
#' @description
#' \code{NitroTreeHybridize} is an R6 class that defines the set of parameters
#' required for performing tree hybridizing operations in \code{nitro}.
#' @importFrom checkmate asInt assertInt assertLogical
#' @importFrom R6 R6Class
#' @export
NitroTreeHybridize <- R6Class("NitroTreeHybridize",
  inherit = NitroMethodsBase,
  private = list(
    .rounds = NULL,
    .hybridizations = NULL,
    .best_trees = NULL,
    .replace = NULL,
    .sample_factor = NULL
  ),
  active = list(
    #' @field rounds An integer value indicating the number of rounds of
    #'   tree-hybridizing to perform.
    rounds = function (value) {
      if (missing(value)) {
        private$.rounds
      } else {
        assertInt(value, lower = 0)
        value <- asInt(value)
        private$.rounds <- value
      }
    },
    #' @field hybridizations An integer value indicating the number of
    #'   hybridizations to perform in each round.
    hybridizations = function (value) {
      if (missing(value)) {
        private$.hybridizations
      } else {
        assertInt(value, lower = 1)
        value <- asInt(value)
        private$.hybridizations <- value
      }
    },
    #' @field best_trees An integer value indicating the number of best trees from
    #'   the previous round of hybridizing to use in the next round.
    best_trees = function (value) {
      if (missing(value)) {
        private$.best_trees
      } else {
        assertInt(value, lower = 1)
        value <- asInt(value)
        private$.best_trees <- value
      }
    },
    #' @field replace A logical value indicating whether to replace the source tree
    #'   with a better tree produced by hybridizing.
    replace = function (value) {
      if (missing(value)) {
        private$.replace
      } else {
        assertLogical(value, len = 1)
        private$.replace <- value
      }
    },
    #' @field sample_factor An integer value indicating the number of times to
    #'   increase the size of initial tree set by. The corresponding number of trees
    #'   to retain will be proportional to the inverse of this value.
    sample_factor = function (value) {
      if (missing(value)) {
        private$.sample_factor
      } else {
        assertInt(value, lower = 1)
        value <- asInt(value)
        private$.sample_factor <- value
      }
    }
  ),
  public = list(
    #' @param rounds An integer value indicating the number of rounds of
    #'   tree-hybridizing to perform.
    #' @param hybridizations An integer value indicating the number of
    #'   hybridizations to perform in each round.
    #' @param best_trees An integer value indicating the number of best trees from
    #'   the previous round of hybridizing to use in the next round.
    #' @param replace A logical value indicating whether to replace the source tree
    #'   with a better tree produced by hybridizing.
    #' @param sample_factor An integer value indicating the number of times to
    #'   increase the size of initial tree set by. The corresponding number of trees
    #'   to retain will be proportional to the inverse of this value.
    initialize = function (rounds = 1, hybridizations = 1000, best_trees = 50,
                           replace = TRUE, sample_factor = 15) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cat("<NitroTreeHybridize>\n")
      cat(paste("* Rounds:", private$.rounds, "\n"))
      cat(paste("* Number of hybridizations:", private$.hybridizations, "\n"))
      cat(paste("* Number of best start trees:", private$.best_trees, "\n"))
      cat(paste("* Replace source trees:", private$.replace, "\n"))
      cat(paste("* Sampling factor:", private$.sample_factor, "\n"))
    },
    #' @param ... Ignored.
    tnt_cmd = function (...) {
      paste("tfuse: hybrid ", private$.rounds, "*", private$.hybridizations, "/",
            private$.best_trees,
            ifelse(private$.replace, " replace", " noreplace"),
            ifelse(private$.sample_factor > 0,
                   paste(" clog", private$.sample_factor), " noclog"), ";",
            sep = "")
    }
  )
)
