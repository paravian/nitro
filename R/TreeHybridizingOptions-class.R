#' Set options for tree hybridizing
#'
#' @description
#' \code{TreeHybridizingOptions} is an R6 class that defines the set of
#'   parameters required for performing tree hybridizing operations in
#'   \code{nitro}.
#' @importFrom checkmate asInt check_int check_flag
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @export
TreeHybridizingOptions <- R6Class("TreeHybridizingOptions",
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
        return(private$.rounds)
      } else {
        val_check <- check_int(value, lower = 0)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg rounds} must be an integer.",
                      "x" = val_check))
        }
        value <- asInt(value)
        private$.rounds <- value
      }
    },
    #' @field hybridizations An integer value indicating the number of
    #'   hybridizations to perform in each round.
    hybridizations = function (value) {
      if (missing(value)) {
        return(private$.hybridizations)
      } else {
        val_check <- check_int(value, lower = 1)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg hybridizations} must be an integer.",
                      "x" = val_check))
        }
        value <- asInt(value)
        private$.hybridizations <- value
      }
    },
    #' @field best_trees An integer value indicating the number of best trees from
    #'   the previous round of hybridizing to use in the next round.
    best_trees = function (value) {
      if (missing(value)) {
        return(private$.best_trees)
      } else {
        val_check <- check_int(value, lower = 1)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg best_trees} must be an integer",
                      "x" = val_check))
        }
        value <- asInt(value)
        private$.best_trees <- value
      }
    },
    #' @field replace A logical value indicating whether to replace the source tree
    #'   with a better tree produced by hybridizing.
    replace = function (value) {
      if (missing(value)) {
        return(private$.replace)
      } else {
        val_check <- check_flag(value)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg replace} must be a logical.",
                      "x" = val_check))
        }
        private$.replace <- value
      }
    },
    #' @field sample_factor An integer value indicating the number of times to
    #'   increase the size of initial tree set by. The corresponding number of trees
    #'   to retain will be proportional to the inverse of this value.
    sample_factor = function (value) {
      if (missing(value)) {
        return(private$.sample_factor)
      } else {
        val_check <- check_int(value, lower = 1)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg sample_factor} must be an integer",
                      "x" = val_check))
        }
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
      cli_text("{col_grey(\"# A TNT tree hybridizing configuration\")}")

      options <- c("Rounds:" = self$rounds,
                   "Number of hybridizations:" = self$hybridizations,
                   "Number of best start trees:" = self$best_trees,
                   "Replace source trees:" = ifelse(self$replace, "yes", "no"),
                   "Sampling factor:" = self$sample_factor) %>%
        data.frame()

      colnames(options) <- NULL
      print(options)
    },
    #' @param ... Ignored.
    queue = function (...) {
      hybrid_opts <- self %>%
        glue_data(": hybrid {rounds}*{hybridizations}/{best_trees}")

      replace_opts <- ifelse(self$replace, "", "no") %>%
        glue("replace")
      sampfrac_opts <- ifelse(
        self$sample_factor > 0, glue("clog {self$sample_factor}"), "noclog"
      )

      queue <- CommandQueue$new()
      drift_opts <- glue("{hybrid_opts} {replace_opts} {sampfrac_opts}")
      queue$add("tfuse", drift_opts)
      return(queue)
    }
  )
)
