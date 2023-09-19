#' Set options for tree drifting
#'
#' @description
#' \code{TreeDriftingOptions} is an R6 class that defines the set of parameters
#' required for performing tree drift operations in \code{nitro}.
#' @importFrom checkmate asInt check_int check_flag check_number
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @export
TreeDriftingOptions <- R6Class("TreeDriftingOptions",
  private = list(
    .iterations = NULL,
    .substitutions = NULL,
    .max_abs_fit_diff = NULL,
    .max_rel_fit_diff = NULL,
    .reject_factor = NULL,
    .autoconstrain_cycles = NULL
  ),
  active = list(
    #' @field iterations An integer value indicating the number of tree-drifting
    #'   cycles to perform.
    iterations = function (value) {
      if (missing(value)) {
        return(private$.iterations)
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg iterations} must be an integer",
                      "x" = val_check))
        }
        private$.iterations <- asInt(value)
      }
    },
    #' @field substitutions An integer value indicating the number of replacements
    #' (i.e., accepted tree rearrangements) to perform in the perturbation phase.
    substitutions = function (value) {
      if (missing(value)) {
        return(private$.substitutions)
      } else {
        val_check <- check_int(value, lower = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg substitutions} must be an integer",
                      "x" = val_check))
        }
        private$.substitutions <- asInt(value)
      }
    },
    #' @field max_abs_fit_diff A numeric value indicating the maximum absolute fit
    #'   difference.
    max_abs_fit_diff = function (value) {
      if (missing(value)) {
        return(private$.max_abs_fit_diff)
      } else {
        val_check <- check_number(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg max_abs_fit_diff} must be an integer",
                      "x" = val_check))
        }
        private$.max_abs_fit_diff <- value
      }
    },
    #' @field max_rel_fit_diff A numeric value indicating the maximum relative fit
    #'   difference.
    max_rel_fit_diff = function (value) {
      if (missing(value)) {
        return(private$.max_rel_fit_diff)
      } else {
        val_check <- check_number(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg max_rel_fit_diff} must be an integer",
                      "x" = val_check))
        }
        private$.max_rel_fit_diff <- value
      }
    },
    #' @field reject_factor A numeric value indicating the rejection factor for
    #'   suboptimal trees.
    reject_factor = function (value) {
      if (missing(value)) {
        return(private$.reject_factor)
      } else {
        val_check <- check_number(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg reject_factor} must be an integer",
                      "x" = val_check))
        }
        private$.reject_factor <- value
      }
    },
    #' @field autoconstrain_cycles An integer value indicating the number of
    #'   autoconstrained cycles to perform.
    autoconstrain_cycles = function (value) {
      if (missing(value)) {
        return(private$.autoconstrain_cycles)
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg autoconstrain_cycles} must be an integer",
                      "x" = val_check))
        }
        private$.autoconstrain_cycles <- asInt(value)
      }
    }
  ),
  public = list(
    #' @param iterations An integer value indicating the number of tree-drifting
    #'   cycles to perform.
    #' @param substitutions An integer value indicating the number of replacements
    #' (i.e., accepted tree rearrangements) to perform in the perturbation phase.
    #' @param max_abs_fit_diff A numeric value indicating the maximum absolute fit
    #'   difference.
    #' @param max_rel_fit_diff A numeric value indicating the maximum relative fit
    #'   difference.
    #' @param reject_factor A numeric value indicating the rejection factor for
    #'   suboptimal trees.
    #' @param autoconstrain_cycles An integer value indicating the number of
    #'   autoconstrained cycles to perform.
    initialize = function (iterations = 30, substitutions = 60,
                           max_abs_fit_diff = 1, max_rel_fit_diff = 0.2,
                           reject_factor = 3, autoconstrain_cycles = 0) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A TNT tree drift configuration\")}")

      options <- c(
        "Iterations:" = self$iterations,
        "Substitutions:" = self$substitutions,
        "Maximum absolute fit difference:" = self$max_abs_fit_diff,
        "Maximum relative fit difference:" = self$max_rel_fit_diff,
        "Rejection factor:" = self$reject_factor,
        "Autoconstrain cycles:" = self$autoconstrain_cycles
        ) %>%
        as.character() %>%
        data.frame()

      colnames(options) <- NULL
      print(options)
    },
    #' @param set_only A logical indicating whether to instruct the command to
    #'   execute immediately (\code{TRUE}) or set the variables for future
    #'   execution \code{FALSE}.
    queue = function (set_only = FALSE) {
      val_check <- check_flag(set_only)
      if (!test_true(val_check)) {
        cli_abort(c("{.arg set_only} must be a logical",
                    "x" = val_check))
      }

      queue <- CommandQueue$new()
      cmd_flag <- ifelse(set_only, ":", "=")
      if (!set_only) {
        queue$add("mult", "= wagner replic 10")
      }

      drift_opts <- self %>%
        glue_data("iterations {iterations} numsubs {substitutions} fitdiff {max_abs_fit_diff} rfitdiff {max_rel_fit_diff} xfactor {reject_factor}")

      autoconst <- ifelse(
        self$autoconstrain_cycles == 0, "noautoconst",
        glue("autoconst {self$autoconstrain_cycles}")
      )

      drift_opts <- glue("{cmd_flag} {drift_opts} {autoconst}")
      queue$add("drift", drift_opts)
      return(queue)
    }
  )
)
