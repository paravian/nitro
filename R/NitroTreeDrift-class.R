#' Define tree drift properties
#'
#' @description
#' \code{NitroTreeDrift} is an R6 class that defines the set of parameters
#' required for performing tree drift operations in \code{nitro}.
#' @importFrom checkmate asInt assertInt assertNumber
#' @importFrom R6 R6Class
#' @export
NitroTreeDrift <- R6Class("NitroTreeDrift",
  inherit = NitroMethodsBase,
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
        private$.iterations
      } else {
        assertInt(value, lower = 0)
        private$.iterations <- asInt(value)
      }
    },
    #' @field substitutions An integer value indicating the number of replacements
    #' (i.e., accepted tree rearrangements) to perform in the perturbation phase.
    substitutions = function (value) {
      if (missing(value)) {
        private$.substitutions
      } else {
        assertInt(value, lower = 1)
        private$.substitutions <- asInt(value)
      }
    },
    #' @field max_abs_fit_diff A numeric value indicating the maximum absolute fit
    #'   difference.
    max_abs_fit_diff = function (value) {
      if (missing(value)) {
        private$.max_abs_fit_diff
      } else {
        assertNumber(value, lower = 0)
        private$.max_abs_fit_diff <- value
      }
    },
    #' @field max_rel_fit_diff A numeric value indicating the maximum relative fit
    #'   difference.
    max_rel_fit_diff = function (value) {
      if (missing(value)) {
        private$.max_rel_fit_diff
      } else {
        assertNumber(value, lower = 0)
        private$.max_rel_fit_diff <- value
      }
    },
    #' @field reject_factor A numeric value indicating the rejection factor for
    #'   suboptimal trees.
    reject_factor = function (value) {
      if (missing(value)) {
        private$.reject_factor
      } else {
        assertNumber(value, lower = 0)
        private$.reject_factor <- value
      }
    },
    #' @field autoconstrain_cycles An integer value indicating the number of
    #'   autoconstrained cycles to perform.
    autoconstrain_cycles = function (value) {
      if (missing(value)) {
        private$.autoconstrain_cycles
      } else {
        assertInt(value, lower = 0)
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
      cat("<NitroTreeDrift>\n")
      cat(paste("* Iterations:", private$.iterations, "\n"))
      cat(paste("* Substitutions:", private$.substitutions, "\n"))
      cat(paste("* Maximum absolute fit difference:", private$.max_abs_fit_diff, "\n"))
      cat(paste("* Maximum relative fit difference:", private$.max_rel_fit_diff, "\n"))
      cat(paste("* Rejection factor:", private$.reject_factor, "\n"))
      cat(paste("* Autoconstrain cycles:", private$.autoconstrain_cycles, "\n"))
    },
    #' @param set_only A logical indicating whether to instruct the command to
    #'   execute immediately (\code{TRUE}) or set the variables for future
    #'   execution \code{FALSE}.
    tnt_cmd = function (set_only = FALSE) {
      assertLogical(set_only, len = 1)
      cmd_flag <- ifelse(set_only, ":", "=")

      fuse_cmd <- c()
      if (!set_only) {
        fuse_cmd <- c("mult= wagner replic 10;")
      }
      fuse_cmd <- c(fuse_cmd,
                    paste("drift", cmd_flag,
                          " iterations ", private$.iterations,
                          " numsubs ", private$.substitutions,
                          " fitdiff ", private$.max_abs_fit_diff,
                          " rfitdiff ", private$.max_rel_fit_diff,
                          " xfactor ", private$.reject_factor,
                          ifelse(private$.autoconstrain_cycles == 0,
                                 " noautoconst",
                                 paste(" autoconst", private$.autoconstrain_cycles)),
                          ";", sep = ""))
      fuse_cmd
    }
  )
)
