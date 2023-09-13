#' Set options for parsimony ratchet
#'
#' \code{RatchetOptions} is an R6 class that defines the set of options
#' for performing a parsimony ratchet phylogenetic analysis in \code{nitro}.
#' @importFrom checkmate asInt check_int check_logical test_true
#' @importFrom cli cli_abort
#' @importFrom glue glue_data
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @export
RatchetOptions <- R6Class("RatchetOptions",
  private = list(
    .iterations = NULL,
    .replacements = NULL,
    .prob_up = NULL,
    .prob_down = NULL
  ),
  active = list(
    #' @field iterations An integer value indicating the number of iterations.
    iterations = function (value) {
      if (missing(value)) {
        return(private$.iterations)
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort("{.arg iterations} must be a single integer.",
                    "x" = val_check)
        }
        private$.iterations <- asInt(value)
      }
    },
    #' @field replacements An integer value indicating the number of
    #'   replacements (i.e., accepted tree rearrangements) to perform in each
    #'   perturbation phase.
    replacements = function (value) {
      if (missing(value)) {
        return(private$.replacements)
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort("{.arg replacements} must be a single integer.",
                    "x" = val_check)
        }
        private$.replacements <- asInt(value)
      }
    },
    #' @field prob_up An integer value indicating the probability of
    #'   upweighting a character.
    prob_up = function (value) {
      if (missing(value)) {
        return(private$.prob_up)
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort("{.arg prob_up} must be a single integer.",
                    "x" = val_check)
        }
        private$.prob_up <- asInt(value)
      }
    },
    #' @field prob_down An integer value indicating the probability of
    #'   downweighting a character.
    prob_down = function (value) {
      if (missing(value)) {
        return(private$.prob_down)
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort("{.arg prob_down} must be a single integer.",
                    "x" = val_check)
        }
        private$.prob_down <- asInt(value)
      }
    }
  ),
  public = list(
    #' @param iterations An integer value indicating the number of iterations.
    #' @param replacements An integer value indicating the number of
    #'   replacements (i.e., accepted tree rearrangements) to perform in each
    #'   perturbation phase.
    #' @param prob_up An integer value indicating the probability of
    #'   upweighting a character.
    #' @param prob_down An integer value indicating the probability of
    #'   downweighting a character.
    initialize = function (iterations = 50, replacements = 40, prob_up = 4,
                           prob_down = 4) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A TNT ratchet configuration\")}")

      options <- c("Iterations:" = self$iterations,
                   "Replacements:" = self$replacements,
                   "Upweighting probability:" = self$prob_up,
                   "Downweighting probability:" = self$prob_down) %>%
        data.frame()
      names(options) <- NULL
      print(options)
    },
    #' @param set_only A logical indicating whether to instruct the command to
    #'   execute immediately (\code{TRUE}) or set the variables for future
    #'   execution (\code{FALSE}).
    queue = function (set_only = FALSE) {
      val_check <- check_logical(set_only, len = 1)
      if (!test_true(val_check)) {
        cli_abort("{.arg set_only} must be logical.",
                  "x" = val_check)
      }

      queue <- CommandQueue$new()
      cmd_flag <- ifelse(set_only, ":", "=")
      if (!set_only) {
        queue$add("mult", "= wagner replic 10")
      }

      ratchet_opts <- self %>%
        glue_data("iter {iterations} numsubs {replacements} upfactor {prob_up} downfact {prob_down}")

      queue$add("ratchet", glue("{cmd_flag} {ratchet_opts}"))
      return(queue)
    }
  )
)
