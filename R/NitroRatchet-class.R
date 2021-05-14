#' Define a parsimony ratchet analysis
#'
#' \code{NitroRatchet} is an R6 class that defines the set of parameters
#' required to perform a parsimony ratchet phylogenetic analysis in
#' \code{nitro}.
#' @importFrom checkmate asInt assertInt
#' @importFrom R6 R6Class
#' @export
NitroRatchet <- R6Class("NitroRatchet",
  inherit = NitroMethodsBase,
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
        private$.iterations
      } else {
        assertInt(value, lower = 0)
        private$.iterations <- asInt(value)
      }
    },
    #' @field replacements An integer value indicating the number of
    #'   replacements (i.e., accepted tree rearrangements) to perform in each
    #'   perturbation phase.
    replacements = function (value) {
      if (missing(value)) {
        private$.replacements
      } else {
        assertInt(value, lower = 1)
        private$.replacements <- asInt(value)
      }
    },
    #' @field prob_up An integer value indicating the probability of
    #'   upweighting a character.
    prob_up = function (value) {
      if (missing(value)) {
        private$.prob_up
      } else {
        assertInt(value, lower = 1)
        private$.prob_up <- asInt(value)
      }
    },
    #' @field prob_down An integer value indicating the probability of
    #'   downweighting a character.
    prob_down = function (value) {
      if (missing(value)) {
        private$.prob_down
      } else {
        assertInt(value, lower = 1)
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
      cat("<NitroRatchet>\n")
      cat(paste("* Iterations:", private$.iterations, "\n"))
      cat(paste("* Replacements:", private$.replacements, "\n"))
      cat(paste("* Upweighting probability:", private$.prob_up, "\n"))
      cat(paste("* Downweighting probability:", private$.prob_down, "\n"))
    },
    #' @param set_only A logical indicating whether to instruct the command to
    #'   execute immediately (\code{TRUE}) or set the variables for future
    #'   execution (\code{FALSE}).
    tnt_cmd = function (set_only = FALSE) {
      assertLogical(set_only, len = 1)
      ratchet_cmd <- c()
      cmd_flag <- ifelse(set_only, ":", "=")
      if (!set_only) {
        ratchet_cmd <- c("mult= wagner replic 10;")
      }
      ratchet_cmd <- c(ratchet_cmd,
                       paste0("ratchet", cmd_flag, " iter ", private$.iterations,
                              " numsubs ", private$.replacements,
                              " upfactor ", private$.prob_up,
                              " downfact ", private$.prob_down, ";", sep = ""))
      return(ratchet_cmd)
    }
  )
)
