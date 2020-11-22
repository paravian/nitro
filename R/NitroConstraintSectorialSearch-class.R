#' Define a constraint sectorial search analysis
#'
#' @description
#' \code{NitroConstraintSectorialSearch} is an R6 class that defines the set of
#' parameters required to perform sectorial searching analyses in
#' \code{nitro}.
#' @importFrom checkmate asInt assertInt assertTRUE
#' @importFrom R6 R6Class
#' @export
NitroConstraintSectorialSearch <- R6Class("NitroConstraintSectorialSearch",
  inherit = NitroSectorialSearch,
  private = list(
    .min_fork = NULL,
    .max_fork = NULL,
    .rounds = NULL
  ),
  active = list(
    #' @field min_fork An integer value indicating the minimum fork number to use
    #'   with constraint-based sectorial searches.
    min_fork = function (value) {
      if (missing(value)) {
        private$.min_fork
      } else {
        assertInt(value, lower = 0)
        if (!is.null(private$.max_fork)) {
          assertTRUE(value <= private$.max_fork)
        }
        private$.min_fork <- asInt(value)
        self
      }
    },
    #' @field max_fork An integer value indicating the maximum fork number to use
    #'   with constraint-based sectorial searches.
    max_fork = function (value) {
      if (missing(value)) {
        private$.max_fork
      } else {
        assertInt(value, lower = 0)
        if (!is.null(private$.min_fork)) {
          assertTRUE(value >= private$.min_fork)
        }
        private$.max_fork <- asInt(value)
        self
      }
    },
    #' @field rounds An integer value indicating the number of times to cycle over
    #'   groups in constraint-based selections.
    rounds = function (value) {
      if (missing(value)) {
        private$.rounds
      } else {
        assertInt(value, lower = 0)
        private$.rounds <- asInt(value)
        self
      }
    }
  ),
  public = list(
    #' @param min_fork An integer value indicating the minimum fork number to use
    #'   with constraint-based sectorial searches.
    #' @param max_fork An integer value indicating the maximum fork number to use
    #'   with constraint-based sectorial searches.
    #' @param rounds An integer value indicating the number of times to cycle over
    #'   groups in constraint-based selections.
    #' @param buffer A logical value indicating whether to use an independent
    #'   memory buffer for analysis of sectors.
    #' @param slack An integer value indicating the percentage to increase the
    #'   available memory during searches.
    initialize = function (min_fork = 10, max_fork = 10, rounds = 3,
                           buffer = TRUE, slack = 0) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cat("<NitroConstraintSectorialSearch>\n")
      cat(paste("* Rounds:", private$.rounds, "\n"))
      cat(paste("* Minimum fork size:", private$.min_fork, "\n"))
      cat(paste("* Maximum fork size:", private$.max_fork, "\n"))
      cat(paste("* Use independent buffer:", private$.buffer, "\n"))
      cat(paste("* Percentage memory increase:", private$.slack, "\n"))
    },
    #' @param set_only A logical indicating whether to instruct the command to
    #'   execute immediately (\code{TRUE}) or set the variables for future
    #'   execution \code{FALSE}.
    tnt_cmd = function (set_only = FALSE) {
      assertLogical(set_only, len = 1)
      cmd_flag <- ifelse(set_only, ":", "=")
      sect_cmd <- c(" minfork ", private$.min_fork,
                    " maxfork ", private$.max_fork,
                    " rounds ", private$.rounds,
                    " slack ", private$.slack,
                    paste0(ifelse(private$.buffer, " ", " no"), "xbuf"))
      sect_cmd <- paste(c("sectsch", cmd_flag, " css ", sect_cmd, ";"), collapse = "", sep = "")
      sect_cmd
    }
  )
)
