#' Set options for a constrained sectorial search
#'
#' @description
#' \code{ConstrainedSectorialSearchOptions} is an R6 class that defines the set
#'   of options required to perform sectorial searching analyses in
#'   \code{nitro}.
#' @importFrom checkmate asInt assert_int check_int check_flag
#'   makeAssertCollection test_true
#' @importFrom cli cli_abort cli_text
#' @importFrom glue glue glue_data
#' @importFrom R6 R6Class
#' @export
ConstrainedSectorialSearchOptions <- R6Class("ConstrainedSectorialSearchOptions",
  inherit = SectorialSearchBaseOptions,
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
        return(private$.min_fork)
      } else {
        coll <- makeAssertCollection()
        assert_int(value, lower = 0, add = coll)
        if (!is.null(private$.min_fork)) {
          assert_int(value, upper = private$.max_fork, add = coll)
        }

        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg min_fork} must be a valid integer.",
                      "x" = val_check))
        }
        private$.min_fork <- asInt(value)
      }
    },
    #' @field max_fork An integer value indicating the maximum fork number to use
    #'   with constraint-based sectorial searches.
    max_fork = function (value) {
      if (missing(value)) {
        return(private$.max_fork)
      } else {
        coll <- makeAssertCollection()
        assert_int(value, lower = 0, add = coll)
        if (!is.null(private$.min_fork)) {
          assert_int(value, lower = private$.min_fork, add = coll)
        }

        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg max_fork} must be a valid integer.",
                      "x" = val_check))
        }

        private$.max_fork <- asInt(value)
      }
    },
    #' @field rounds An integer value indicating the number of times to cycle over
    #'   groups in constraint-based selections.
    rounds = function (value) {
      if (missing(value)) {
        return(private$.rounds)
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg rounds} must be an integer.",
                      "x" = val_check))
        }

        private$.rounds <- asInt(value)
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
      cli_text("{col_grey(\"# A TNT constraint sectorial search configuration\")}")

      options <- c("Rounds:" = self$rounds,
                   "Minimum fork size:" = self$min_fork,
                   "Maximum fork size:" = self$max_fork,
                   "Using independent buffer:" = ifelse(self$buffer, "yes", "no"),
                   "Percentage memory increase:" = self$slack) %>%
        data.frame()

      colnames(options) <- NULL
      print(options)
    },
    #' @param set_only A logical indicating whether to instruct the command to
    #'   execute immediately (\code{TRUE}) or set the variables for future
    #'   execution (\code{FALSE}, default).
    queue = function (set_only = FALSE) {
      val_check <- check_flag(set_only)
      if (!test_true(val_check)) {
        cli_abort(c("{.arg set_only} must be a logical.",
                    "x" = val_check))
      }

      queue <- CommandQueue$new()
      cmd_flag <- ifelse(set_only, ":", "=")

      css_opts <- self %>%
        glue_data("minfork {min_fork} maxfork {max_fork} rounds {rounds} slack {slack}")
      buffer <- paste(ifelse(private$.buffer, "", "no"), "xbuf", sep = "")
      css_opts <- glue("{cmd_flag} css {css_opts} {buffer}")

      queue$add("sectsch", css_opts)
      return(queue)
    }
  )
)
