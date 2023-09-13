#' Set options for an exclusive sectorial search
#'
#' @description
#' \code{ExclusiveSectorialSearchOptions} is an R6 class that defines the set
#'   of options required to perform exclusive sectorial searching analyses in
#'   \code{nitro}.
#' @importFrom checkmate asInt assert_int check_int check_flag
#'   makeAssertCollection test_true
#' @importFrom cli cli_abort cli_text
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @export
ExclusiveSectorialSearchOptions <- R6Class("ExclusiveSectorialSearchOptions",
  inherit = SectorialSearchBaseOptions,
  private = list(
    .selections = NULL,
    .rounds = NULL
  ),
  active = list(
    #' @field selections An integer value indicating the number of exclusive (i.e.,
    #'   non-overlapping) sectors to subdivide the entire tree into.
    selections = function (value) {
      if (missing(value)) {
        return(private$.selections)
      } else {
        val_check <- check_int(value, lower = 2)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg selections} must be an integer.",
                      "x" = val_check))
        }

        private$.selections <- asInt(value)
      }
    },
    #' @field rounds An integer value indicating the number of times to repeat
    #'   exclusive sector selection and analysis.
    rounds = function (value) {
      if (missing(value)) {
        private$.rounds
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
    #' @param selections An integer value indicating the number of exclusive (i.e.,
    #'   non-overlapping) sectors to subdivide the entire tree into.
    #' @param rounds An integer value indicating the number of times to repeat
    #'   exclusive sector selection and analysis.
    #' @param buffer A logical value indicating whether to use an independent
    #'   memory buffer for analysis of sectors.
    #' @param slack An integer value indicating the percentage to increase the
    #'   available memory during searches.
    initialize = function (selections, rounds = 2, buffer = TRUE, slack = 0) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A TNT exclusive sectorial search configuration\")}")

      options <- c(
        "Selections:" = self$selections,
        "Rounds:" = self$rounds,
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

      xss_opts <- self %>%
        glue_data("{selections}+{rounds} slack {slack}")
      buffer <- paste(ifelse(private$.buffer, "", "no"), "xbuf", sep = "")
      xss_opts <- glue("{cmd_flag} xss {xss_opts} {buffer}")

      queue$add("sectsch", xss_opts)
      return(queue)
    }
  )
)
