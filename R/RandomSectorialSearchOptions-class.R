#' Set options for a random sectorial search
#'
#' @description
#' \code{RandomSectorialSearchOptions} is an R6 class that defines the set of
#'   options required to perform sectorial searching analyses in \code{nitro}.
#' @importFrom checkmate assert asInt assert_int check_int check_flag
#'   check_null makeAssertCollection
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @export
RandomSectorialSearchOptions <- R6Class("RandomSectorialSearchOptions",
  inherit = SectorialSearchBaseOptions,
  private = list(
    .min_size = NULL,
    .max_size = NULL,
    .selection_factor = NULL,
    .increase = NULL,
    .small_starts = NULL
  ),
  active = list(
    #' @field min_size An integer value indicating the minimum size of random
    #'   selections. If \code{NULL} (the default), this value will be
    #'   automatically set according to the number of taxa in the matrix when
    #'   used with a \code{"\link{TreeAnalysis}"} object.
    min_size = function (value) {
      if (missing(value)) {
        if (is.null(private$.min_size)) {
          return(0)
        }
        return(private$.min_size)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_int(value, lower = 5),
          add = coll
        )

        if (!is.null(private$.max_size)) {
          assert_int(value, upper = private$.max_size, add = coll)
        }

        val_check <- coll$getMessages()
        if (!isTRUE(coll$isEmpty())) {
          cli_abort(c("{.arg min_size} must be a valid integer",
                      "x" = val_check))
        }

        # value <- asInt(value)
        private$.min_size <- value
      }
    },
    #' @field max_size An integer value indicating the maximum size of random
    #'   selections. If \code{NULL} (the default), this value will be
    #'   automatically set according to the number of taxa in the matrix when a
    #'   new \code{"\link{TreeAnalysis}"} object is initialized.
    max_size = function (value) {
      if (missing(value)) {
        if (is.null(private$.max_size)) {
          return(0)
        }
        return(private$.max_size)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_int(value, lower = 5),
          add = coll
        )

        if (!is.null(private$.min_size)) {
          assert_int(value, upper = private$.max_size, add = coll)
        }

        val_check <- coll$getMessages()
        if (!isTRUE(coll$isEmpty())) {
          cli_abort(c("{.arg max_size} must be a valid integer",
                      "x" = val_check))
        }

        # value <- asInt(value)
        private$.max_size <- value
      }
    },
    #' @field selection_factor An integer value indicating the maximum number
    #'   of random selections for the currently active taxa.
    selection_factor = function (value) {
      if (missing(value)) {
        return(private$.selection_factor)
      } else {
        val_check <- check_int(value, lower = 0)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg selection_factor} must be an integer",
                      "x" = val_check))
        }
        private$.selection_factor <- asInt(value)
      }
    },
    #' @field increase An integer value indicating the factor to increase the
    #'   size of random selections if enough selection of the current size have
    #'   been completed.
    increase = function (value) {
      if (missing(value)) {
        private$.increase
      } else {
        val_check <- check_int(value, lower = 0)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg increase} must be an integer",
                      "x" = val_check))
        }
        private$.increase <- asInt(value)
      }
    },
    #' @field small_starts An integer value indicating the number of random
    #'   addition sequence plus tree-bisection reconnection swaps to perform
    #'   for random selections below \code{min_size}.
    small_starts = function (value) {
      if (missing(value)) {
        private$.small_starts
      } else {
        val_check <- check_int(value, lower = 1)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg small_starts} must be an integer",
                      "x" = val_check))
        }
        private$.small_starts <- asInt(value)
      }
    }
  ),
  public = list(
    #' @param min_size An integer value indicating the minimum size of random
    #'   selections. If left as 0 (the default), this value will be
    #'   automatically set according to the number of taxa in the matrix when a
    #'   new \code{"\link{TreeAnalysis}"} object is initialized.
    #' @param max_size An integer value indicating the maximum size of random
    #'   selections. If left at 0 (the default), this value will be
    #'   automatically set according to the number of taxa in the matrix when a
    #'   new \code{"\link{TreeAnalysis}"} object is initialized.
    #' @param selection_factor An integer value indicating the maximum number
    #'   of random selections for the currently active taxa.
    #' @param increase An integer value indicating the factor to increase the
    #'   size of random selections if enough selection of the current size have
    #'   been completed.
    #' @param small_starts An integer value indicating the number of random
    #'   addition sequence plus tree-bisection reconnection swaps to perform
    #'   for random selections below \code{min_size}.
    #' @param buffer A logical value indicating whether to use an independent
    #    memory buffer for analysis of sectors.
    #' @param slack An integer value indicating the percentage to increase the
    #'   available memory during searches.
    initialize = function (min_size = NULL, max_size = NULL, selection_factor = 50,
                           increase = 100, small_starts = 3, buffer = TRUE,
                           slack = 0) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A TNT random sectorial search configuration\")}")

      options <- data.frame(
        c(self$min_size, self$max_size, self$selection_factor, self$increase, ifelse(self$buffer, "yes", "no"), self$slack)
      )
      rownames(options) <- c("Minimum selection size:", "Maximum selection size:", "Maximum random selections:", "Selection increase factor:", "Using independent buffer:", "Percentage memory increase:")

      colnames(options) <- NULL
      print(options)
    },
    #' @param set_only A logical indicating whether to instruct the command to
    #'   execute immediately (\code{TRUE}) or set the variables for future
    #'   execution \code{FALSE}.
    queue = function (set_only = FALSE) {
      val_check <- check_flag(set_only)
      if (!isTRUE(val_check)) {
        cli_abort(c("{.arg set_only} must be a logical.",
                    "x" = val_check))
      }

      queue <- CommandQueue$new()
      cmd_flag <- ifelse(set_only, ":", "=")

      css_opts <- self %>%
        glue_data("minsize {min_size} maxsize {max_size} selfact {selection_factor} increase {increase} starts {small_starts} slack {slack}")
      buffer <- paste(ifelse(private$.buffer, "", "no"), "xbuf", sep = "")
      css_opts <- glue("{cmd_flag} rss {css_opts} {buffer}")

      queue$add("sectsch", css_opts)
      return(queue)
    }
  )
)
