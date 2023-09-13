#' Set options common to resampling analyses
#'
#' @description
#' \code{ResampleBaseOptions} is an R6 class that defines options common to
#'   resampling analyses.
#' @importFrom checkmate asInt check_character check_int check_multi_class
#'   check_subset test_true
#' @importFrom cli cli_abort cli_text
#' @importFrom R6 R6Class
ResampleBaseOptions <- R6Class("ResampleBaseOptions",
  private = list(
    .search_method = NULL,
    .replications = NULL,
    .cutoff = NULL,
    .frequency_summary = NULL
  ),
  active = list(
    #' @field search_method A valid tree search configuration.
    search_method = function (value) {
      if (missing(value)) {
        return(private$.search_method)
      } else {
        val_check <- check_multi_class(value, c("BranchSwappingOptions", "ConstraintSectorialSearchOptions", "DrivenSearchOptions", "RandomSectorialSearchOptions", "RatchetOptions"))
        if (!test_true(val_check)) {
          cli_abort(c("{.arg search_method} must be valid tree search configuration.",
                      "x" = val_check))
        }
        private$.search_method <- value
      }
    },
    #' @field replications An integer value indicating the number of resampling
    #'   replications to perform.
    replications = function (value) {
      if (missing(value)) {
        return(private$.replications)
      } else {
        val_check <- check_int(value, lower = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg replications} must be an integer.",
                      "x" = val_check))
        }
        value <- asInt(value)
        private$.replications <- value
      }
    },
    #' @field cutoff An integer value indicating the cutoff value for
    #'   frequencies.
    cutoff = function (value) {
      if (missing(value)) {
        return(private$.cutoff)
      } else {
        val_check <- check_int(value, lower = 0, upper = 99)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg cutoff} must be a valid integer.",
                      "x" = val_check))
        }
        private$.cutoff <- asInt(value)
        self
      }
    },
    #' @field frequency_summary A character vector indicating which method(s) to
    #'   use to summarize supports. More than one option can be specified. The
    #'   options are:
    #' \itemize{
    #'   \item \code{difference}: frequency differences (i.e., group supported/contradicted), default;
    #'   \item \code{absolute}: absolute frequencies; or
    #'   \item \code{slope}: frequency slopes.
    #' }
    frequency_summary = function (value) {
      if (missing(value)) {
        return(private$.frequency_summary)
      } else {
        val_check <- check_character(value, min.chars = 1, min.len = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg frequency_summary} must be a character vector.",
                      "x" = val_check))
        }

        choices <- c("absolute", "difference", "slope")
        value <- pmatch(value, choices) %>%
          na.omit() %>%
          {choices[.]}

        val_check <- check_subset(value, choices)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg frequency_summary} must be a character vector of valid summary option.",
                      "x" = val_check))
        }
        private$.frequency_summary <- value
      }
    }
  ),
  public = list(
    #' @param search_method A valid tree search configuration.
    #' @param replications An integer value indicating the number of resampling
    #'   replications to perform.
    #' @param frequency_summary A character vector indicating which method(s) to
    #'   use to summarize supports. More than one option can be specified. The
    #'   options are:
    #' \itemize{
    #'   \item \code{difference}: frequency differences (i.e., group supported/contradicted), detault;
    #'   \item \code{absolute}: absolute frequencies; or
    #'   \item \code{slope}: frequency slopes.
    #' }
    initialize = function (search_method, replications = 100, frequency_summary = "absolute") {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    }
  )
)
