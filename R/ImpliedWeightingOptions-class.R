#' Set options for implied weighting
#'
#' @description
#' \code{ImpliedWeightingOptions} is an R6 class that defines the set of options
#'   for using (extended) implied weighting with phylogenetic analyses in
#'   \code{nitro}.
#' @importFrom checkmate check_flag check_number test_true
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom R6 R6Class
#' @export
ImpliedWeightingOptions <- R6Class("ImpliedWeightingOptions",
  private = list(
    .k = NULL,
    .multi_k = NULL,
    .proportion = NULL,
    .max_ratio = NULL
  ),
  active = list(
    #' @field k A numeric value indicating the concavity constant to apply
    #'   during an implied weights analysis. When \code{multi_k} is
    #'   \code{TRUE}, \code{k} represents the concavity for a character with
    #'   no missing entries and serves as the basis for calculating per-site
    #'   concavity constants.
    k = function (value) {
      if (missing(value)) {
        return(private$.k)
      } else {
        val_check <- check_number(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg k} must be a number.",
                      "x" = val_check))
        }
        private$.k <- value
      }
    },
    #' @field multi_k A logical value indicating whether each character will be
    #'   given an independent concavity constant based on the value of \code{k}
    #'   during an implied weights analysis.
    multi_k = function (value) {
      if (missing(value)) {
        return(private$.multi_k)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg multi_k} must be a logical.",
                      "x" = val_check))
        }

        private$.multi_k <- value
      }
    },
    #' @field proportion A numeric value indicating the proportion of homoplasy
    #'   missing values are assumed to have under implied weighting. This
    #'   parameter is only valid when \code{multi_k} is \code{TRUE}. A
    #'   proportion of 0 is equivalent to standard implied weighting, with
    #'   \code{multi_k} set as \code{FALSE}.
    proportion =  function (value) {
      if (missing(value)) {
        return(private$.proportion)
      } else {
        val_check <- check_number(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg proportion} must be a number.",
                      "x" = val_check))
        }
        private$.proportion <- value
        self
      }
    },
    #' @field max_ratio A numeric value indicating the maximum acceptable ratio
    #'   between two k values when multiple concavity constants are used (i.e.,
    #'   when \code{multi_k} is \code{TRUE}). A maximum ratio of 1 is
    #'   equivalent to standard implied weighting, with \code{multi_k} set as
    #'   \code{FALSE}.
    max_ratio =  function (value) {
      if (missing(value)) {
        return(private$.max_ratio)
      } else {
        val_check <- check_number(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg max_ratio} must be a number.",
                      "x" = val_check))
        }
        private$.max_ratio <- value
      }
    }
  ),
  public = list(
    #' @param k A numeric value indicating the concavity constant to apply during
    #'   an implied weights analysis. When \code{multi_k} is \code{TRUE}, \code{k}
    #'   represents the concavity for a character with no missing entries and
    #'   serves as the basis for calculating per-site concavity constants.
    #' @param multi_k A logical value indicating whether each character will be
    #'   given an independent concavity constant based on the value of \code{k}
    #'   during an implied weights analysis.
    #' @param proportion A numeric value indicating the proportion of homoplasy
    #'   missing values are assumed to have under implied weighting. This parameter
    #'   is only valid when \code{multi_k} is \code{TRUE}. A proportion of 0 is
    #'   equivalent to standard implied weighting, with \code{multi_k} set as
    #'   \code{FALSE}.
    #' @param max_ratio A numeric value indicating the maximum acceptable ratio
    #'   between two k values when multiple concavity constants are used (i.e.,
    #'   when \code{multi_k} is \code{TRUE}). A maximum ratio of 1 is equivalent
    #'   to standard implied weighting, with \code{multi_k} set as \code{FALSE}.
    initialize = function (k = 3, multi_k = FALSE, proportion = 0.5,
                           max_ratio = 5) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A TNT implied weighting configuration\")}")

      options <- c("Concavity constant (k):" = self$k,
                   "Multiple constants:" = ifelse(self$multi_k, "yes", "no"))

      if (private$.multi_k) {
        options <- c(options, "Proportion:" = self$proportion,
                     "Maximum ratio:" = self$max_ratio)
      }

      options <- data.frame(options)

      colnames(options) <- NULL
      print(options)
    },
    #' @param ... Ignored.
    queue = function (...) {
      queue <- CommandQueue$new()
      queue$add("piwe", "=")
      if (private$.multi_k) {
        ext_iw_opts <- glue("( *{self$proportion} <{self$max_ratio} /{self$k}")
        queue$add("xpiwe", ext_iw_opts)
      }
      return(queue)
    }
  )
)
