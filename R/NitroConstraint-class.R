#' Define constrained tree search
#'
#' @description
#' \code{NitroConstraint} is an R6 class that defines a constrained tree
#' search.
#' @importFrom checkmate assertFALSE assertLogical assertTRUE checkLogical
#'   checkNull
#' @importFrom R6 R6Class
#' @export
NitroConstraint <- R6Class("NitroConstraint",
  private = list(
    .is_positive = NULL,
    .fixed_otus = NULL,
    .floating_otus = NULL
  ),
  active = list(
    #' @field is_positive A logical value indicating whether the constraint is
    #'   positive (\code{TRUE}) or negative (\code{FALSE}).
    is_positive = function (value) {
      if (missing(value)) {
        private$.is_positive
      } else {
        assertLogical(value, len = 1)
        private$.is_positive <- value
        self
      }
    },
    #' @field fixed_otus A logical vector indicating which OTUs from the matrix
    #'   to assign as a fixed constraint.
    fixed_otus = function (value) {
      if (missing(value)) {
        private$.fixed_otus
      } else {
        assert(
          checkNull(value),
          checkLogical(value, any.missing = FALSE)
        )
        if (!is.null(private$.floating_otus)) {
          assertLogical(private$.floating_otus, any.missing = FALSE,
                        len = length(value))
          assertFALSE(any(apply(data.frame(value, private$.floating_otus), 1, all)))
        }
        private$.fixed_otus <- value
        self
      }
    },
    #' @field floating_otus An optional logical vector indicating which OTUs from
    #'   the matrix to assign as floating constraints.
    floating_otus = function (value) {
      if (missing(value)) {
        private$.floating_otus
      } else {
        assert(
          checkNull(value),
          checkLogical(value, any.missing = FALSE)
        )
        if (!is.null(private$.fixed_otus)) {
          assertLogical(private$.fixed_otus, any.missing = FALSE,
                        len = length(value))
          assertFALSE(any(apply(data.frame(value, private$.fixed_otus), 1, all)))
        }
        private$.floating_otus <- value
        self
      }
    }
  ),
  public = list(
    #' @param is_positive A logical value indicating whether the constraint is
    #'   positive (\code{TRUE}) or negative (\code{FALSE}).
    #' @param fixed_otus A character vector indicating which OTUs from the matrix
    #'   to assign as a fixed constraint.
    #' @param floating_otus An character logical vector indicating which OTUs from
    #'   the matrix to assign as floating constraints.
    initialize = function (is_positive = TRUE, fixed_otus = NULL,
                           floating_otus = NULL) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cat("<NitroConstraint>\n")
      cat(paste("* Type:", ifelse(private$.is_positive, "positive", "negative"), "\n"))
      cat(paste("* Fixed OTUs:", sum(private$.fixed_otus), "\n"))
      if (any(private$.floating_otus)) {
        cat(paste("* Floating OTUs:", sum(private$.floating_otus), "\n"))
      }
    },
    #' @param ... Ignored.
    tnt_cmd = function (...) {
      fixed <- paste(which(private$.fixed_otus) - 1, collapse = " ")
      floating <- paste(which(private$.floating_otus) - 1, collapse = " ")
      if (nchar(floating) > 0) {
        floating <- paste(" (", floating, ")")
      }
      type <- ifelse(private$.is_positive, " +", " -")
      return(paste(type, " [ ", fixed, floating, " ]", sep = ""))
    }
  )
)

