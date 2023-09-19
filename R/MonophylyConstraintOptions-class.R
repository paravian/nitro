#' Set constraints on monophyly
#'
#' @description
#' \code{MonophylyConstraintOptions} is an R6 class that sets options for tree
#'   searches with constraints on monophyly.
#' @importFrom checkmate assert check_character check_disjunct check_flag
#'   check_null check_character makeAssertCollection test_true
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @export
MonophylyConstraintOptions <- R6Class("MonophylyConstraintOptions",
  inherit = ConstraintBaseOptions,
  private = list(
    .fixed_otus = NULL,
    .floating_otus = NULL
  ),
  active = list(
    #' @field fixed_otus A logical vector indicating which OTUs from the matrix
    #'   to assign as a fixed constraint.
    fixed_otus = function (value) {
      if (missing(value)) {
        return(private$.fixed_otus)
      } else {
        val_check <- check_character(value, min.chars = 1, any.missing = FALSE, min.len = 2, unique = TRUE)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg fixed_otus} must be a valid character vector.",
                      "x" = val_check))
        }

        if (!is.null(self$floating_otus)) {
          val_check <- check_disjunct(value, self$floating_otus)
          if (!test_true(val_check)) {
            cli_abort(c("{.arg fixed_otus} must not contain taxa from {.arg floating_otus}.",
                        "x" = val_check))
          }
        }

        private$.fixed_otus <- value
      }
    },
    #' @field floating_otus An optional logical vector indicating which OTUs from
    #'   the matrix to assign as floating constraints.
    floating_otus = function (value) {
      if (missing(value)) {
        return(private$.floating_otus)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_character(value, min.chars = 1, any.missing = FALSE, min.len = 1, unique = TRUE),
          add = coll
        )

        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg floating_otus} must be either a valid character vector or {.val NULL}.",
                      "x" = val_check))
        }

        if (!is.null(self$fixed_otus)) {
          val_check <- check_disjunct(value, self$fixed_otus)
          if (!test_true(val_check)) {
            val_check <- str_replace_all(val_check, "([\\{\\}])", "\\1\\1")
            cli_abort(c("{.arg floating_otus} must not contain taxa from {.arg fixed_otus}.",
                        "x" = val_check))
          }
        }

        private$.floating_otus <- value
      }
    }
  ),
  public = list(
    #' @param fixed_otus A character vector indicating which OTUs from the matrix
    #'   to assign as a fixed constraint.
    #' @param floating_otus An character logical vector indicating which OTUs from
    #'   the matrix to assign as floating constraints.
    #' @param is_positive A logical value indicating whether the constraint is
    #'   positive (\code{TRUE}) or negative (\code{FALSE}).
    initialize = function (fixed_otus, floating_otus = NULL,
                           is_positive = TRUE) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A monophyly constraint\")}")

      options <- c("Type:" = ifelse(self$is_positive, "positive", "negative"),
                   "Fixed OTUs:" = length(self$fixed_otus))
      if (!is.null(self$floating_otus)) {
        options <- c(options, "Floating OTUs:" = length(self$floating_otus))
      }
      options <- data.frame(c(options))
      names(options) <- NULL

      print(options)
    },
    #' @param ... Ignored.
    queue = function (...) {
      queue <- CommandQueue$new()
      force_arg <- paste(self$fixed_otus, collapse = " ")

      if (!is.null(self$floating_otus)) {
        force_arg <- glue("[ {force_arg} ({floating}) ]", floating = paste(self$floating_otus, collapse = " "))
      }

      type <- ifelse(self$is_positive, "+", "-")
      force_arg <- glue("{type} {force_arg}")
      queue$add("force", force_arg)
      return(queue)
    }
  )
)

