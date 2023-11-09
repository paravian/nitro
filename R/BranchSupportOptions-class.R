#' Set options for branch support calculation
#'
#' @description
#' \code{BranchSupportOptions} is an R6 class that defines options for calculating
#'   decay indices.
#' @importFrom checkmate check_int
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom R6 R6Class
#' @export
BranchSupportOptions <- R6Class("BranchSupportOptions",
  private = list(
    .index_type = NULL,
    .group_loss_cost = NULL,
    .group_collapse_value = NULL,
    .suboptimal_steps = NULL,
    .relative_suboptimal_fit = NULL
  ),
  active = list(
    #' @field index_type A character vector indicating the type of index to
    #'   calculate. Options are either \code{absolute} (default) or
    #'   \code{relative}.
    index_type = function (value) {
      if (missing(value)) {
        return(private$.index_type)
      } else {
        options <- c("absolute", "relative")
        value <- options[pmatch(value, options)]
        val_check <- check_choice(value, options)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg index_type} must be a valid choice.",
                      "x" = val_check))
        }

        if (value == "relative" & !test_null(self$relative_suboptimal_fit)) {
          val_check <- check_number(self$relative_suboptimal_fit, lower = 0, upper = 1)
          if (!isTRUE(val_check)) {
            cli_abort(c("{.arg relative_suboptimal_fit} must be a numeric when changing {.arg index_type} to {.val relative}.",
                        "x" = val_check))
          }
        }

        private$.index_type <- value
      }
    },
    #' @field group_loss_cost A character vector indicating what cost to use
    #'   for losing a group in a suboptimal tree.
    group_loss_cost = function (value) {
      if (missing(value)) {
        return(private$.group_loss_cost)
      } else {
        options <- c("standard", "score_difference")
        value <- options[pmatch(value, options)]
        val_check <- check_choice(value, options)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg group_loss_cost} must be a valid choice.",
                      "x" = val_check))
        }
        private$.group_loss_cost <- value
      }
    },
    #' @field suboptimal_steps An integer value indicating the maximum absolute
    #'   fit difference (i.e., length difference) of suboptimal trees to use
    #'   for calculating supports.
    suboptimal_steps = function (value) {
      if (missing(value)) {
        return(private$.suboptimal_steps)
      } else {
        val_check <- check_number(value, lower = 1)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg suboptimal_steps} must be a positive integer.",
                      "x" = val_check))
        }
        private$.suboptimal_steps <- value
      }
    },
    #' @field relative_suboptimal_fit A numeric value indicating the maximum
    #'   relative fit difference of suboptimal trees to use for calculating
    #'   supports.
    relative_suboptimal_fit = function (value) {
      if (missing(value)) {
        return(private$.relative_suboptimal_fit)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_number(value, lower = 0, upper = 1),
          add = coll
        )
        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg relative_suboptimal_fit} must be {.val NULL} or a number.",
                      "x" = val_check))
        }

        if (self$index_type == "relative") {
          val_check <- check_number(value, lower = 0, upper = 1)
          if (!isTRUE(val_check)) {
            cli_abort(c("{.arg relative_suboptimal_fit} must be a numeric when changing {.arg index_type} to {.val relative}.",
                        "x" = val_check))
          }
        }

        private$.relative_suboptimal_fit <- value
      }
    },
    #' @field group_collapse_value An integer value indicating the support value
    #'   below which groups in the tree will be collapsed. Can be zero or a
    #'   negative number.
    group_collapse_value = function (value) {
      if (missing(value)) {
        return(private$.group_collapse_value)
      } else {
        val_check <- check_int(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg group_collapse_support} must be an integer.",
                      "x" = val_check))
        }
        
        private$.group_collapse_value <- value
      }
    }
  ),
  public = list(
    #' @param index_type A character vector indicating the type of index to
    #'   calculate. Options are either \code{absolute} (default) or
    #'   \code{relative}.
    #' @param group_loss_cost A character vector indicating what cost to use
    #'   for losing a group in a suboptimal tree.
    #' @param suboptimal_steps An integer value indicating the maximum length
    #'   of suboptimal trees to keep during branch swapping.
    #' @param group_collapse_value An integer value indicating the support value
    #'   below which groups in the tree will be collapsed. Can be zero or a
    #'   negative number.
    #' @param relative_suboptimal_fit A numeric value indicating the maximum
    #'   relative fit difference of suboptimal trees to use for calculating
    #'   supports.
    initialize = function (index_type = "absolute", group_loss_cost = "standard",
                           group_collapse_value = 1, suboptimal_steps = 10,
                           relative_suboptimal_fit = NULL) {
      a <- as.list(environment(), all = TRUE)

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A branch support configuration\")}")

      options <- c(
        "Index type:" = self$index_type,
        "Group loss cost:" = self$group_loss_cost,
        "Group collapse value:" = self$group_collapse_value,
        "Suboptimal steps:" = self$suboptimal_steps
      )

      if (self$index_type == "relative") {
        options <- c(options,
                     "Relative suboptimal fit:" = self$relative_suboptimal_fit)
      }

      options <- data.frame(options)

      names(options) <- NULL
      print(options)
    },
    #' @param ... Ignored.
    queue = function (...) {
      queue <- CommandQueue$new()

      subopt_arg <- glue(
        "{steps}{fit}",
        steps = self$suboptimal_steps,
        fit = ifelse(self$index_type == "relative", glue("x{self$relative_suboptimal_fit}"), ""))
      queue$add("subopt", subopt_arg)

      bs_args <- glue(
        "{index_type} ={group_collapse_value} !{group_loss_cost}0",
        group_collapse_value = self$group_collapse_value,
        group_loss_cost = ifelse(self$group_loss_cost == "score_difference", "+", ""),
        index_type = ifelse(self$index_type == "relative", "[", "")
      )

      queue$add("bsupport", bs_args)
      return(queue)
    }
  )
)
