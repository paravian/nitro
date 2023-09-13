#' Set options for branch breaking
#'
#' @description
#' \code{BranchBreakingOptions} is an R6 class that defines the set of optiopns
#' required to perform branch breaking.
#' @importFrom checkmate asInt check_choice check_flag check_int test_true
#' @importFrom cli cli_abort cli_text
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @export
BranchBreakingOptions <- R6Class("BranchBreakingOptions",
  private = list(
    .swapper = NULL,
    .cluster_size = NULL,
    .safe_unclip = NULL,
    .fill_only = NULL,
    .save_multiple = NULL,
    .random_clip = NULL
  ),
  active = list(
    #' @field swapper A character vector indicating the branch swapping algorithm
    #'   to use. Options are either '\code{tbr}' (the default) or '\code{spr}'.
    swapper = function (value) {
      if (missing(value)) {
        return(private$.swapper)
      } else {
        val_check <- check_choice(value, c("spr", "tbr"))
        if (!test_true(val_check)) {
          cli_abort(c("{.arg swapper} must be a valid option.",
                      "x" = val_check))
        }
        private$.swapper <- value
      }
    },
    #' @field cluster_size An integer value indicating the number of nodes
    #'   (clusters) to use during swapping. Applied only when \code{swapper} is
    #'   set to \code{tbr}. Larger clusters will result in faster swapping as
    #'   matrix size increases. If left as 0 (the default), cluster size will be
    #'   automatically determined.
    cluster_size = function (value) {
      if (missing(value)) {
        return(private$.cluster_size)
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg cluster_size} must be an integer.",
                      "x" = "val_check"))
        }
        value <- asInt(value)
        private$.cluster_size <- value
      }
    },
    #' @field safe_unclip A logical value indicating whether to use a safer but
    #'   slower method for updating buffers when clipping to find a better tree.
    #'   Applied only when \code{swapper} is set to \code{tbr}.
    safe_unclip = function (value) {
      if (missing(value)) {
        return(private$.safe_unclip)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg safe_unclip} must be a logical.",
                      "x" = val_check))
        }
        private$.safe_unclip <- value
      }
    },
    #' @field fill_only A logical value indicating whether to stop swapping when
    #'   the tree buffer is full.
    fill_only = function (value) {
      if (missing(value)) {
        return(private$.fill_only)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg fill_only} must be a logical.",
                      "x" = val_check))
        }
        private$.fill_only <- value
      }
    },
    #' @field save_multiple A logical value indicating whether to save multiple
    #'   trees during swapping.
    save_multiple = function (value) {
      if (missing(value)) {
        return(private$.save_multiple)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg save_multiple} must be a logical.",
                      "x" = val_check))
        }
        private$.save_multiple <- value
      }
    },
    #' @field random_clip A logical value indicating whether to randomize the
    #'   tree clipping sequence.
    random_clip = function (value) {
      if (missing(value)) {
        return(private$.random_clip)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg random_clip} must be a logical.",
                      "x" = val_check))
        }
        private$.random_clip <- value
      }
    }
  ),
  public = list(
    #' @param swapper A character vector indicating the branch swapping algorithm
    #'   to use. Options are either '\code{tbr}' (the default) or '\code{spr}'.
    #' @param cluster_size An integer value indicating the number of nodes
    #'   (clusters) to use during swapping. Applied only when \code{swapper} is
    #'   set to \code{tbr}. Larger clusters will result in faster swapping as
    #'   matrix size increases. If left as 0 (the default), cluster size will be
    #'   automatically determined.
    #' @param safe_unclip A logical value indicating whether to use a safer but
    #'   slower method for updating buffers when clipping to find a better tree.
    #'   Applied only when \code{swapper} is set to \code{tbr}.
    #' @param fill_only A logical value indicating whether to stop swapping when
    #'   the tree buffer is full.
    #' @param save_multiple A logical value indicating whether to save multiple
    #'   trees during swapping.
    #' @param random_clip A logical value indicating whether to randomize the
    #'   tree clipping sequence.
    initialize = function (swapper = "tbr", cluster_size = 0,
                           safe_unclip = FALSE, fill_only = FALSE,
                           save_multiple = TRUE, random_clip = FALSE) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A TNT branch breaking configuration\")}")

      options <- c("Swapper type:" = toupper(private$.swapper))
      if (private$.swapper == "tbr") {
        options <- c(
          options,
          "Number of nodes to swap:" = ifelse(private$.cluster_size, private$.cluster_size, "auto"))
      }
      options <- c(
        options,
        "Use safe unclipping:" = ifelse(private$.safe_unclip, "yes", "no"),
        "Stop when tree buffer full:" = ifelse(private$.fill_only, "yes", "no"),
        "Save multiple trees:" = ifelse(private$.save_multiple, "yes", "no"),
        "Randomize node clips:" = ifelse(private$.random_clip, "yes", "no")
      ) %>%
        data.frame()

      names(options) <- NULL
      print(options)
    },
    #' @param ... Ignored.
    queue = function (...) {
      queue <- CommandQueue$new()

      bbreak_cmd <- c()
      if (private$.cluster_size > 0) {
        bb_clust_args <- glue("clusters {self$cluster_size}")
        queue$add("bbreak", bb_clust_args)
      }

      bbreak_args <- glue("= {self$swapper} {s}safe {f}fillonly {m}mulpars {r}randclip",
                          s = ifelse(self$safe_unclip, "", "no"),
                          f = ifelse(self$fill_only, "", "no"),
                          m = ifelse(self$save_multiple, "", "no"),
                          r = ifelse(self$random_clip, "", "no"))
      queue$add("bbreak", bbreak_args)
      return(queue)
    }
  )
)
