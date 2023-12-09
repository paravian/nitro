#' Results of a tree analyses
#'
#' @description
#' \code{TreeAnalysisResults} is an R6 class that contains the results of a tree
#'   analysis in \code{nitro}.
#' @importFrom ape .compressTipLabel
#' @importFrom checkmate assert check_character check_class check_data_frame
#'   check_numeric check_null check_number makeAssertCollection
#' @importFrom dplyr full_join everything
#' @importFrom tidyr unnest
#' @importFrom treeio as_tibble
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @export
TreeAnalysisResults <- R6Class("TreeAnalysisResults",
  private = list(
    .queue = NULL,
    .trees = NULL
  ),
  active = list(
    #' @field trees The trees generated from the analysis of \code{queue}.
    trees = function (...) {
      if (length(list(...)) == 0) {
        return(private$.trees)
      } else {
        cli_abort(c("{.arg trees} is a read-only attribute."))
      }
    },
    #' @field queue The \code{CommandQueue} object and generated the results.
    queue = function (...) {
      if (length(list(...)) == 0) {
        return(private$.queue$clone())
      } else {
        cli_abort(c("{.arg queue} is a read-only attribute."))
      }
    },
    #' @field statistics The statistics associated with each tree in \code{trees}.
    statistics = function (...) {
      if (length(list(...)) == 0) {
        stat_df <- lapply(self$trees, function (x) x@info) %>%
          Reduce(f = rbind) %>%
          as_tibble() %>%
          unnest(cols = everything()) %>%
          bind_cols(tree = seq(nrow(.)), .)

        return(stat_df)
      } else {
        cli_abort(c("{.arg statistics} is a read-only attribute."))
      }
    }
  ),
  public = list(
    #' @param phy A \code{multiPhylo} object.
    #' @param queue A \code{"\link{CommandQueue}"} object.
    #' @param lengths A numeric vector of lengths for \code{trees}.
    #' @param adjusted_homoplasy_scores A numeric vector of adjusted homoplasy scores for \code{trees}.
    #' @param min_length A number indicating the minimum possible length for
    #'   \code{trees} given the active characters and taxa.
    #' @param max_length A number indicating the maximum possible length for
    #'   \code{trees} given the active characters and taxa.
    #' @param tags A data frame containing annotation variables for \code{trees}.
    initialize = function (phy, queue, lengths = NULL, adjusted_homoplasy_scores = NULL,
                           min_length = NULL, max_length = NULL, tags = NULL) {
      val_check <- check_class(phy, "multiPhylo")
      if (!isTRUE(val_check)) {
        cli_abort("{.arg phy} must be {.cls multiPhylo} object.",
                  "x" = val_check)
      }

      val_check <- check_class(queue, c("CommandQueue", "R6"))
      if (!isTRUE(val_check)) {
        cli_abort("{.arg queue} must be a {.cls CommandQueue} object.",
                  "x" = val_check)
      }

      if (!is.null(lengths)) {
        val_check <- check_numeric(lengths, lower = 0, len = length(phy), any.missing = FALSE)
        if (!isTRUE(val_check)) {
          cli_abort("{.arg lengths} must be a numeric vector of correct length.",
                    "x" = val_check)
        }
      }

      if (!is.null(adjusted_homoplasy_scores)) {
        val_check <- check_numeric(adjusted_homoplasy_scores, lower = 0, len = length(phy), any.missing = FALSE)
        if (!isTRUE(val_check)) {
          cli_abort("{.arg adjusted_homoplasy_scores} must be a numeric vector of correct length.",
                    "x" = val_check)
        }
      }

      coll <- makeAssertCollection()
      assert(
        check_null(tags),
        check_data_frame(tags, min.cols = 2, all.missing = FALSE),
        add = coll
      )

      val_check <- coll$getMessages()
      if (!coll$isEmpty()) {
        cli_abort("{.arg tags} must be either a {.cls data.frame} or {.val NULL}.",
                  "x" = val_check)
      }

      minmax_length <- c(min_length, max_length)
      for (arg in minmax_length) {
        assert(
          check_null(arg),
          check_number(arg, lower = 0),
          combine = "or", add = coll
        )
      }

      var_check <- coll$getMessages()
      if (!coll$isEmpty()) {
        cli_abort(c("{.arg min_length} and {.arg max_length} must be positive numbers or {.val NULL}.",
                    "x" = val_check))
      }

      coll_inner <- makeAssertCollection()
      assert(
        assert(
          check_null(min_length),
          check_null(max_length),
          combine = "and", add = coll_inner
        ),
        assert(
          check_number(min_length, lower = 0),
          check_number(max_length, lower = 0),
          combine = "and", add = coll_inner
        ),
        add = coll
      )

      var_check <- coll$getMessages()
      if (!coll$isEmpty()) {
        cli_abort(c("{.arg min_length} and {.arg max_length} must be both positive numbers or {.val NULL}.",
                    "x" = val_check))
      }

      tree_data <- data.frame(matrix(nrow = length(phy), ncol = 0))

      if (!is.null(min_length)) {
        tree_data$lengths <- lengths
        if (!is.null(lengths) & !is.null(min_length)) {
          tree_data$CI <- min_length / lengths
          tree_data$RI <- (max_length - lengths) / (max_length - min_length)
          tree_data$RC <- tree_data$CI * tree_data$RI
        }
        
        if (!is.null(adjusted_homoplasy_scores)) {
          tree_data$adjusted_homoplasy_scores <- adjusted_homoplasy_scores
        }
      }
      
      if (!is.null(tags)) {
        tags$node <- tags$node - 1
      }

      trees <- lapply(seq(nrow(tree_data)), function (row) {
        tree <- phy[[row]] %>%
          as_tibble()
        if (!is.null(tags)) {
          tree <- full_join(tree, tags, by = "node")
        }
        tree <- as.treedata(tree)
        result <- tree_data[row,]
        tree@info <- as.list(result)
        return(tree)
      })

      private$.queue <- queue
      private$.trees <- trees
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A tree analysis result\")}")

      options <- c(length(private$.trees)) %>%
        data.frame()

      rownames(options) <- c("Number of trees:")
      names(options) <- NULL

      print(options)
    }
  )
)


#' Convert results to \code{multiPhylo}
#'
#' @description
#' Converts a \code{"\link{TreeAnalysisResults}"} object generated by
#'   \code{"\link{TreeAnalysis}"} into a \code{multiPhylo} object.
#' @param x A \code{"\link{TreeAnalysisResults}"} object.
#' @importFrom ape .compressTipLabel
#' @importFrom checkmate check_class
#' @importFrom cli cli_abort
#' @importFrom magrittr %>%
#' @importFrom treeio as.phylo
#' @importFrom TreeTools as.multiPhylo
#' @export
as.multiPhylo.TreeAnalysisResults <- function (x) {
  val_check <- check_class(x, "TreeAnalysisResults")
  if (!isTRUE(val_check)) {
    cli_abort(c("{.arg x} must be a {.cls TreeAnalysisResults} object.",
                "x" = val_check))
  }

  phy <- lapply(x$trees, as.phylo) %>%
    .compressTipLabel()
  return(phy)
}
