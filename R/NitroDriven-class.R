#' Set options for driven searches
#'
#' @description
#' \code{DrivenSearchOptions} is an R6 class that defines the set of parameters
#' required to perform a driven search in \code{nitro}.
#' @importFrom checkmate asInt assert check_class check_flag check_int
#'    check_class check_list check_null test_true
#' @importFrom cli cli_abort cli_text
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @export
DrivenSearchOptions <- R6Class("DrivenSearchOptions",
  private = list(
    .replications = NULL,
    .hits = NULL,
    .consense_times = NULL,
    .keep_all = NULL,
    .multiply = NULL,
    .sectorial_search = NULL,
    .tree_fusing = NULL,
    .tree_hybridizing = NULL,
    .tree_drifting = NULL,
    .ratchet = NULL),
  active = list(
    #' @field replications An integer value indicating the number of replications.
    replications = function (value) {
      if (missing(value)) {
        return(private$.replications)
      } else {
        val_check <- check_int(value, lower = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg replications} must be an integer.",
                      "x" = val_check))
        }

        private$.replications <- asInt(value)
      }
    },
    #' @field hits An integer value indicating the number of times the shortest
    #'   tree must be found on consecutive re-runs of the analysis before stopping.
    hits = function (value) {
      if (missing(value)) {
        return(private$.hits)
      } else {
        val_check <- check_int(value, lower = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg hits} must be an integer.",
                      "x" = val_check))
        }

        private$.hits <- asInt(value)
      }
    },
    #' @field consense_times An integer value indicating the number of times to
    #'   consense until the consensus is stablilised.
    consense_times = function (value) {
      if (missing(value)) {
        return(private$.consense_times)
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg consense_times} must be an integer",
                      "x" = val_check))
        }

        private$.consense_times <- asInt(value)
      }
    },
    #' @field keep_all A logical value indicating whether to retain all generated
    #'   trees from each replication regardless of length.
    keep_all = function (value) {
      if (missing(value)) {
        return(private$.keep_all)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg keep_all} must be a logical.",
                      "x" = val_check))
        }
        private$.keep_all <- value
      }
    },
    #' @field multiply A logical value indicating whether to find additional trees
    #'   by fusing suboptimal trees with optimal trees.
    multiply = function (value) {
      if (missing(value)) {
        return(private$.multiply)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg multiply} must be an integer.",
                      "x" = val_check))
        }
        private$.multiply <- value
      }
    },
    #' @field sectorial_search A list of objects of inheriting
    #'   \code{"\link{SectorialSearchOptions}"}.
    sectorial_search = function (value) {
      if (missing(value)) {
        return(private$.sectorial_search)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_class(value, c("SectorialSearchOptions", "R6")),
          check_list(value, types = "SectorialSearchOptions"),
          add = coll
        )

        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg sectorial_search} must be a valid (list of) objects.",
                      "x" = val_check))
        }

        if (is.null(value)) {
          value <- list(RandomSectorialSearchOptions$new(),
                        ConstrainedSectorialSearchOptions$new())
        }
        private$.sectorial_search <- value
      }
    },
    #' @field tree_fusing An object of class \code{"\link{TreeFusingOptions}"}.
    tree_fusing = function (value) {
      if (missing(value)) {
        return(private$.tree_fusing)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_class(value, c("TreeFusingOptions", "R6")),
          add = coll
        )

        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg tree_fusing} must be a valid object.",
                      "x" = val_check))
        }

        if (is.null(value)) {
          value <- TreeFusingOptions$new()
        }
        private$.tree_fusing <- value
        self
      }
    },
    #' @field tree_hybridizing An object of class \code{"\link{TreeHybridizingOptions}"}.
    tree_hybridizing = function (value) {
      if (missing(value)) {
        return(private$.tree_hybridizing)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_class(value, c("TreeHybridizingOptions", "R6")),
          add = coll
        )

        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg tree_hybridizing} must be a valid object.",
                      "x" = val_check))
        }

        if (is.null(value)) {
          value <- TreeHybridizingOptions$new(rounds = 0)
        }
        private$.tree_hybridizing <- value
      }
    },
    #' @field tree_drifting An object of class \code{"\link{TreeDriftingOptions}"}.
    tree_drifting = function (value) {
      if (missing(value)) {
        return(private$.tree_drifting)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_class(value, "TreeDriftingOptions"),
          add = coll
        )

        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg tree_drifting} must be a valid object.",
                      "x" = val_check))
        }

        if (is.null(value)) {
          value <- TreeDriftingOptions$new(iterations = 5)
        }
        private$.tree_drifting <- value
      }
    },
    #' @field ratchet An object of class \code{"\link{RatchetOptions}"}.
    ratchet = function (value) {
      if (missing(value)) {
        return(private$.ratchet)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_class(value, "RatchetOptions"),
          add = coll
        )

        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg ratchet} must be a valid object.",
                      "x" = val_check))
        }

        if (is.null(value)) {
          value <- RatchetOptions$new(iterations = 0)
        }
        private$.ratchet <- value
      }
    }
  ),
  public = list(
    #' @param replications an integer value indicating the number of replications.
    #' @param hits An integer value indicating the number of times the shortest
    #'   tree must be found on consecutive re-runs of the analysis before stopping.
    #' @param consense_times An integer value indicating the number of times to
    #'   consense until the consensus is stablilised.
    #' @param keep_all A logical value indicating whether to retain all generated
    #'   trees from each replication regardless of length. This has a different
    #'   meaning when \code{hits} = 1 and when \code{hits} > 1. When
    #'   \code{hits} = 1, it is trees from each of the RAS + TBR +  SS or DFT or
    #'   RAT, in addition to the trees resulting from fusing those. When
    #'   \code{hits} > 1, then it means the trees resulting from fusing the
    #'   initial starting trees for each of starting points.
    #' @param multiply A logical value indicating whether to find additional trees
    #'   by fusing suboptimal trees with optimal trees.
    #' @param sectorial_search A list of objects of inheriting
    #'   \code{"\link{NitroSectorialSearch}"}.
    #' @param tree_fusing An object of class \code{"\link{NitroTreeFusing}"}.
    #' @param tree_hybridizing An object of class \code{"\link{NitroTreeHybridize}"}.
    #' @param tree_drifting An object of class \code{"\link{NitroTreeDrifting}"}.
    #' @param ratchet An object of class \code{"\link{NitroRatchet}"}.
    initialize = function (replications = 4, hits = 1, consense_times = 0,
                           keep_all = FALSE, multiply = TRUE,
                           sectorial_search = NULL, tree_fusing = NULL,
                           tree_hybridizing = NULL, tree_drifting = NULL,
                           ratchet = NULL) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A TNT driven search configuration\")}")

      options <- c("Keep all trees:" = self$keep_all,
                   "Multiply trees by fusing:" = self$multiply) %>%
        ifelse("yes", "no")

      if (self$consense_times > 0) {
        options <- c("Consense times:" = self$consense_times, options)
      }

      options <- c("Iterations:" = self$replications,
                   "Substitutions:" = self$hits, options) %>%
        data.frame()

      colnames(options) <- NULL
      print(options)
    },
    #' @param ... Ignored.
    queue = function (...) {
      queue <- CommandQueue$new()

      if (length(self$sectorial_search)) {
        queue <- c(queue, sapply(self$sectorial_search, function (x) x$queue(set_only = TRUE)))
      }
      if (self$tree_fusing$rounds > 0) {
        queue <- c(queue, self$tree_fusing$queue())
      }
      if (self$tree_hybridizing$rounds > 0) {
        queue <- c(queue, self$tree_hybridizing$queue())
      }
      if (self$tree_drifting$iterations > 0) {
        queue <- c(queue, self$tree_drifting$queue(set_only = TRUE))
      }
      if (self$ratchet$iterations > 0) {
        queue <- c(queue, self$ratchet$queue(set_only = TRUE))
      }

      sect_classes <- sapply(self$sectorial_search, class)

      driven_cmd <- glue(
        "= hits {self$hits} replications {self$replications} {rss}rss {css}css {xss}xss {fuse} {hybrid}hybrid {drift} {ratchet} {consense} {keepall}keepall {multiply}multiply",
        rss = ifelse("RandomSectorialSearchOptions" %in% sect_classes, "", "no"),
        css = ifelse("ConstraintSectorialSearchOptions" %in% sect_classes, "", "no"),
        xss = ifelse("ExclusiveSectorialSearchOptions" %in% sect_classes, "", "no"),
        fuse = ifelse(self$tree_fusing$rounds == 0, "nofuse", glue("fuse {self$tree_fusing$rounds}")),
        hybrid = ifelse(self$tree_hybridizing$rounds == 0, "no", ""),
        drift = ifelse(self$tree_drifting$iterations == 0, "nodrift", glue("drift {self$tree_drifting$iterations}")),
        ratchet = ifelse(self$ratchet$iterations == 0, "noratchet", glue("ratchet {self$ratchet$iterations}")),
        consense = ifelse(self$consense_times == 0, "noconsense", glue("consense {self$consense_times}")),
        keepall = ifelse(self$keep_all, "", "no"),
        multiply = ifelse(self$multiply, "", "no")
      )

      queue$add("xmult", driven_cmd)
      return(queue)
    }
  )
)
