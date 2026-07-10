#' Branch Support Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures the calculation of branch
#' (Bremer) support values in \pkg{nitro}.
#'
#' `BranchSupportCommand` implements the suboptimal sampling approach
#' described by Goloboff et al. (2008): starting from an optimal-length
#' tree, branch swapping is performed repeatedly while progressively
#' increasing the number of suboptimal steps accepted, filling the tree
#' buffer at each step. Once the suboptimal sampling loop is complete,
#' the buffer is flushed via [KeepTreesCommand] and the optimal trees are
#' reloaded from a [ReadTreesCommand]. TNT then calculates the minimum
#' score difference required to lose each group with the `bsupport`
#' command.
#'
#' @details
#' ## Dependencies
#' `BranchSupportCommand` requires three dependencies to be resolved
#' before the command can be enqueued:
#' * `"starting trees"` (required) — a [ReadTreesCommand] object
#'   providing the optimal trees to reload into the buffer after
#'   suboptimal sampling is complete. Supplied automatically by
#'   [execute_analysis()] via the `starting_trees` argument.
#' * `"tree search"` (required) — a [BranchBreakingCommand] object
#'   defining the branch swapping strategy used during suboptimal tree
#'   sampling. The command is automatically set to `set_only = TRUE` so
#'   that it configures the search without executing it immediately.
#' * `"tree buffer"` (required) — a [TreeBufferCommand] object defining
#'   the maximum number of trees to retain. The buffer size is
#'   redistributed proportionally across the suboptimal steps during
#'   enqueuing.
#'
#' All three dependencies are resolved automatically when the command is
#' attached to a [TreeAnalysis] via [set_support()] and the analysis is
#' run via [execute_analysis()].
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `503`. When `$method` is `"suboptimal_sampling"`, the enqueue method
#' also adds:
#' * A staged sequence of [SuboptimalCommand], [TreeBufferCommand], and
#'   [BranchBreakingCommand] entries at priority `502` — one round per
#'   value in `$suboptimal_steps` — with the buffer size redistributed
#'   proportionally across steps.
#' * A [KeepTreesCommand] at priority `504` that flushes the buffer after
#'   the suboptimal loop completes.
#' * A [ReadTreesCommand] at priority `505` that reloads the optimal
#'   trees, ensuring that `bsupport` compares against the correct set of
#'   trees.
#'
#' ## Command output
#' `$render()` produces a `bsupport` command of the form:
#' * Absolute index: `bsupport =<group_collapse_value>;`
#' * Relative index: `bsupport [=<group_collapse_value>;`
#'
#' ## Default values
#' | Parameter              | Default                 |
#' |------------------------|-------------------------|
#' | `method`               | `"suboptimal_sampling"` |
#' | `index_type`           | `"absolute"`            |
#' | `group_collapse_value` | `1`                     |
#'
#' @seealso
#' * [set_support()] — recommended way to attach branch support
#'   calculation to a [TreeAnalysis].
#' * [new_support()] — create a support command object by name.
#' * [ResamplingCommand] — alternative resampling-based support methods
#'   (bootstrap, jackknife, symmetric resampling).
#' * [BranchBreakingCommand] — the tree search command required as a
#'   dependency.
#' * [SuboptimalCommand] — the command used to set the suboptimality
#'   threshold at each step.
#'
#' @references
#' Goloboff, P. A., Farris, J. S., & Nixon, K. C. (2008). TNT, a free
#'   program for phylogenetic analysis. *Cladistics*, 24(5), 774--786.
#'   \doi{10.1111/j.1096-0031.2008.00217.x}
#'
#' @importFrom checkmate check_choice check_int check_integer check_null
#'   check_number test_null test_true
#' @importFrom cli cli_abort cli_text col_grey style_italic col_red
#' @importFrom magrittr divide_by multiply_by
#' @importFrom R6 R6Class
#' @export
BranchSupportCommand <- R6Class(
  "BranchSupportCommand",
  inherit = AbstractGroupSupportCommand,
  private = list(
    methods = NULL,
    index_types = NULL,
    .method = NULL,
    .suboptimal_steps = NULL
  ),
  active = list(
    #' @field method \[`character(1)`\]\cr
    #'   The method used to sample suboptimal trees for branch support
    #'   calculation. Partial matches are accepted. Currently the only
    #'   available option is `"suboptimal_sampling"`, which performs branch
    #'   swapping while progressively increasing the number of suboptimal
    #'   steps accepted according to the schedule defined by
    #'   `$suboptimal_steps`. See the Details section for a description of
    #'   the staged sampling approach.
    method = function(value) {
      if (missing(value)) {
        return(private$.method)
      } else {
        value <- private$methods[pmatch(value, private$methods)]
        val_check <- check_choice(value, private$methods)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg method} must be a valid choice.",
            "x" = val_check
          ))
        }

        private$.method <- value
      }
    },
    #' @field suboptimal_steps \[`integer`\]\cr
    #'   A sorted integer vector defining the suboptimality schedule used
    #'   during tree sampling. Each value specifies the absolute number of
    #'   extra steps above the optimal tree length that will be accepted
    #'   during one round of branch swapping. Values must be positive,
    #'   unique, and supplied in increasing order. The tree buffer is
    #'   redistributed proportionally across steps during enqueuing, with
    #'   larger buffer allocations assigned to later (more suboptimal)
    #'   steps. Only applicable when `$method` is `"suboptimal_sampling"`.
    #'   Must not be `NULL` when `$method` is `"suboptimal_sampling"`.
    suboptimal_steps = function(value) {
      if (missing(value)) {
        return(private$.suboptimal_steps)
      } else {
        val_check <- check_integer(value, lower = 1, any.missing = FALSE, min.len = 1, sorted = TRUE)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg suboptimal_steps} must be a vector of integers.",
            "x" = val_check
          ))
        }

        private$.suboptimal_steps <- value
      }
    },
    #' @field index_type \[`character(1)`\]\cr
    #'   The type of branch support index to calculate. Partial matches are
    #'   accepted. Options are:
    #'   * `"absolute"` (default) — the absolute number of extra steps
    #'     required to lose the group.
    #'   * `"relative"` — the support expressed relative to the length of
    #'     the optimal tree.
    index_type = function(value) {
      label <- "index_type"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        value <- private$index_types[pmatch(value, private$index_types)]
        val_check <- check_choice(value, private$index_types)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be a valid choice.",
            "x" = val_check
          ))
        }

        self$set_argument_value(label, value)
      }
    },
    #' @field group_collapse_value \[`integer(1)`\]\cr
    #'   The support value below which groups in the recovered trees will
    #'   be collapsed. Can be zero or a negative integer, which is useful
    #'   when all groups should be retained regardless of support. Defaults
    #'   to `1`, which collapses groups with no positive support.
    group_collapse_value = function(value) {
      label <- "group_collapse_value"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be an integer.",
            "x" = val_check
          ))
        }

        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command to a [CommandQueue].
    #'
    #' Adds this command at priority `503`. When `$method` is
    #' `"suboptimal_sampling"`, also enqueues:
    #' * A staged sequence of [SuboptimalCommand], [TreeBufferCommand],
    #'   and [BranchBreakingCommand] entries at priority `502`, one round
    #'   per value in `$suboptimal_steps`, with the tree buffer size
    #'   redistributed proportionally across steps.
    #' * A [KeepTreesCommand] at priority `504` that flushes the buffer
    #'   after the suboptimal loop.
    #' * A [ReadTreesCommand] at priority `505` that reloads the optimal
    #'   trees before `bsupport` runs.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(self, 503)

      if (self$method == "suboptimal_sampling") {
        search_cmd <- self$get_dependency("tree search")
        buffer_cmd <- self$get_dependency("tree buffer")

        buf_size <- buffer_cmd$size

        subopt_steps <- self$suboptimal_steps
        buf_steps <- subopt_steps %>%
          divide_by(max(.)) %>%
          multiply_by(buf_size) %>%
          sapply(floor)

        if (search_cmd$set_only == TRUE) {
          search_cmd$set_only <- FALSE
        }

        subopt_cmd <- SuboptimalCommand$new()

        for (idx in seq(buf_steps)) {
          subopt_cmd$absolute_threshold <- subopt_steps[idx]
          buffer_cmd$size <- buf_steps[idx]

          .queue$add(subopt_cmd, 502)
          if (idx > 1) {
            .queue$add(buffer_cmd, 502)
          }
          .queue$add(search_cmd, 502)

          subopt_cmd <- subopt_cmd$clone(deep = TRUE)
          buffer_cmd <- buffer_cmd$clone()
        }

        keep_cmd <- KeepTreesCommand$new(number = 0)
        .queue$add(keep_cmd, 504)

        tree_cmd <- self$get_dependency("starting trees")
        .queue$add(tree_cmd, 505)
      }

      .queue
    },
    #' @description
    #' Create a new `BranchSupportCommand` object.
    #'
    #' In most cases, [set_support()] is the recommended way to create and
    #' attach this command to a [TreeAnalysis].
    #'
    #' @param method \[`character(1)`\]\cr
    #'   The suboptimal tree sampling method (default:
    #'   `"suboptimal_sampling"`). See the `$method` field.
    #' @param suboptimal_steps \[`integer`\]\cr
    #'   The suboptimality schedule. Must not be `NULL` when `method` is
    #'   `"suboptimal_sampling"`. See the `$suboptimal_steps` field.
    #' @param index_type \[`character(1)`\]\cr
    #'   The type of support index to calculate (default: `"absolute"`).
    #'   See the `$index_type` field.
    #' @param group_collapse_value \[`integer(1)`\]\cr
    #'   The collapse threshold (default: `1`). See the
    #'   `$group_collapse_value` field.
    #'
    #' @return A new `BranchSupportCommand` object.
    initialize = function(method = "suboptimal_sampling",
                          index_type = "absolute", suboptimal_steps = NULL,
                          group_collapse_value = 1) {
      super$initialize(
        name = "bsupport",
        description = "Branch support"
      )

      private$methods <- c("suboptimal_sampling")
      private$index_types <- c("absolute", "relative")

      if (method == "suboptimal_sampling" & test_null(suboptimal_steps)) {
        cli_abort(c("{.arg suboptimal_steps} must not be {.val NULL} if {.arg method} is {.val suboptimal_sampling}.",
          "i" = "Set a value for {.arg suboptimal steps}"
        ))
      }

      index_type_cmd_fmt <- function(value) {
        ifelse(value == "relative", "[", NA)
      }
      self$new_argument(
        label = "index_type",
        description = "Resampling index",
        command_format = index_type_cmd_fmt,
        default_value = "absolute"
      )

      self$new_argument(
        label = "group_collapse_value",
        description = "Group collapse value",
        command_format = "{value}",
        default_value = 1
      )

      self$template <- c(
        "{index_type}",
        "={group_collapse_value}"
      )

      validate_topology <- function(value) {
        if (!test_null(value)) {
          val_check <- check_class(value, "ReadTreesCommand")

          if (!test_true(val_check)) {
            cli_abort(c("{.arg value} must be a {.cls ReadTreesCommand} object"))
          }
        }

        value
      }

      self$new_dependency("starting trees", TRUE, validate_topology)

      validate_tree_search <- function(value) {
        val_check <- check_class(value, "BranchBreakingCommand")

        if (!test_true(val_check)) {
          cli_abort(c("{.arg value} must be a {.cls BranchBreakingCommand} object.",
            "x" = val_check
          ))
        }

        value$set_only <- TRUE
        ts <- value$clone(deep = TRUE)

        value
      }

      self$new_dependency("tree search", TRUE, validate_tree_search)

      validate_buffer <- function(value) {
        val_check <- check_class(value, "TreeBufferCommand")

        if (!test_true(val_check)) {
          cli_abort(c("{.arg value} must be a {.cls TreeBufferCommand} object"))
        }

        value
      }

      self$new_dependency("tree buffer", TRUE, validate_buffer)

      arguments <- names(formals())

      for (argument in arguments) {
        self[[argument]] <- get(argument)
      }
    },
    #' @description
    #' Parse raw TNT output from the `bsupport` command.
    #'
    #' Extracts the number of trees used and the collapse cutoff from the
    #' legend written by TNT after branch support calculation, and returns
    #' a tidy tibble.
    #'
    #' @param output \[`character`\]\cr
    #'   A character vector of raw output lines from TNT.
    #'
    #' @return A [tibble][tibble::tibble] with columns:
    #'   * `method` \[`character`\] — always `"branch support"`.
    #'   * `trees` \[`numeric`\] — the number of trees used in the
    #'     calculation.
    #'   * `cutoff` \[`numeric`\] — the collapse cutoff applied.
    transform = function(output) {
      legend_re <- "Copied legends: \"Bremer supports \\(from (?<trees>[0-9]+) trees, cut (?<cutoff>[0-9]+)\\)"

      output <- str_match_all(output, legend_re) %>%
        Reduce(f = rbind) %>%
        extract(, -1) %>%
        t() %>%
        as_tibble() %>%
        mutate(
          method = "branch support",
          label = method,
          across(all_of(c("trees", "cutoff")), as.numeric)
        )

      output
    }
  )
)
