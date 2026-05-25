#' Branch Breaking Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures a branch breaking (branch
#' swapping on existing trees) analysis in \pkg{nitro}.
#'
#' Branch breaking performs tree bisection-reconnection (TBR) or subtree
#' pruning-regrafting (SPR) swapping on trees already held in memory,
#' rather than building new trees from scratch. This wraps the TNT `bbreak`
#' command.
#'
#' @details
#' ## Swapping algorithms
#' * **TBR** (tree bisection-reconnection) — the default and more thorough
#'   algorithm. The `$cluster_size` and `$safe_unclip` options apply only
#'   to TBR.
#' * **SPR** (subtree pruning-regrafting) — faster but less thorough.
#'
#' ## Default values
#' | Parameter       | Default |
#' |-----------------|---------|
#' | `swapper`       | `"tbr"` |
#' | `cluster_size`  | `15`    |
#' | `safe_unclip`   | `FALSE` |
#' | `fill_only`     | `FALSE` |
#' | `save_multiple` | `TRUE`  |
#' | `random_clip`   | `FALSE` |
#' | `set_only`      | `FALSE` |
#'
#' ## Command output
#' `$render()` produces a string of the form
#' `bbreak= {swapper} clusters {cluster_size} [no]safe [no]fillonly ...;`
#'
#' ## Set-only mode and queue integration
#' When `$set_only` is `TRUE`, the command configures parameters without
#' triggering a search. Calling `$enqueue()` adds this command to a
#' [CommandQueue] and, when `$set_only` is `FALSE`, also enqueues a
#' [CollapseTreesCommand] and a [TreePlottingCommand].
#'
#' @seealso
#' * [BranchSwappingCommand] — branch swapping with replications (builds
#'   new trees from scratch).
#' * [set_tree_search()] — recommended way to add this command to a
#'   [TreeAnalysis].
#'
#' @references
#' Goloboff, P. A., Farris, J. S., & Nixon, K. C. (2008). TNT, a free
#' program for phylogenetic analysis. *Cladistics*, 24(5), 774--786.
#'
#' @examples
#' # Create with default settings (TBR swapping)
#' bb <- BranchBreakingCommand$new()
#'
#' # Switch to SPR swapping
#' bb$swapper <- "spr"
#'
#' # Adjust cluster size for large matrices
#' bb$swapper <- "tbr"
#' bb$cluster_size <- 30
#'
#' # Save only the best tree
#' bb$save_multiple <- FALSE
#'
#' # Generate the TNT command
#' bb$render()
#'
#' @importFrom checkmate asInt check_choice check_flag check_int test_true
#' @importFrom cli cli_abort cli_text col_grey col_red style_italic
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom stringr str_to_upper
#' @export
BranchBreakingCommand <- R6Class(
  "BranchBreakingCommand",
  inherit = TreeSearchCommand,
  active = list(
    #' @field swapper \[`character(1)`\]\cr
    #'   The branch swapping algorithm to use. One of:
    #'   * `"tbr"` — tree bisection-reconnection (default).
    #'   * `"spr"` — subtree pruning-regrafting.
    swapper = function(value) {
      label <- "swapper"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_choice(value, c("spr", "tbr"))
        if (!test_true(val_check)) {
          cli_abort(c("{.arg swapper} must be a valid option.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field cluster_size \[`integer(1)`\]\cr
    #'   The number of nodes (clusters) to use during TBR swapping. Larger
    #'   values result in faster swapping as matrix size increases. A value
    #'   of `0` enables automatic determination. Only applies when
    #'   `$swapper` is `"tbr"`.
    cluster_size = function(value) {
      label <- "cluster_size"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg cluster_size} must be an integer.",
                      "x" = val_check))
        }
        value <- asInt(value)
        self$set_argument_value(label, value)
      }
    },
    #' @field safe_unclip \[`logical(1)`\]\cr
    #'   Whether to use a safer but slower method for updating buffers when
    #'   clipping to find a better tree. Only applies when `$swapper` is
    #'   `"tbr"`.
    safe_unclip = function(value) {
      label <- "safe_unclip"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg safe_unclip} must be a logical.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field fill_only \[`logical(1)`\]\cr
    #'   Whether to stop swapping when the tree buffer is full.
    fill_only = function(value) {
      label <- "fill_only"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg fill_only} must be a logical.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field save_multiple \[`logical(1)`\]\cr
    #'   Whether to save multiple equally parsimonious trees during
    #'   swapping.
    save_multiple = function(value) {
      label <- "save_multiple"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg save_multiple} must be a logical.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field random_clip \[`logical(1)`\]\cr
    #'   Whether to randomise the tree clipping sequence during swapping.
    random_clip = function(value) {
      label <- "random_clip"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg random_clip} must be a logical.",
                      "x" = val_check))
        }
        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `BranchBreakingCommand` object.
    #'
    #' @param swapper \[`character(1)`\]\cr
    #'   Branch swapping algorithm to use: `"tbr"` (default) or `"spr"`.
    #'   See the `$swapper` field.
    #' @param cluster_size \[`integer(1)`\]\cr
    #'   Number of nodes to use during TBR swapping; `0` enables automatic
    #'   determination (default: `15`). Only applies when `swapper` is
    #'   `"tbr"`. See the `$cluster_size` field.
    #' @param safe_unclip \[`logical(1)`\]\cr
    #'   Use safer but slower buffer updating during TBR clipping (default:
    #'   `FALSE`). Only applies when `swapper` is `"tbr"`. See the
    #'   `$safe_unclip` field.
    #' @param fill_only \[`logical(1)`\]\cr
    #'   Stop swapping when the tree buffer is full (default: `FALSE`). See
    #'   the `$fill_only` field.
    #' @param save_multiple \[`logical(1)`\]\cr
    #'   Save multiple equally parsimonious trees during swapping (default:
    #'   `TRUE`). See the `$save_multiple` field.
    #' @param random_clip \[`logical(1)`\]\cr
    #'   Randomise the tree clipping sequence during swapping (default:
    #'   `FALSE`). See the `$random_clip` field.
    #' @param set_only \[`logical(1)`\]\cr
    #'   Configure-only mode (default: `FALSE`). See the `$set_only` field.
    #'
    #' @return A new `BranchBreakingCommand` object.
    initialize = function(swapper, cluster_size, safe_unclip, fill_only,
                          save_multiple, random_clip, set_only = FALSE){
      super$initialize(
        name = "bbreak",
        description = "Branch swapping using existing trees",
        set_only = set_only
      )

      swapper_fmt <- function(value) {
        str_to_upper(value)
      }
      self$new_argument(
        label = "swapper",
        description = "Swapping method",
        command_format = "{value}",
        default_value = "tbr",
        pretty_format = swapper_fmt
      )

      cluster_cmd_fmt <- function(value) {
        ifelse(value > 0, glue("clusters {value}"), "")
      }
      cluster_pty_fmt <- function(value) {
        ifelse(value > 0, value, "auto")
      }
      self$new_argument(
        label = "cluster_size",
        description = "Number of nodes to swap",
        command_format = cluster_cmd_fmt,
        default_value = 15,
        pretty_format = cluster_pty_fmt
      )

      yes_no_pty_fmt <- function(value) {
        ifelse(value, "yes", "no")
      }
      safe_unclip_cmd_fmt <- function(value) {
        glue("{value}safe", value = ifelse(value, "", "no"))
      }
      self$new_argument(
        label = "safe_unclip",
        description = "Use safe unclipping",
        command_format = safe_unclip_cmd_fmt,
        default_value = FALSE,
        pretty_format = yes_no_pty_fmt
      )

      fill_only_cmd_fmt <- function(value) {
        glue("{value}fillonly", value = ifelse(value, "", "no"))
      }
      self$new_argument(
        label = "fill_only",
        description = "Stop when tree buffer full",
        command_format = fill_only_cmd_fmt,
        default_value = FALSE,
        pretty_format = yes_no_pty_fmt
      )

      save_multiple_cmd_fmt <- function(value) {
        glue("{value}mulpars", value = ifelse(value, "", "no"))
      }
      self$new_argument(
        label = "save_multiple",
        description = "Save multiple trees",
        command_format = save_multiple_cmd_fmt,
        default_value = TRUE,
        pretty_format = yes_no_pty_fmt
      )

      random_clip_cmd_fmt <- function(value) {
        glue("{value}randclip", value = ifelse(value, "", "no"))
      }
      self$new_argument(
        label = "random_clip",
        description = "Randomize node clips",
        command_format = random_clip_cmd_fmt,
        default_value = FALSE,
        pretty_format = yes_no_pty_fmt
      )

      all_labels <- sapply(private$.arguments, `[[`, "label")
      self$template <- paste("{", all_labels, "}", sep = "")

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

      for (argument in private$.arguments) {
        arg_val <- try(get(argument$label), silent = TRUE)
        if (test_class(arg_val, "try-error")) {
          arg_val <- argument$default_value
        }
        self[[argument$label]] <- arg_val
      }
    }
  )
)
