#' Branch Swapping Tree Search Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures a branch swapping phylogenetic
#' tree search with replications in \pkg{nitro}.
#'
#' `BranchSwappingCommand` wraps the TNT `mult` command. It allows users to
#' control the number of replications, the number of trees retained per
#' replicate, and whether all trees are kept regardless of length. This is
#' the traditional search strategy in TNT, building new trees via random
#' addition sequences and refining them with branch swapping.
#'
#' @details
#' ## Default values
#' | Parameter      | Default |
#' |----------------|---------|
#' | `replications` | `10`    |
#' | `hold_rep`     | `10`    |
#' | `keep_all`     | `FALSE` |
#' | `set_only`     | `FALSE` |
#'
#' ## Command output
#' `$render()` produces a string of the form
#' `mult= replic {replications} hold {hold_rep} [no]keepall;`
#' (or `mult:` when `$set_only` is `TRUE`).
#'
#' ## Set-only mode
#' When `$set_only` is `TRUE`, the command configures parameters in TNT
#' without triggering a search. This is used internally by
#' [ExtraSearchMethodsCommand] when orchestrating multiple strategies.
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] and, when
#' `$set_only` is `FALSE`, also enqueues a [CollapseTreesCommand] and a
#' [TreePlottingCommand] as follow-up steps.
#'
#' @seealso
#' * [TreeSearchCommand] — direct parent class; adds set-only mode and
#'   queue integration for tree searches.
#' * [BranchBreakingCommand] — branch swapping on existing trees (without
#'   replications).
#' * [ExtraSearchMethodsCommand] — driven search combining multiple
#'   strategies.
#' * [RatchetCommand], [TreeDriftingCommand], [TreeFusingCommand],
#'   [TreeHybridizingCommand] — individual extra search strategies.
#'
#' @references
#' Goloboff, P. A., Farris, J. S., & Nixon, K. C. (2008). TNT, a free
#' program for phylogenetic analysis. *Cladistics*, 24(5), 774--786.
#'
#' @examples
#' # Create a branch swapping search with default settings
#' bs <- BranchSwappingCommand$new()
#'
#' # Create with custom settings
#' bs <- BranchSwappingCommand$new(replications = 100, hold_rep = 25,
#'                                  keep_all = TRUE)
#'
#' # Inspect current vs. default values
#' bs$format()
#'
#' # Modify settings after creation
#' bs$replications <- 50
#' bs$hold_rep <- 15
#' bs$keep_all <- FALSE
#'
#' # Generate the TNT command string
#' bs$render()
#'
#' # Configure only (no search execution)
#' bs$set_only <- TRUE
#' bs$render()
#'
#' # Add to a command queue
#' queue <- bs$enqueue()
#'
#' @importFrom checkmate check_int check_flag test_true test_class
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom R6 R6Class
#' @export
BranchSwappingCommand <- R6Class(
  "BranchSwappingCommand",
  inherit = TreeSearchCommand,
  active = list(
    #' @field replications \[`integer(1)`\]\cr
    #'   The number of replications to perform. Each replication builds a
    #'   new tree via random addition sequence and refines it with branch
    #'   swapping. Must be a positive integer. Corresponds to the `replic`
    #'   subcommand in TNT.
    replications = function(value) {
      label <- "replications"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg replications} must be a valid integer.",
                      "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field hold_rep \[`integer(1)`\]\cr
    #'   The maximum number of trees to retain during each replication. Must
    #'   be a positive integer. Corresponds to the `hold` subcommand in
    #'   TNT.
    hold_rep = function(value) {
      label <- "hold_rep"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_int(value, lower = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg hold_rep} must be a valid integer.",
                      "x" = val_check
          ))
        }
        self$set_argument_value(label, value)
      }
    },
    #' @field keep_all \[`logical(1)`\]\cr
    #'   Whether to retain all generated trees from each replication
    #'   regardless of length.
    keep_all = function(value) {
      label <- "keep_all"
      if (missing(value)) {
        return(self$get_argument_value(label))
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg keep_all} must be a logical value.",
                      "x" = val_check
          ))
        }

        self$set_argument_value(label, value)
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `BranchSwappingCommand` object.
    #'
    #' All parameters are optional. When omitted, default values are used
    #' (see **Details**). Parameters can also be modified after construction
    #' via the active bindings (e.g., `bs$replications <- 50`).
    #'
    #' @param replications \[`integer(1)`\]\cr
    #'   Number of replications (default: `10`). See the `$replications`
    #'   field.
    #' @param hold_rep \[`integer(1)`\]\cr
    #'   Maximum trees to hold per replicate (default: `10`). See the
    #'   `$hold_rep` field.
    #' @param keep_all \[`logical(1)`\]\cr
    #'   Retain all trees regardless of length? (default: `FALSE`). See the
    #'   `$keep_all` field.
    #'
    #' @return A new `BranchSwappingCommand` object.
    initialize = function(replications, hold_rep, keep_all) {
      super$initialize(
        name = "mult",
        description = "Branch swapping tree search with replicates"
      )

      self$new_argument(
        label = "replications",
        description = "Replications",
        command_format = "replic {value}",
        default_value = 10
      )
      self$new_argument(
        label = "hold_rep",
        description = "Trees to hold per replicate",
        command_format = "hold {value}",
        default_value = 10
      )

      keep_all_cmd_fmt <- function(value) {
        glue("{value}keepall", value = ifelse(value, "", "no"))
      }
      keep_all_pty_fmt <- function(value) {
        ifelse(value, "yes", "no")
      }
      self$new_argument(
        label = "keep_all",
        description = "Keeping all trees",
        command_format = keep_all_cmd_fmt,
        default_value = FALSE,
        pretty_format = keep_all_pty_fmt
      )

      all_labels <- sapply(private$.arguments, `[[`, "label")
      self$template <- paste("{", all_labels, "}", sep = "")

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
