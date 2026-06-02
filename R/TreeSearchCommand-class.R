#' Tree Search Command Base Class
#'
#' @description
#' `TreeSearchCommand` extends [StandardCommand] for TNT commands that
#' perform phylogenetic tree searches. It adds set-only mode and queue
#' integration that automatically appends tree collapsing and plotting steps
#' after a search.
#'
#' This class is not exported. It is intended to be subclassed by concrete
#' search strategies such as [BranchSwappingCommand].
#'
#' @details
#' ## Subclassing
#' Subclasses should call `super$initialize(name, description, set_only)`
#' and then register their own arguments with `$new_argument()`. The parent
#' constructor sets `$provides` to `"tree search"` and initialises
#' `$set_only`.
#'
#' ## Set-only mode
#' When `$set_only` is `TRUE`, `$render()` appends a colon to the command
#' name (e.g., `mult:`) instead of an equals sign (`mult=`). This
#' configures parameters in TNT without triggering execution. Set-only mode
#' is applied automatically by [ExtraSearchMethodsCommand] when
#' orchestrating sub-strategies.
#'
#' ## Default values
#' | Parameter  | Default |
#' |------------|---------|
#' | `set_only` | `FALSE` |
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] at priority
#' `501`. When `$set_only` is `FALSE`, it also enqueues:
#' * A [CollapseTreesCommand] at priority `600`.
#' * A [TreePlottingCommand] at priority `601`.
#'
#' @seealso
#' * [StandardCommand] — parent class providing the argument system.
#' * [BranchSwappingCommand], [BranchBreakingCommand],
#'   [ExtraSearchMethodsCommand], [RatchetCommand],
#'   [TreeDriftingCommand], [TreeFusingCommand],
#'   [TreeHybridizingCommand] — concrete subclasses.
#' * [SectorialSearchCommand] — intermediate subclass for sectorial
#'   search strategies.
#'
#' @keywords internal
#' @importFrom checkmate assert assert_string check_flag check_function check_choice check_int check_null check_number check_string check_subset makeAssertCollection test_null test_true test_string
#' @importFrom cli cli_abort
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @importFrom stringr str_replace str_replace_all
TreeSearchCommand <- R6Class(
  "TreeSearchCommand",
  inherit = StandardCommand,
  private = list(
    .set_only = NULL
  ),
  active = list(
    #' @field requires \[`character`\]\cr
    #'   *(Read-only.)* The names of dependencies required by the command.
    #'   Derived from dependencies where `required` is `TRUE`. If `$set_only` is
    #'   `TRUE` and a "starting trees" dependency is present, it is removed.
    requires = function(value) {
      if (missing(value)) {
        requires <- super$requires
        if (self$set_only & "starting trees" %in% requires) {
          requires <- requires[!requires == "starting trees"]
        }
        return(requires)
      }
      cli_abort(c("{.var requires} is a read-only attribute."))
    },
    #' @field set_only \[`logical(1)`\]\cr
    #'   Whether the command is in set-only mode. When `TRUE`, `$render()`
    #'   produces a configuration command (e.g., `mult:`) rather than an
    #'   execution command (`mult=`).
    set_only = function (value) {
      if (missing(value)) {
        return(private$.set_only)
      } else {
        val_check <- check_flag(value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg set_only} must be a logical value.",
                      "x" = val_check))
        }
        private$.set_only <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Add this command and follow-up steps to a [CommandQueue].
    #'
    #' Adds this command at priority `501`. When `$set_only` is `FALSE`,
    #' also enqueues a [CollapseTreesCommand] (priority `600`) and a
    #' [TreePlottingCommand] (priority `601`).
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      .queue$add(self, 501)

      if (!self$set_only) {
        collapse_trees <- CollapseTreesCommand$new()
        .queue$add(collapse_trees, 600)

        plot_trees <- TreePlottingCommand$new(TRUE)
        .queue$add(plot_trees, 601)
      }

      .queue
    },
    #' @description
    #' Create a new `TreeSearchCommand` object.
    #'
    #' Sets `$provides` to `"tree search"` and initialises `$set_only`.
    #' Subclasses should call this via `super$initialize()` before
    #' registering their own arguments.
    #'
    #' @param name \[`character(1)`\]\cr
    #'   The TNT command name. See the `$name` field.
    #' @param description \[`character(1)`\]\cr
    #'   A human-readable description. See the `$description` field.
    #' @param set_only \[`logical(1)`\]\cr
    #'   Configure-only mode (default: `FALSE`). See the `$set_only` field.
    #'
    #' @return A new `TreeSearchCommand` object.
    initialize = function (name, description, set_only = FALSE) {
      a <- as.list(environment(), all = TRUE)

      super$initialize(
        name = name,
        description = description,
        provides = "tree search"
      )

      self$set_only <- set_only
    },
    #' @description
    #' Render the TNT command string.
    #'
    #' Calls the parent [StandardCommand] `$render()` method and then
    #' replaces the command name with `{name}=` (execute) or `{name}:`
    #' (set-only) depending on the value of `$set_only`.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT command.
    render = function(...) {
      cmd <- super$render()

      name <- glue(
        "{self$name}{suffix}",
        suffix = ifelse(self$set_only, ":", "=")
      )
      cmd <- str_replace(cmd, self$name, name)

      cmd
    }
  )
)
