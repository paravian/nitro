#' Extra Tree Search Methods Command
#'
#' @description
#' An [R6][R6::R6Class] class that configures a combined search using
#' multiple tree search strategies in \pkg{nitro}.
#'
#' This command orchestrates several search methods — sectorial searches,
#' tree fusing, tree hybridizing, tree drifting, and the parsimony ratchet
#' — into a single driven search that cycles through the enabled strategies
#' for a specified number of replications. This wraps the TNT `xmult`
#' command.
#'
#' @details
#' ## Default values
#' | Parameter          | Default |
#' |--------------------|---------|
#' | `replications`     | `4`     |
#' | `hits`             | `1`     |
#' | `consense_times`   | `0` (disabled) |
#' | `keep_all`         | `FALSE` |
#' | `multiply`         | `TRUE`  |
#' | `sectorial_search` | Constrained + Random (defaults) |
#' | `tree_fusing`      | Enabled (defaults) |
#' | `tree_hybridizing` | Disabled (`rounds = 0`) |
#' | `tree_drifting`    | Enabled (`iterations = 5`) |
#' | `ratchet`          | Disabled (`iterations = 0`) |
#' | `set_only`         | `FALSE` |
#'
#' ## Strategy configuration
#' Each sub-strategy is configured via its own command object. Strategies
#' can be disabled by setting their iteration or round count to zero:
#' * `$ratchet$iterations <- 0` — disable the ratchet.
#' * `$tree_drifting$iterations <- 0` — disable tree drifting.
#' * `$tree_fusing$rounds <- 0` — disable tree fusing.
#' * `$tree_hybridizing$rounds <- 0` — disable tree hybridizing.
#'
#' Setting a strategy field to `NULL` resets it to its default
#' configuration.
#'
#' ## Queue integration
#' Calling `$enqueue()` adds this command to a [CommandQueue] and also
#' enqueues each enabled sub-strategy in set-only mode at priority `500`,
#' so their parameters are configured before the driven search executes at
#' priority `501`.
#'
#' ## Command output
#' `$render()` produces a string of the form
#' `xmult= replications {replications} hits {hits} ...;`
#'
#' @seealso
#' * [BranchSwappingCommand] — traditional branch swapping search.
#' * [RatchetCommand], [TreeDriftingCommand], [TreeFusingCommand],
#'   [TreeHybridizingCommand] — individual search strategies.
#' * [ConstrainedSectorialSearchCommand],
#'   [ExclusiveSectorialSearchCommand],
#'   [RandomSectorialSearchCommand] — sectorial search types.
#' * [set_tree_search()] — recommended way to add this command to a
#'   [TreeAnalysis].
#'
#' @references
#' Goloboff, P. A., Farris, J. S., & Nixon, K. C. (2008). TNT, a free
#' program for phylogenetic analysis. *Cladistics*, 24(5), 774--786.
#'
#' @examples
#' # Create with default settings
#' xm <- ExtraSearchMethodsCommand$new()
#'
#' # Increase replications and require more hits
#' xm$replications <- 10
#' xm$hits <- 3
#'
#' # Enable the ratchet
#' xm$ratchet$iterations <- 20
#'
#' # Disable tree drifting
#' xm$tree_drifting$iterations <- 0
#'
#' # View full configuration
#' xm$format()
#'
#' # Generate the TNT command
#' xm$render()
#'
#' @importFrom checkmate asInt assert check_class check_flag check_int check_class check_list check_null test_true test_null
#' @importFrom cli cli_abort cli_text col_grey symbol
#' @importFrom magrittr %>% extract extract2
#' @importFrom R6 R6Class
#' @importFrom stringr str_detect str_extract_all str_replace str_to_sentence str_trim
#' @export
ExtraSearchMethodsCommand <- R6Class(
  "ExtraSearchMethodsCommand",
   inherit = TreeSearchCommand,
   active = list(
     #' @field replications \[`integer(1)`\]\cr
     #'   The number of replications of the full search cycle. Must be a
     #'   positive integer.
     replications = function(value) {
       label <- "replications"
       if (missing(value)) {
         return(self$get_argument_value(label))
       } else {
         val_check <- check_int(value, lower = 1)
         if (!test_true(val_check)) {
           cli_abort(c("{.arg replications} must be an integer.",
                       "x" = val_check
           ))
         }

         self$set_argument_value(label, value)
       }
     },
     #' @field hits \[`integer(1)`\]\cr
     #'   The number of times the shortest tree must be found on consecutive
     #'   re-runs before the search stops. Must be a positive integer.
     hits = function(value) {
       label <- "hits"
       if (missing(value)) {
         return(self$get_argument_value(label))
       } else {
         val_check <- check_int(value, lower = 1)
         if (!test_true(val_check)) {
           cli_abort(c("{.arg hits} must be an integer.",
                       "x" = val_check
           ))
         }

         self$set_argument_value(label, value)
       }
     },
     #' @field consense_times \[`integer(1)`\]\cr
     #'   The number of times to compute a consensus until it stabilises.
     #'   Set to `0` (default) to disable consensus stabilisation. Must be a
     #'   non-negative integer.
     consense_times = function(value) {
       label <- "consense_times"
       if (missing(value)) {
         return(self$get_argument_value(label))
       } else {
         val_check <- check_int(value, lower = 0)
         if (!test_true(val_check)) {
           cli_abort(c("{.arg consense_times} must be an integer",
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
           cli_abort(c("{.arg keep_all} must be a logical.",
                       "x" = val_check
           ))
         }

         self$set_argument_value(label, value)
       }
     },
     #' @field multiply \[`logical(1)`\]\cr
     #'   Whether to find additional trees by fusing suboptimal trees with
     #'   optimal trees.
     multiply = function(value) {
       label <- "multiply"
       if (missing(value)) {
         return(self$get_argument_value(label))
       } else {
         val_check <- check_flag(value)
         if (!test_true(val_check)) {
           cli_abort(c("{.arg multiply} must be a logical.",
                       "x" = val_check
           ))
         }
         self$set_argument_value(label, value)
       }
     },
     #' @field sectorial_search \[`SectorialSearchCommand` or `list`\]\cr
     #'   One or more [SectorialSearchCommand] objects defining the
     #'   sectorial search strategies to use. Set to `NULL` to reset to the
     #'   default (constrained + random sectorial searches).
     sectorial_search = function(value) {
       label <- "sectorial_search"
       if (missing(value)) {
         return(self$get_argument_value(label))
       } else {
         coll <- makeAssertCollection()
         assert(
           check_class(value, "SectorialSearchCommand"),
           check_list(value, types = "SectorialSearchCommand"),
           add = coll
         )

         val_check <- coll$getMessages()
         if (!coll$isEmpty()) {
           cli_abort(c("{.arg sectorial_search} must be either {.arg NULL} or a valid (list of) {.cls SectorialSearchCommand} objects.",
                       "x" = val_check
           ))
         }

         self$set_argument_value(label, value)
       }
     },
     #' @field tree_fusing \[`TreeFusingCommand`\]\cr
     #'   A [TreeFusingCommand] object configuring tree fusing. Set to
     #'   `NULL` to reset to the default. Disable by setting
     #'   `$tree_fusing$rounds` to `0`.
     tree_fusing = function(value) {
       label <- "tree_fusing"
       if (missing(value)) {
         return(self$get_argument_value(label))
       } else {
         val_check <- check_class(value, "TreeFusingCommand")
         if (!test_true(val_check)) {
           cli_abort(c("{.arg tree_fusing} must be a valid object.",
                       "x" = val_check
           ))
         }

         self$set_argument_value(label, value)
       }
     },
     #' @field tree_hybridizing \[`TreeHybridizingCommand`\]\cr
     #'   A [TreeHybridizingCommand] object configuring tree hybridizing.
     #'   Set to `NULL` to reset to the default (disabled). Disable by
     #'   setting `$tree_hybridizing$rounds` to `0`.
     tree_hybridizing = function(value) {
       label <- "tree_hybridizing"
       if (missing(value)) {
         return(self$get_argument_value(label))
       } else {
         val_check <- check_class(value, "TreeHybridizingCommand")
         if (!test_true(val_check)) {
           cli_abort(c("{.arg tree_hybridizing} must be a valid object.",
                       "x" = val_check
           ))
         }

         self$set_argument_value(label, value)
       }
     },
     #' @field tree_drifting \[`TreeDriftingCommand`\]\cr
     #'   A [TreeDriftingCommand] object configuring tree drifting. Set to
     #'   `NULL` to reset to the default. Disable by setting
     #'   `$tree_drifting$iterations` to `0`.
     tree_drifting = function(value) {
       label <- "tree_drifting"
       if (missing(value)) {
         return(self$get_argument_value(label))
       } else {
         val_check <- check_class(value, "TreeDriftingCommand")
         if (!test_true(val_check)) {
           cli_abort(c("{.arg tree_drifting} must be a valid object.",
                       "x" = val_check
           ))
         }

         self$set_argument_value(label, value)
       }
     },
     #' @field ratchet \[`RatchetCommand`\]\cr
     #'   A [RatchetCommand] object configuring the parsimony ratchet. Set
     #'   to `NULL` to reset to the default (disabled). Disable by setting
     #'   `$ratchet$iterations` to `0`.
     ratchet = function(value) {
       label <- "ratchet"
       if (missing(value)) {
         return(self$get_argument_value(label))
       } else {
         val_check <- check_class(value, "RatchetCommand")
         if (!test_true(val_check)) {
           cli_abort(c("{.arg ratchet} must be a valid object.",
                       "x" = val_check
           ))
         }

         self$set_argument_value(label, value)
       }
     }
   ),
   public = list(
     #' @description
     #' Add this command and all enabled sub-strategies to a [CommandQueue].
     #'
     #' Each enabled sub-strategy is enqueued in set-only mode at priority
     #' `500` so that its parameters are configured before the driven search
     #' executes at priority `501`.
     #'
     #' @param .queue A [CommandQueue] object.
     #'
     #' @return A [CommandQueue] object.
     enqueue = function(.queue = NULL) {
       .queue <- super$enqueue(.queue)

       for (cmd in self$sectorial_search) {
         cmd$set_only <- TRUE
         .queue$add(cmd, 500)
       }

       if (self$tree_fusing$rounds > 0) {
         self$tree_fusing$set_only <- TRUE
         .queue$add(self$tree_fusing, 500)
       }
       if (self$tree_hybridizing$rounds > 0) {
         self$tree_hybridizing$set_only <- TRUE
         .queue$add(self$tree_hybridizing, 500)
       }
       if (self$tree_drifting$iterations > 0) {
         self$tree_drifting$set_only <- TRUE
         .queue$add(self$tree_drifting, 500)
       }
       if (self$ratchet$iterations > 0) {
         self$ratchet$set_only <- TRUE
         .queue$add(self$ratchet, 500)
       }

       .queue
     },
     #' @description
     #' Format the command and all enabled sub-strategies as a summary table.
     #'
     #' Returns a data frame showing the top-level parameters followed by
     #' the configuration of each enabled sub-strategy, separated by header
     #' rows.
     #'
     #' @param ... Not used.
     #'
     #' @return A `data.frame` with columns for description, current value,
     #'   and default value.
     format = function(...) {
       all_cmds <- self$sectorial_search
       if (self$tree_fusing$rounds > 0) {
         all_cmds <- c(all_cmds, self$tree_fusing)
       }
       if (self$tree_hybridizing$rounds > 0) {
         all_cmds <- c(all_cmds, self$tree_hybridizing)
       }
       if (self$tree_drifting$iterations > 0) {
         all_cmds <- c(all_cmds, self$tree_drifting)
       }
       if (self$ratchet$iterations > 0) {
         all_cmds <- c(all_cmds, self$ratchet)
       }

       options <- super$format()
       empty_row <- data.frame(matrix("", 1, 3))
       names(empty_row) <- names(options)

       parse_names <- function(name) {
         str_extract_all(name, "[A-Z][a-z]+") %>%
           extract2(1) %>%
           paste(collapse = " ") %>%
           str_to_sentence()
       }

       for (module in all_cmds) {
         if (test_null(module)) {
           next
         }

         module_class <- class(module) %>%
           extract(1)

         module_desc <- str_replace(module_class, "Command", "") %>%
           parse_names() %>%
           paste("parameters:")
         module_header_row <- empty_row
         module_header_row[, 1] <- module_desc

         module_options <- format(module)
         names(module_options) <- str_trim(names(module_options))
         module_options[, 1] <- str_trim(module_options[, 1])
         options <- rbind(options, empty_row, module_header_row, module_options)
       }

       options[, 1] <- format(options[, 1], justify = "left")
       options
     },
     #' @description
     #' Create a new `ExtraSearchMethodsCommand` object.
     #'
     #' Sub-strategy objects can be replaced entirely or configured in place
     #' after construction (e.g., `xm$ratchet$iterations <- 20`). Setting a
     #' sub-strategy argument to `NULL` resets it to its default
     #' configuration.
     #'
     #' @param replications \[`integer(1)`\]\cr
     #'   Number of full search cycle replications (default: `4`). See the
     #'   `$replications` field.
     #' @param hits \[`integer(1)`\]\cr
     #'   Number of consecutive shortest-tree hits required to stop (default:
     #'   `1`). See the `$hits` field.
     #' @param consense_times \[`integer(1)`\]\cr
     #'   Number of consensus stabilisation rounds; `0` disables (default:
     #'   `0`). See the `$consense_times` field.
     #' @param keep_all \[`logical(1)`\]\cr
     #'   Retain all trees from each replication regardless of length
     #'   (default: `FALSE`). See the `$keep_all` field.
     #' @param multiply \[`logical(1)`\]\cr
     #'   Find additional trees by fusing suboptimal with optimal trees
     #'   (default: `TRUE`). See the `$multiply` field.
     #' @param sectorial_search \[`SectorialSearchCommand`, `list`, or `NULL`\]\cr
     #'   One or more [SectorialSearchCommand] objects. `NULL` resets to the
     #'   default (constrained + random). See the `$sectorial_search` field.
     #' @param tree_fusing \[`TreeFusingCommand` or `NULL`\]\cr
     #'   Tree fusing configuration. `NULL` resets to the default (enabled,
     #'   5 rounds). See the `$tree_fusing` field.
     #' @param tree_hybridizing \[`TreeHybridizingCommand` or `NULL`\]\cr
     #'   Tree hybridizing configuration. `NULL` resets to the default
     #'   (disabled). See the `$tree_hybridizing` field.
     #' @param tree_drifting \[`TreeDriftingCommand` or `NULL`\]\cr
     #'   Tree drifting configuration. `NULL` resets to the default (enabled,
     #'   5 iterations). See the `$tree_drifting` field.
     #' @param ratchet \[`RatchetCommand` or `NULL`\]\cr
     #'   Ratchet configuration. `NULL` resets to the default (disabled).
     #'   See the `$ratchet` field.
     #' @param set_only \[`logical(1)`\]\cr
     #'   Configure-only mode (default: `FALSE`). See the `$set_only` field.
     #'
     #' @return A new `ExtraSearchMethodsCommand` object.
     initialize = function(replications, hits, consense_times, keep_all,
                           multiply, sectorial_search, tree_fusing,
                           tree_hybridizing, tree_drifting, ratchet,
                           set_only = FALSE) {
       super$initialize(
         name = "xmult",
         description = "Extra search methods",
         set_only = set_only
       )

       self$new_argument("replications", "Replications", "replications {value}", 4)
       self$new_argument("hits", "Hits", "hits {value}", 1)

       consense_cmd_fmt <- function(value) {
         ifelse(value == 0, "noconsense", glue("consense {value}"))
       }
       self$new_argument("consense_times", "Number of consensus rounds", consense_cmd_fmt, 0)

       keep_all_cmd_fmt <- function(value) {
         glue("{value}keepall", value = ifelse(keep_all, "", "no"))
       }
       yes_no_pty_fmt <- function(value) {
         ifelse(value, "yes", "no")
       }
       self$new_argument("keep_all", "Keep all trees", keep_all_cmd_fmt, FALSE, yes_no_pty_fmt)

       multiply_cmd_fmt <- function(value) {
         glue("{value}multiply", value = ifelse(keep_all, "", "no"))
       }
       self$new_argument("multiply", "Multiply trees by fusing", multiply_cmd_fmt, TRUE, yes_no_pty_fmt)

       sectorial_cmd_fmt <- function(value) {
         options <- c(
           Constrained = "css",
           Random = "rss",
           Exclusive = "xss"
         )
         class_names <- sapply(value, class) %>%
           extract(1, )
         mask <- sapply(names(options), str_detect, string = class_names) %>%
           apply(2, any)
         cmd <- paste(
           c(options[mask], paste("no", options[!mask], sep = "")),
           collapse = " "
         )
         cmd
       }
       sectorial_pty_fmt <- function(value) {
         ifelse(
           test_null(value),
           "none",
           sapply(value, class) %>%
             extract(1, ) %>%
             str_replace("^([A-Z][a-z]+).+", "\\1") %>%
             paste(collapse = ", ")
         )
       }
       sectorial_default <- c(
         ConstrainedSectorialSearchCommand$new(),
         RandomSectorialSearchCommand$new()
       )
       self$new_argument("sectorial_search", "Sectorial searches", sectorial_cmd_fmt, sectorial_default, sectorial_pty_fmt)

       tree_fusing_cmd_fmt <- function(value) {
         ifelse(value$rounds == 0, "nofuse", glue("fuse {value$rounds}"))
       }
       rounds_pty_fmt <- function(value) {
         ifelse(value$rounds == 0, "no", "yes")
       }
       tree_fusing_default <- TreeFusingCommand$new()
       self$new_argument("tree_fusing", "Tree fusing", tree_fusing_cmd_fmt, tree_fusing_default, rounds_pty_fmt)

       tree_hybrid_cmd_fmt <- function(value) {
         glue("{value}hybrid", value = ifelse(value$rounds == 0, "no", ""))
       }
       tree_hybrid_default <- TreeHybridizingCommand$new(rounds = 0)
       self$new_argument("tree_hybridizing", "Tree hybridizing", tree_hybrid_cmd_fmt, tree_hybrid_default, rounds_pty_fmt)

       tree_drift_cmd_fmt <- function(value) {
         ifelse(value$iterations == 0, "nodrift", glue("drift {value$iterations}"))
       }
       iter_pty_fmt <- function(value) {
         ifelse(value$iterations == 0, "no", "yes")
       }
       tree_drift_default <- TreeDriftingCommand$new(iterations = 5)
       self$new_argument("tree_drifting", "Tree drifting", tree_drift_cmd_fmt, tree_drift_default, iter_pty_fmt)

       ratchet_cmd_fmt <- function(value) {
         ifelse(value$iterations == 0, "noratchet", glue("ratchet {value$iterations}"))
       }
       ratchet_default <- RatchetCommand$new(iterations = 0)
       self$new_argument("ratchet", "Ratchet", ratchet_cmd_fmt, ratchet_default, iter_pty_fmt)

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
