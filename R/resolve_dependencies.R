#' Resolve Command Dependencies
#'
#' @description
#' Resolve the required dependencies of a `CommandList` and return a single
#' [CommandQueue] ready for execution.
#'
#' Each command is resolved against the commands that have already been
#' successfully resolved in the current pass. When a command is resolved,
#' its [CommandQueue] is built immediately by calling `$enqueue()` and
#' cached. Because resolving one command may set a dependency on a
#' previously resolved command — changing the state of that command and
#' therefore the output of its `$enqueue()` method — the function detects
#' these side effects and rebuilds the affected cached queues before
#' continuing. Once all commands have been resolved, the cached queues are
#' concatenated in resolution order and returned as a single
#' [CommandQueue].
#'
#' Resolution of optional dependencies is not performed here. It is
#' deferred to the `$initialize()` method of [TntInterface], which has
#' access to the fully assembled queue.
#'
#' @details
#' ## Resolution algorithm
#' The function iterates over the unresolved command list in a `while`
#' loop. In each iteration, every remaining command is tested:
#'
#' 1. If `$is_resolved` is already `TRUE` (the command has no required
#'    dependencies), it is enqueued immediately and moved to the resolved
#'    list.
#' 2. Otherwise, [resolve_command()] is called with the current resolved
#'    list. If it succeeds, the command is enqueued and moved to the
#'    resolved list.
#' 3. After each successful resolution, the function checks whether
#'    resolving the new command induced side effects in any previously
#'    resolved command by comparing the current `$format()` output of each
#'    resolved command against a snapshot taken before the resolution
#'    attempt. If a difference is detected, the cached queue for that
#'    command is rebuilt by calling `$enqueue()` again.
#' 4. The resolved command's queue is also checked via `$is_resolved`. If
#'    the queue itself contains unresolved commands — commands spawned
#'    inside `$enqueue()` that depend on other commands in the queue —
#'    those commands are resolved against the union of the global resolved
#'    list and the commands already resolved within the queue.
#' 5. If a complete iteration resolves no new commands, a deadlock is
#'    detected and [resolve_command()] is called with
#'    `suppress_errors = FALSE` on the first remaining command to produce
#'    an informative error.
#'
#' ## Side-effect detection
#' Side effects arise when a command object is shared by reference between
#' two command objects — for example, when [BranchSupportCommand] holds a
#' reference to the same [BranchBreakingCommand] object that was
#' previously resolved and enqueued. Setting the `"tree search"`
#' dependency on [BranchSupportCommand] calls the dependency's validation
#' callback, which may mutate the shared object (e.g. setting
#' `set_only = TRUE`). The snapshot-and-diff mechanism detects this and
#' rebuilds the affected queue so that the rendered TNT script reflects
#' the updated state.
#'
#' @param commands \[`CommandList`\]\cr
#'   A `CommandList` object of commands whose required dependencies are to
#'   be resolved.
#' @param suppress_errors \[`logical(1)`\]\cr
#'   If `TRUE` (default), unmet dependencies cause the command to be
#'   deferred silently. If `FALSE`, an error is raised immediately when a
#'   dependency cannot be satisfied. This argument is passed through to
#'   [resolve_command()] only when a deadlock is detected.
#'
#' @return A [CommandQueue] containing all commands in dependency-safe
#'   order, ready to be passed to [TntInterface]`$execute()`.
#'
#' @seealso
#' * [resolve_command()] — resolves the dependencies of a single command
#'   against a resolved list.
#' * [execute_analysis()] — the primary caller of this function.
#' * [CommandQueue] — the queue class returned by this function.
#' * [TntInterface] — resolves optional dependencies after the queue is
#'   assembled.
#'
#' @keywords internal
resolve_dependencies <- function (commands,  suppress_errors = TRUE) {
  val_check <- check_class(commands, "CommandList")
  if (!test_true(val_check)) {
    cli_abort(c("{.arg commands} must be a {.cls CommandList} object.",
                "x" = val_check))
  }

  val_check <- check_flag(suppress_errors)
  if (!test_true(val_check)) {
    cli_abort(c("{.arg suppress_errors} must be a logical value.",
                "x" = val_check))
  }

  queues <- NULL
  resolved <- structure(list(), class = "CommandList")
  eq_n <- 1

  while (length(commands) > 0) {
    unresolved <- structure(list(), class = "CommandList")

    for (cmd in commands) {
      is_resolved <- cmd$is_resolved
      if (!is_resolved) {
        # Snapshot resolved commands before attempting resolution so that
        # any side effects on previously resolved commands can be detected.
        old_resolved <- lapply(resolved, function (x) x$clone(deep = TRUE))
        is_resolved <- resolve_command(cmd, resolved, "required")

        # Detect side effects: if resolving cmd mutated a previously
        # resolved command, rebuild that command's cached queue.
        cmd_diff <- sapply(seq(resolved), function (x) {
          !identical(resolved[[x]]$format(), old_resolved[[x]]$format())
        })
        for (cmd_idx in which(cmd_diff)) {
          queues[[cmd_idx]] <- resolved[[cmd_idx]]$enqueue()
        }
      }

      if (is_resolved) {
        # Check whether commands spawned inside $enqueue() are themselves
        # resolved. If not, attempt to resolve them against the union of
        # the global resolved list and the commands already resolved within
        # the queue.
        tmp_queue <- cmd$enqueue()
        is_resolved <- tmp_queue$is_resolved
        if (!is_resolved) {
          queue_cmds <- tmp_queue$commands()
          queue_resolved <- structure(list(), class = "CommandList")

          for (queue_cmd in queue_cmds) {
            if (queue_cmd$is_resolved) {
              queue_resolved <- c(queue_resolved, queue_cmd)
            } else {
              all_resolved <- c(resolved, queue_resolved)
              is_resolved <- resolve_command(queue_cmd, all_resolved, "required")
            }
          }
        }

        if (is_resolved) {
          queues <- c(queues, list(tmp_queue))
          resolved <- c(resolved, cmd)
          commands <- Filter(f = function (x) !identical(x, cmd), commands)
        }
      }

      if (!is_resolved) {
        unresolved <- c(unresolved, cmd)
      }

      if (length(commands) > 0 & length(commands) == length(unresolved)) {
        eq_n <- eq_n + 1
        if (eq_n > 1) {
          resolve_command(commands[[1]], resolved, type = "required", suppress_errors = FALSE)
        }
      }
    }
  }

  queue <- Reduce(c, queues)

  queue
}
