#' Resolve a single command
#'
#' @param command A command to be resolved
#' @param resolved A `CommandList` of resolved commands
#' @param type The type of dependency to resolve
#' @param suppress_errors \[`logical(1)`\]\cr
#'   If `TRUE` (default), unmet dependencies return `FALSE` silently.
#'   If `FALSE`, an error is raised immediately.
#'
#' @keywords internal
resolve_command <- function(command, resolved, type = "required",
                            suppress_errors = TRUE) {
  val_check <- check_class(command, "BasicCommand")
  if (!test_true(val_check)) {
    cli_abort(c("{.arg command} must be inherit from {.cls BasicCommandList}.",
      "x" = val_check
    ))
  }

  coll <- makeAssertCollection()
  assert(
    check_null(resolved),
    check_class(resolved, "CommandList"),
    add = coll
  )

  if (!coll$isEmpty()) {
    val_check <- coll$getMessages()
    cli_abort(c("{.arg resolved} must be either {.val NULL} or {.cls CommandList} object.",
      "x" = val_check
    ))
  }

  val_check <- check_choice(type, c("required", "optional"))
  if (!test_true(val_check)) {
    cli_abort(c("{.arg type} must be a valid option.",
      "x" = val_check
    ))
  }

  val_check <- check_flag(suppress_errors)
  if (!test_true(val_check)) {
    cli_abort(c("{.arg suppress_errors} must be a logical value.",
      "x" = val_check
    ))
  }

  provides <- sapply(resolved, getElement, "provides")
  provide_cmds <- sapply(provides, test_null) %>%
    not() %>%
    which() %>%
    resolved[.]
  provides <- unlist(provides)

  all_depen <- command$requires
  if (type == "optional") {
    all_depen <- command$optional
  }

  res <- TRUE
  if (!test_null(all_depen)) {
    for (depen in all_depen) {
      if (type == "required" & depen %notin% provides) {
        res <- FALSE
        if (!suppress_errors) {
          mask <- all_depen %in% provides
          cli_abort(c("Failed dependency check for {class(command)[1]}: {all_requires[!mask]}.",
            "x" = val_check
          ))
        }
      }
      mask <- depen == provides
      if (sum(mask) > 1) {
        res <- FALSE
        if (!suppress_errors) {
          cli_abort(c("More than one command satisfies the dependency"))
        }
      } else if (sum(mask) == 1) {
        dep_cmd <- extract(provide_cmds, mask) %>%
          extract2(1)
        command$set_dependency(depen, dep_cmd)
      }

      if (res) {
        sub_cmd <- command$get_dependency(depen)
        if (!test_null(sub_cmd)) {
          res <- res & resolve_command(sub_cmd, resolved, type)
        }
      }
    }
  }

  res
}
