#' Command Queue
#'
#' @description
#' \code{CommandQueue} is an R6 class that stores a list of TNT commands to
#'   execute in order.
#' @importFrom checkmate check_character check_number check_string test_number
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom R6 R6Class
#' @export
CommandQueue <- R6Class("CommandQueue",
  private = list(
    .queue = list()
  ),
  public = list(
    #' @param name The name of the command.
    #' @param arguments The arguments of the command.
    add = function (name, arguments = NULL) {
      name_check <- check_string(name, min.chars = 1)
      if (!isTRUE(name_check)) {
        cli_abort("{.arg name} must be a string.",
                  "x" = name_check)
      }
      coll <- makeAssertCollection()
      arg_check <- assert(
        check_null(arguments),
        check_number(arguments),
        check_character(arguments, min.chars = 1),
        add = coll
      )
      val_check <- coll$getMessages()
      if (!coll$isEmpty()) {
        cli_abort("{.arg arguments} must be either a number or character vector.",
                  "x" = arg_check)
      }
      if (test_number(arguments)) {
        arguments <- as.character(arguments)
      }

      private$.queue[[self$length() + 1]] <- list(name = name, arguments = arguments)
    },
    #' @param ... Ignored.
    read_next = function (...) {
      if (self$length() == 0) {
        cli_abort(c("Queue is empty, no more commands to read."))
      }
      item <- private$.queue[[1]]
      private$.queue[[1]] <- NULL
      return(item)
    },
    #' @param ... Ignored.
    length = function (...) {
      return(length(private$.queue))
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A command queue\")}")

      config <- c("Queue length:" = self$length()) %>%
        data.frame()
      names(config) <- NULL
      print(config)
    }
  )
)

#' Concatenate \code{CommandQueue} objects
#'
#' @param ... One or more \code{"\link{CommandQueue}"} objects.
#' @importFrom checkmate check_class
#' @importFrom cli cli_abort
#' @export
c.CommandQueue <- function (...) {
  obj <- list(...)
  obj <- unlist(obj)
  val_check <- sapply(obj, check_class, classes = c("CommandQueue", "R6")) %>%
    sapply(isTRUE)

  if (!all(val_check)) {
    cli_abort(c("All objects must be {.cls CommandQueue} class objects"))
  }

  obj <- Reduce(function(q1, q2) {
    while (q2$length() > 0) {
      cmd <- q2$read_next()
      q1$add(cmd$name, cmd$arguments)
    }
    return(q1)
  }, obj)

  return(obj)
}
