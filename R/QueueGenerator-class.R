#' Queue generator
#'
#' @description
#' Defines functions common to objects that generate queues.
#' @importFrom checkmate assert check_class check_null makeAssertCollection
#' @importFrom cli cli_abort
QueueGenerator <- R6Class("QueueGenerator",
  public = list(
    #' @param .queue A \code{\link{CommandQueue}} object.
    queue = function (.queue = NULL) {
      coll <- makeAssertCollection()
      assert(
        check_null(.queue),
        check_class(.queue, "CommandQueue"),
        add = coll
      )

      if (!coll$isEmpty()) {
        val_check <- coll$getMessages()
        cli_abort(c("{.arg .queue} must be either {.val NULL} or a {.cls CommandQueue} object.",
                    "x" = val_check))
      }

      if (!test_null(.queue)) {
        queue <- .queue
      } else {
        queue <- CommandQueue$new()
      }
      return(queue)
    }
  )
)
