#' Define parameters for a random sectorial search analysis
#'
#' @importFrom methods new
#' @param min_size an integer value indicating the minimum size of random
#'   selections.
#' @param max_size an integer value indicating the maximum size of random
#'   selections.
#' @param selection_factor an integer value indicating the maximum number of
#'   random selections for the currently active taxa.
#' @param increase an integer value indicating the factor to increase the size
#'   of random selections if enough selection of the current size have been
#'   completed.
#' @param small_starts an integer value indicating the number of random
#'   addition sequence plus tree-bisection reconnection swaps to perform for
#'   random selections below \code{min_size}.
#' @param buffer a logical value indicating whether to use an independent
#    memory buffer for analysis of sectors.
#' @param slack an integer value indicating the percentage to increase the
#'   available memory during searches.
#' @export
NitroRandomSectorialSearch <- function (min_size = 0, max_size = 0,
  selection_factor = 50, increase = 100, small_starts = 3, buffer = TRUE,
  slack = 0) {
  new("NitroRandomSectorialSearch", min_size = min_size, max_size = max_size,
      selection_factor = selection_factor, increase = increase,
      small_starts = small_starts, buffer = buffer, slack = slack)
}

#' @importFrom methods callNextMethod slot<-
setMethod("initialize", "NitroRandomSectorialSearch",
  function (.Object, min_size, max_size, selection_factor,
            increase, small_starts, ...) {
  if (class(min_size) == "numeric") {
    min_size <- as.integer(min_size)
  }
  if (class(max_size) == "numeric") {
    max_size <- as.integer(max_size)
  }
  if (class(selection_factor) == "numeric") {
    selection_factor <- as.integer(selection_factor)
  }
  if (class(increase) == "numeric") {
    increase <- as.integer(increase)
  }
  if (class(small_starts) == "numeric") {
    small_starts <- as.integer(small_starts)
  }
  objs <- ls()
  for (obj in objs) {
    slot(.Object, obj) <- get(obj)
  }
  .Object <- callNextMethod(.Object, ...)
  .Object
})

#' @rdname tnt_cmd
setMethod("tnt_cmd", "NitroRandomSectorialSearch", function (n) {
  set_only <- any(sapply(sys.frames(),
                         function (f) class(f$n) == "NitroDriven"))
  sect_cmd <- c(" minsize ", n@min_size,
                " maxsize ", n@max_size,
                " selfact ", n@selection_factor,
                " increase ", n@increase,
                " starts ", n@small_starts,
                " slack ", n@slack,
                paste0(ifelse(n@buffer, " ", " no"), "xbuf"))
  if (set_only) {
    sect_cmd <- paste(c("sectsch: rss", sect_cmd, ";"), collapse = "")
  } else {
    sect_cmd <- paste(c("sectsch= rss", sect_cmd, ";"), collapse = "")
  }
  return(sect_cmd)
})

setMethod("show", "NitroRandomSectorialSearch", function (object) {
  cat("Parameters for random sectorial searches:\n\n")
  cat(paste("Minimum selection size:     ", object@min_size, "\n"))
  cat(paste("Maximum selection size:     ", object@max_size, "\n"))
  cat(paste("Maximum random selections:  ", object@selection_factor, "\n"))
  cat(paste("Selection increase factor:  ", object@increase, "\n"))
  cat(paste("Use independent buffer:     ", object@buffer, "\n"))
  cat(paste("Percentage memory increase: ", object@slack, "\n"))
})
