#' Define parameters for a random sectorial search analysis
#'
#' @importFrom methods new
#' @param min_size an integer value indicating the minimum size of random
#'   selections. If left at 0 (the default), this value will be automatically
#'   set according to the number of taxa in the matrix when a new
#'   \code{"\linkS4class{NitroTreeSearch}"} object is created by
#'   \code{"\link{newTreeSearch}"}.
#' @param max_size an integer value indicating the maximum size of random
#'   selections. If left at 0 (the default), this value will be automatically
#'   set according to the number of taxa in the matrix when a new
#'   \code{"\linkS4class{NitroTreeSearch}"} object is created by
#'   \code{"\link{newTreeSearch}"}.
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
  objs <- ls()
  args <- as.list(environment())[objs]
  do.call("new", c("NitroRandomSectorialSearch", args))
}

#' @importFrom methods callNextMethod slot<-
setMethod("initialize", "NitroRandomSectorialSearch",
  function (.Object, min_size = 0, max_size = 0, selection_factor = 50,
            increase = 100, small_starts = 3, buffer = TRUE, slack = 0) {
  objs <- ls()
  mf <- match.call()
  m <- match(c(".Object", objs), names(mf), 0L)
  mf <- mf[m]

  args <- as.list(mf)
  if (class(args$min_size) == "numeric") {
    .Object@min_size <- as.integer(args$min_size)
  }
  if (class(args$max_size) == "numeric") {
    .Object@max_size <- as.integer(args$max_size)
  }
  if (class(args$selection_factor) == "numeric") {
    .Object@selection_factor <- as.integer(args$selection_factor)
  }
  if (class(args$increase) == "numeric") {
    .Object@increase <- as.integer(args$increase)
  }
  if (class(args$small_starts) == "numeric") {
    .Object@small_starts <- as.integer(args$small_starts)
  }
  do.call("callNextMethod", args)
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
