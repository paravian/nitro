#' Define parameters for a constraint sectorial search analysis
#'
#' @importFrom methods new
#' @param min_fork an integer value indicating the minimum fork number to use
#'   with constraint-based sectorial searches.
#' @param max_fork an integer value indicating the maximum fork number to use
#'   with constraint-based sectorial searches.
#' @param rounds an integer value indicating the number of times to cycle over
#'   groups in constraint-based selections.
#' @param buffer a logical value indicating whether to use an independent
#'   memory buffer for analysis of sectors.
#' @param slack an integer value indicating the percentage to increase the
#'   available memory during searches.
#' @export
NitroConstraintSectorialSearch <- function (min_fork = 10, max_fork = 10,
                                            rounds = 3, buffer = TRUE,
                                            slack = 0) {
  objs <- ls()
  args <- as.list(environment())[objs]
  do.call("new", c("NitroConstraintSectorialSearch", args))
}

#' @importFrom methods callNextMethod slot<-
setMethod("initialize", "NitroConstraintSectorialSearch",
  function (.Object, min_fork = 10, max_fork = 10, rounds = 3, buffer = TRUE,
            slack = 0) {
  objs <- ls()
  mf <- match.call()
  m <- match(c(".Object", objs), names(mf), 0L)
  mf <- mf[m]

  args <- as.list(mf)
  if (is.numeric(args$min_fork)) {
    .Object@min_fork <- as.integer(args$min_fork)
  }
  if (is.numeric(args$max_fork)) {
    .Object@max_fork <- as.integer(args$max_fork)
  }
  if (is.numeric(args$rounds)) {
    .Object@rounds <- as.integer(args$rounds)
  }
  do.call("callNextMethod", args)
})

#' @rdname tnt_cmd
setMethod("tnt_cmd", "NitroConstraintSectorialSearch", function (n) {
  set_only <- any(sapply(sys.frames(),
                         function (f) inherits(f$n, "NitroDriven")))
  sect_cmd <- c(" minfork ", n@min_fork,
                " maxfork ", n@max_fork,
                " rounds ", n@rounds,
                " slack ", n@slack,
                paste0(ifelse(n@buffer, " ", " no"), "xbuf"))
  if (set_only) {
    sect_cmd <- paste(c("sectsch: css", sect_cmd, ";"), collapse = "")
  } else {
    sect_cmd <- paste(c("sectsch= css", sect_cmd, ";"), collapse = "")
  }
  return(sect_cmd)
})

setMethod("show", "NitroConstraintSectorialSearch", function (object) {
  cat("Parameters for random sectorial searches:\n\n")
  cat(paste("Rounds:                     ", object@rounds, "\n"))
  cat(paste("Minimum fork size:          ", object@min_fork, "\n"))
  cat(paste("Maximum fork size:          ", object@max_fork, "\n"))
  cat(paste("Use independent buffer:     ", object@buffer, "\n"))
  cat(paste("Percentage memory increase: ", object@slack, "\n"))
})
