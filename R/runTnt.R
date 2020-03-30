#' Executes TNT commands
#'
#' @importFrom processx run
#' @importFrom ape write.nexus.data
#' @description Executes commands on a supplied phylogenetic matrix in the TNT
#'   command line binary.
#' @param tnt.path The location of the TNT command line binary.
#' @param analysis The parameters for an analysis as specified from the output
#'   of the \code{branchswap}, \code{ratchet}, or \code{driven} commands.
#' @param progress A list providing the progress bar object and a callback
#'   function for updating the progress bar.
#' @return A character vector of the output from TNT.
tnt <- function (tnt.path, analysis, progress) {
  tnt.tempfile <- tempfile("nitro", fileext = ".tnt")
  tnt.matrix <- as.character(analysis$matrix)

  write.nexus.data(tnt.matrix, file=tnt.tempfile, interleaved = FALSE,
                   format = "standard")

  tnt.cmds <- c(paste0("collapse ", analysis$tnt.params$collapse, ";"))
  if (!is.null(analysis$tnt.params$hold)) {
    tnt.cmds <- c(tnt.cmds, paste0("hold ", analysis$tnt.params$hold, ";"))
  }
  if (!is.null(analysis$tnt.params$outgroup)) {
    tnt.cmds <-
      c(tnt.cmds, paste0("outgroup ",
                         which(names(analysis$matrix) == analysis$tnt.params$outgroup) - 1, ";"))
  }

  char.codes <- c()
  if (!is.null(attr(analysis$matrix, "ordered"))) {
    char.codes <- c(char.codes, "+",
                    which(attr(analysis$matrix, "ordered")) - 1)
  }
  if (!is.null(attr(analysis$matrix, "inactive.characters"))) {
    char.codes <- c(char.codes, "]",
                    which(attr(analysis$matrix, "inactive.characters")) - 1)
  }
  if (length(char.codes)) {
    tnt.cmds <- c(tnt.cmds, paste(c("ccode", char.codes, ";"), collapse=" "))
  }

  if (!is.null(attr(analysis$matrix, "inactive.taxa"))) {
    tnt.cmds <- c(tnt.cmds, paste(c("taxcode -",
                                    which(attr(analysis$matrix, "inactive.taxa")) - 1, ";"), collapse=" "))
  }

  tnt.cmds <- c(tnt.cmds, analysis$tnt.params$cmd)

  tnt.args <- c(paste0("proc ", tnt.tempfile, ";"))

  tnt.block <- c("BEGIN TNT;", "log stdout;", "tables =;", tnt.cmds,
                 "condense;", "tplot *;", "length;", "minmax;")

  if (!is.null(analysis$tnt.params$iw.cmd)) {
    tnt.args <- c(analysis$tnt.params$iw.cmd, tnt.args)
    tnt.block <- c(tnt.block, "score;")
    if (!is.null(analysis$tnt.params$eiw.cmd)) {
      tnt.cmds <- c(analysis$tnt.params$eiw.cmd, tnt.cmds)
    }
  }

  tnt.block <- c(tnt.block, "END;")

  write(tnt.block, file=tnt.tempfile, append=TRUE)

  # Prepare command line arguments for the TNT binary
  platform <- .Platform$OS.type
  tnt.arg <- paste0("proc ", tnt.tempfile,
                    ifelse(platform == "windows", ":", ","))

  # Initialise progress bar
  progress$bar$tick(0)

  # Define callback function for process line-by-line TNT output
  callback <- function (out, proc) {
    prog.info <- progress$value(out)
    if (!is.null(prog.info) & !progress$bar$finished) {
      progress$bar$update(prog.info$ratio, tokens = prog.info$tokens)
    }
    return(out)
  }

  # Run TNT binary and collect output
  tnt.output <- run(normalizePath(tnt.path), tnt.arg,
                    stderr_callback = callback)

  # Terminate progress bar and split TNT stdout
  progress$bar$update(1)
  tnt.output <- strsplit(tnt.output$stdout, "[\n\r]+")[[1]]
  return(tnt.output)
}
