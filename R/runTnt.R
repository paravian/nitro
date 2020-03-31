#' Executes TNT commands
#'
#' @importFrom processx run
#' @importFrom ape write.nexus.data
#' @description Executes commands on a supplied phylogenetic matrix in the TNT
#'   command line binary.
#' @param params the parameters for an analysis as specified from the output
#'   of the \code{implicit.enum}, \code{branchswap}, \code{ratchet}, or
#'   \code{driven} commands.
#' @param tnt.path the location of the TNT command line binary.
#' @return An object containing the TNT parameters and the \code{phyDat} matrix
#'   used in the analysis, and a \code{multiPhylo} containing the trees found
#'   during the search.
#' @export
tnt <- function (params, tnt.path) {
  tnt.tempfile <- tempfile("nitro", fileext = ".tnt")
  tnt.matrix <- as.character(params$matrix)

  write.nexus.data(tnt.matrix, file=tnt.tempfile, interleaved = FALSE,
                   format = "standard")

  tnt.cmds <- c(paste0("collapse ", params$tnt.params$collapse, ";"))
  if (!is.null(params$tnt.params$hold)) {
    tnt.cmds <- c(tnt.cmds, paste0("hold ", params$tnt.params$hold, ";"))
  }
  if (!is.null(params$tnt.params$outgroup)) {
    tnt.cmds <-
      c(tnt.cmds, paste0("outgroup ",
                         which(names(params$matrix) == params$tnt.params$outgroup) - 1, ";"))
  }

  char.codes <- c()
  if (!is.null(attr(params$matrix, "ordered"))) {
    char.codes <- c(char.codes, "+",
                    which(attr(params$matrix, "ordered")) - 1)
  }
  if (!is.null(attr(params$matrix, "inactive.characters"))) {
    char.codes <- c(char.codes, "]",
                    which(attr(params$matrix, "inactive.characters")) - 1)
  }
  if (length(char.codes)) {
    tnt.cmds <- c(tnt.cmds, paste(c("ccode", char.codes, ";"), collapse=" "))
  }

  if (!is.null(attr(params$matrix, "inactive.taxa"))) {
    tnt.cmds <- c(tnt.cmds, paste(c("taxcode -",
                                    which(attr(params$matrix, "inactive.taxa")) - 1, ";"), collapse=" "))
  }

  tnt.cmds <- c(tnt.cmds, params$tnt.params$cmd)

  tnt.args <- c(paste0("proc ", tnt.tempfile, ";"))

  tnt.block <- c("BEGIN TNT;", "log stdout;", "tables =;", tnt.cmds,
                 "condense;", "tplot *;", "length;", "minmax;")

  if (!is.null(params$tnt.params$iw.cmd)) {
    tnt.args <- c(params$tnt.params$iw.cmd, tnt.args)
    tnt.block <- c(tnt.block, "score;")
    if (!is.null(params$tnt.params$eiw.cmd)) {
      tnt.cmds <- c(params$tnt.params$eiw.cmd, tnt.cmds)
    }
  }

  tnt.block <- c(tnt.block, "END;")

  write(tnt.block, file=tnt.tempfile, append=TRUE)

  # Prepare command line arguments for the TNT binary
  platform <- .Platform$OS.type
  tnt.arg <- paste0("proc ", tnt.tempfile,
                    ifelse(platform == "windows", ":", ","))

  # Initialise progress bar
  params$progress$bar$tick(0)

  # Define callback function for process line-by-line TNT output
  callback <- function (out, proc) {
    prog.info <- params$progress$value(out)
    if (!is.null(prog.info) & !params$progress$bar$finished) {
      params$progress$bar$update(prog.info$ratio, tokens = prog.info$tokens)
    }
    return(out)
  }

  # Run TNT binary and collect output
  tnt.output <- run(normalizePath(tnt.path), tnt.arg,
                    stderr_callback = callback)

  # Terminate progress bar and split TNT stdout
  if (!params$progress$bar$finished) {
    params$progress$terminate
  }
  tnt.output <- strsplit(tnt.output$stdout, "[\n\r]+")[[1]]
  params$trees <- tntTreeParse(tnt.output, names(params$matrix))
  params$progress <- NULL
  return(params)
}
