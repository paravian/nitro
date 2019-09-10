#' Executes TNT commands
#'
#' @importFrom ape write.nexus.data
#' @description Executes commands on a supplied phylogenetic matrix in the TNT
#'   command line binary.
#' @param tnt.path The location of the TNT command line binary.
#' @param analysis The parameters for an analysis as specified from the output
#'   of the \code{branchswap}, \code{ratchet}, or \code{driven} commands.
#' @param timeout Time after which to terminate a non-responsive TNT process
#'   (milliseconds).
#' @return A character vector of the output from TNT.
runTnt <- function (tnt.path, analysis, timeout = 10000) {
  tnt.tempfile <- tempfile("nitro", fileext = ".tnt")
  tnt.matrix <- as.character(analysis$matrix)

  write.nexus.data(tnt.matrix, file=tnt.tempfile, interleaved = FALSE,
                   format = "standard")

  tnt.cmds <- c(paste0("hold ", analysis$tnt.params$hold, ";"),
                paste0("collapse ", analysis$tnt.params$collapse, ";"))
  if (!is.null(analysis$tnt.params$outgroup)) {
    tnt.cmds <- c(tnt.cmds, paste0("outgroup ",
                                   analysis$tnt.params$outgroup, ";"))
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

  platform <- tolower(.Platform$OS.type)
  if (platform == "windows") {
    output <- system2(normalizePath(tnt.path),
                      args = paste(tnt.args, sep=" "),
                      stdout = TRUE, stderr = FALSE)
  }
  else {
    output <- system2(normalizePath(tnt.path), stdout = TRUE,
                      stderr = FALSE,
                      input = paste(tnt.args, sep=" "))
  }
  return(output)
}
