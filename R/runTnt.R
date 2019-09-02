#' Executes TNT commands
#'
#' @importFrom ape write.nexus.data
#' @importFrom subprocess spawn_process process_read
#' @description Executes commands on a supplied phylogenetic matrix in the TNT
#'   command line binary.
#' @param tnt.path The location of the TNT command line binary.
#' @param matrix A \code{phyDat} object of the matrix.
#' @param tnt.params A list of parameters for executing commands in TNT.
#' @param timeout Time after which to terminate a non-responsive TNT process
#'   (milliseconds).
#' @return A character vector of the output from TNT.
runTnt <- function (tnt.path, matrix, tnt.params, timeout = 10000) {
  tnt.tempfile <- tempfile("nitro", fileext = ".tnt")
  tnt.matrix <- as.character(matrix)

  write.nexus.data(tnt.matrix, file=tnt.tempfile, interleaved = FALSE,
                   format = "standard")

  tnt.cmds <- c(paste0("hold ", tnt.params$hold, ";"))
  if (!is.null(tnt.params$outgroup)) {
    tnt.cmds <- c(tnt.cmds, paste0("outgroup ", tnt.params$outgroup, ";"))
  }
  if (!is.null(attr(matrix, "ordered"))) {
    tnt.cmds <- c(tnt.cmds,
                  paste0("ccode +", paste(which(attr(matrix, "ordered")), collapse = " "), ";"))
  }
  tnt.cmds <- c(tnt.cmds, tnt.params$cmd)

  tnt.args <- c(paste0("proc ", tnt.tempfile, ";"))

  if (!is.null(tnt.params$iw.cmd)) {
    tnt.args <- c(tnt.params$iw.cmd, tnt.args)
    if (!is.null(tnt.params$eiw.cmd)) {
      tnt.cmds <- c(tnt.params$eiw.cmd, tnt.cmds)
    }
  }

  tnt.block <- paste(c("BEGIN TNT;", "log stdout;", "tables =;", tnt.cmds,
                       "tplot *;", "length;", "minmax;", "END;"),
                     collapse = "\n")

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
