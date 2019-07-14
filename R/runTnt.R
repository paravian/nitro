#' Executes TNT commands
#'
#' @importFrom ape write.nexus.data
#' @description Executes commands on a supplied phylogenetic matrix in the TNT command line binary.
#' @param tnt.path The location of the TNT command line binary.
#' @param matrix A \code{phyDat} object of the matrix.
#' @param cmds A vector of commands to be issued by TNT.
#' @return A character vector of the output from TNT.
runTnt <- function (tnt.path, matrix, cmds) {
  tnt.tempfile <- tempfile("nitro", fileext = ".tnt")
  tnt.matrix <- as.character(matrix)
  write.nexus.data(tnt.matrix, file=tnt.tempfile, interleaved = FALSE, format = "standard")

  tnt.block <- paste(cmds, collapse="\n", sep="\n") %>%
    paste(., "tplot *;", "length;", "minmax;", sep="\n") %>%
    paste("BEGIN TNT;", ., "END;", sep="\n")

  write(tnt.block, file=tnt.tempfile, append=TRUE)

  platform <- tolower(.Platform$OS.type)

  if (platform == "unix") {
    output <- system2(normalizePath(tnt.path),
                      stdout = TRUE,
                      stderr = TRUE,
                      input=paste0("proc ", tnt.tempfile, ";"), env=c("COLUMNS=10000"))
    return(output)
  } else if (platform == "windows") {
    stop("Sorry, not available for your platform!")
  }
}
