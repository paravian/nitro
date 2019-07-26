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

  write.nexus.data(tnt.matrix, file=tnt.tempfile, interleaved = FALSE,
                   format = "standard")

  tnt.block <- paste(c("BEGIN TNT;", "log stdout;", "tables =;", cmds,
                       "tplot *;", "length;", "minmax;", "END;"),
                     collapse = "\n")

  write(tnt.block, file=tnt.tempfile, append=TRUE)

  output <- system2(normalizePath(tnt.path), stdout = TRUE, stderr = FALSE,
                    input=paste0("proc ", tnt.tempfile, ";"))
  return(output)
}
