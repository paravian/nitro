#' Define ordered characters
#'
#' @param matrix a \code{phyDat} object of USER type.
#' @param characters a vector of characters to be set as ordered.
#' @return a \code{phyDat} object.
#' @export
orderedCharacters <- function (matrix, characters) {
  if (class(matrix) != "phyDat") {
    stop("'matrix' must be a phyDat object")
  }
  if (attr(matrix, "type") != "USER") {
    stop("Cannot apply ordering to sequences of type DNA, AA or CODON")
  }
  if (is.numeric(characters) == FALSE) {
    stop("'characters' must be a numeric")
  } else if (any(characters %% 1 != 0) | any(characters < 1)) {
    stop("'characters' can only contain positive integers > 0")
  }
  if (any(characters > attr(matrix, "nr"))) {
    stop(paste("Values of 'characters' exceed number of characters in the matrix (", attr(matrix, "nr"), ")"))
  }

  attr(matrix, "ordered") <- 1:attr(matrix, "nr") %in% characters
  return(matrix)
}

#' Define inactive characters
#'
#' @param matrix a \code{phyDat} object of USER type.
#' @param characters a numeric vector of characters to be set as inactive.
#' @return a \code{phyDat} object.
#' @export
inactiveCharacters <- function (matrix, characters) {
  if (class(matrix) != "phyDat") {
    stop("'matrix' must be a phyDat object")
  }
  if (is.numeric(characters) == FALSE) {
    stop("'characters' must be a numeric")
  } else if (any(characters %% 1 != 0) | any(characters < 1)) {
    stop("'characters' can only contain positive integers > 0")
  }
  if (any(characters > attr(matrix, "nr"))) {
    stop(paste("Values of 'characters' exceed number of characters in the matrix (", attr(matrix, "nr"), ")"))
  }

  attr(matrix, "inactive.characters") <- 1:attr(matrix, "nr") %in% characters
  return(matrix)
}
