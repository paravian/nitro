#' @importFrom methods show
setMethod("show", "NitroBase", function (object) {
  cat("Matrix properties:\n\n")
  cat(paste("Number of taxa:             ", nrow(object@matrix), "\n"))
  cat(paste("Number of characters        ", ncol(object@matrix), "\n"))
  cat(paste("Outgroup taxon:             ", object@outgroup, "\n"))
  cat(paste("Ordered characters:         ",
            ifelse(any(object@ordered_characters),
                   paste(which(object@ordered_characters), collapse=", "),
                   "None"), "\n"))
  cat(paste("Inactive characters:        ",
            ifelse(any(object@inactive_characters),
                   paste(which(object@inactive_characters), collapse=", "),
                   "None"), "\n"))
  cat(paste("Inactive taxa:              ",
            ifelse(any(object@inactive_taxa),
                   paste(rownames(object@matrix)[object@inactive_taxa],
                         collapse=", "),
                   "None"), "\n\n"))
  show(object@tree_search)
  if (inherits(object, "NitroImpliedWeights")) {
    cat(paste("\nImplied weighting:           enabled\n"))
    cat(paste("Concavity constant (k):     ", object@k, "\n"))
    cat(paste0("Multi-k                      ",
               ifelse(object@multi_k, "en", "dis"), "abled\n"))
  }
})

#' @importFrom methods callNextMethod
setMethod("initialize", "NitroBase",
  function (.Object, matrix, tree_search, ordered_characters,
            inactive_taxa, inactive_characters, collapse, outgroup) {
    matrix <- PhyDatToMatrix(matrix)
    if (class(collapse) == "numeric") {
      collapse <- as.integer(collapse)
    }
    if (is.null(outgroup)) {
      outgroup <- rownames(matrix)[1]
    }
    if (class(ordered_characters) %in% c("numeric", "integer")) {
      if (length(ordered_characters)) {
        ordered_characters <- 1:ncol(matrix) %in% ordered_characters
      } else {
        ordered_characters <- rep(FALSE, ncol(matrix))
      }
    }
    if (class(inactive_taxa) == "character") {
      if (length(inactive_taxa)) {
        inactive_taxa <- rownames(matrix) %in% inactive_taxa
      } else {
        inactive_taxa <- rep(FALSE, nrow(matrix))
      }
    }
    if (class(inactive_characters) %in% c("numeric", "integer")) {
      if (length(inactive_characters)) {
        inactive_characters <- 1:ncol(matrix) %in% inactive_characters
      } else {
        inactive_characters <- rep(FALSE, ncol(matrix))
      }
    }
    .Object <- callNextMethod(.Object, matrix = matrix,
      tree_search = tree_search, collapse = collapse, outgroup = outgroup,
      ordered_characters = ordered_characters, inactive_taxa = inactive_taxa,
      inactive_characters = inactive_characters)
    .Object
  })

#' Zero-length branch collapse rule
#'
#' Function to return or set the rule used for collapsing of zero-length
#' branches.
#' @param n an object that inherits \code{NitroBase}.
#' @return a numeric vector indicating the rule used for collapsing of
#' zero-length branches.
#' @export
#' @include NitroBase-class.R
#' @rdname collapse
setGeneric("collapse", function (n) standardGeneric("collapse"))

#' @rdname collapse
setMethod("collapse", c("NitroBase"), function (n) n@collapse)

#' @templateVar isgeneric TRUE
#' @template collapse-template
#' @export
#' @rdname collapse
setGeneric("collapse<-", function (n, value) standardGeneric("collapse<-"))

#' @importFrom methods validObject
#' @rdname collapse
setMethod("collapse<-", signature("NitroBase", "numeric"), function (n, value) {
  n@collapse <- as.integer(value)
  validObject(n)
  n
})

#' Outgroup
#'
#' Function to return or set the outgroup taxon.
#' @param n an object that inherits \code{NitroBase}.
#' @return a character vector indicating the name of the outgroup taxon.
#' @export
#' @include NitroBase-class.R
#' @rdname outgroup
setGeneric("outgroup", function (n) standardGeneric("outgroup"))

#' @rdname outgroup
setMethod("outgroup", c("NitroBase"), function (n) n@outgroup)

#' @templateVar isgeneric TRUE
#' @template outgroup-template
#' @export
#' @rdname outgroup
setGeneric("outgroup<-", function (n, value) standardGeneric("outgroup<-"))

#' @importFrom methods validObject
#' @rdname outgroup
setMethod("outgroup<-", signature("NitroBase", "character"),
          function (n, value) {
  n@outgroup <- value
  validObject(n)
  n
})

#' List ordered characters
#'
#' Function to return or set indexes of characters as ordered.
#' @title Character modification functions
#' @param n an object that inherits \code{NitroBase}.
#' @return a numeric vector indicating the characters defined as ordered.
#' @export
#' @include NitroBase-class.R
#' @rdname character-methods
setGeneric("ordered_characters", function (n)
  standardGeneric("ordered_characters"))

#' @rdname character-methods
setMethod("ordered_characters", signature("NitroBase"), function (n)
  which(n@ordered_characters))

#' @templateVar isgeneric TRUE
#' @template ordered_characters-template
#' @export
#' @rdname character-methods
setGeneric("ordered_characters<-", function (n, value)
  standardGeneric("ordered_characters<-"))

.ordered_characters_body <- function (n, value) {
  n@ordered_characters[n@ordered_characters] <- FALSE
  if (!is.null(value)) {
    n@ordered_characters[value] <- TRUE
  }
  validObject(n)
  n
}

#' @importFrom methods validObject
#' @rdname character-methods
setMethod("ordered_characters<-", signature("NitroBase", "numeric"),
          .ordered_characters_body)

#' @rdname character-methods
setMethod("ordered_characters<-", signature("NitroBase", "NULL"),
          .ordered_characters_body)

#' List inactive characters
#'
#' Function that returns the indexes of characters defined as inactive.
#' @title Character modification functions
#' @param n an object that inherits \code{NitroBase}.
#' @return a numeric vector indicating the characters defined as inactive.
#' @export
#' @include NitroBase-class.R
#' @rdname character-methods
setGeneric("inactive_characters", function (n)
  standardGeneric("inactive_characters"))

#' @rdname character-methods
setMethod("inactive_characters", signature("NitroBase"), function (n)
  which(n@inactive_characters))

#' @templateVar isgeneric TRUE
#' @template inactive_characters-template
#' @export
#' @rdname character-methods
setGeneric("inactive_characters<-", function (n, value)
  standardGeneric("inactive_characters<-"))

.inactive_characters_body <- function (n, value) {
  n@inactive_characters[n@inactive_characters] <- FALSE
  if (!is.null(value)) {
    n@inactive_characters[value] <- TRUE
  }
  validObject(n)
  n
}

#' @rdname character-methods
setMethod("inactive_characters<-", signature("NitroBase", "numeric"),
          .inactive_characters_body)

#' @rdname character-methods
setMethod("inactive_characters<-", signature("NitroBase", "NULL"),
          .inactive_characters_body)

#' List inactive taxa
#'
#' Function that returns the names of taxa defined as inactive.
#' @param n an object that inherits \code{NitroBase}.
#' @return a character vector indicating the taxa defined as inactive.
#' @export
#' @include NitroBase-class.R
#' @rdname taxa-methods
setGeneric("inactive_taxa", function (n, value)
  standardGeneric("inactive_taxa"))

#' @rdname taxa-methods
setMethod("inactive_taxa", signature("NitroBase", "missing"), function (n)
  rownames(n@matrix)[n@inactive_taxa])

.inactive_taxa_body <- function (n, value) {
  if (is.null(value)) {
    n@inactive_taxa[n@inactive_taxa] <- FALSE
  } else {
    n@inactive_taxa <- rownames(n@matrix) %in% value
    if (sum(n@inactive_taxa) != length(value)) {
      warning(paste("Taxa not added to inactive list:",
                    value[!value %in% rownames(n@matrix)]))
    }
  }
  validObject(n)
  n
}

#' @templateVar isgeneric TRUE
#' @template inactive_taxa-template
#' @export
#' @rdname taxa-methods
setGeneric("inactive_taxa<-", function (n, value)
  standardGeneric("inactive_taxa<-"))

#' @rdname taxa-methods
setMethod("inactive_taxa<-", signature("NitroBase", "character"),
          .inactive_taxa_body)

#' @rdname taxa-methods
setMethod("inactive_taxa<-", signature("NitroBase", "NULL"),
          .inactive_taxa_body)

#' Return TNT command
#'
#' A function that returns the command to perform the phylogenetic analysis for
#' a branch swapping analysis.
#' @param n an object of class \code{NitroBranchSwap}.
#' @return a character vector of the TNT command.
#' @rdname tnt_cmd
setGeneric("tnt_cmd", function (n) standardGeneric("tnt_cmd"))
