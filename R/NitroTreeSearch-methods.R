#' @include NitroTreeSearch-class.R
NULL

#' Define tree search parameters
#'
#' @importFrom methods new
#' @importFrom TreeTools PhyDatToMatrix
#' @param matrix a \code{phyDat} object representing a character-taxon matrix
#' @param method an object that inherits from class
#'   \code{"\linkS4class{NitroMethodsBase}"}
#' @templateVar isgeneric FALSE
#' @template ordered_characters-template
#' @template inactive_taxa-template
#' @template inactive_characters-template
#' @template outgroup-template
#' @template collapse-template
#' @template weighting-template
#' @template k-template
#' @template weights-template
#' @template multi_k-template
#' @template proportion-template
#' @template max_ratio-template
#' @return an object of class \code{"\linkS4class{NitroTreeSearch}"}.
#' @export
newTreeSearch <- function (matrix, method, ordered_characters = numeric(),
                           inactive_taxa = character(),
                           inactive_characters = numeric(), outgroup = NULL,
                           collapse = 3, weighting = c("equal", "implied"),
                           k = 3, weights = numeric(), multi_k = FALSE,
                           proportion = 0.5, max_ratio = 5) {
  if (class(matrix) != "phyDat") {
    stop("matrix must be of class phyDat")
  }
  if (!inherits(method, "NitroMethodsBase")) {
    stop("method must inherit from class NitroMethodsBase")
  }
  matrix <- PhyDatToMatrix(matrix)
  weighting <- match.arg(weighting)
  if (weighting == "equal") {
    weights_obj <- new("NitroEqualWeights")
  } else {
    weights_obj <- new("NitroImpliedWeights", k, weights, multi_k, proportion,
                       max_ratio)
  }
  constraints_obj <- new("NitroConstraintsBase")
  tree_search <- new("NitroTreeSearch", matrix = matrix,
    ordered_characters = ordered_characters, inactive_taxa = inactive_taxa,
    inactive_characters = inactive_characters, collapse = collapse,
    outgroup = outgroup, constraints = constraints_obj, method = method,
    weights = weights_obj)
}

#' @importFrom methods show
setMethod("show", "NitroTreeSearch", function (object) {
  cat("Matrix properties:\n\n")
  cat(paste("Number of taxa:             ", nrow(object@matrix), "\n"))
  cat(paste("Number of characters        ", ncol(object@matrix), "\n"))
  cat(paste("Outgroup taxon:             ", rownames(object@matrix)[object@outgroup], "\n"))
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
  show(object@method)
  if (inherits(object@weights, "NitroImpliedWeights")) {
    show(object@weights)
  }
  if (length(object@constraints) > 0) {
    show(object@constraints)
  }
})

#' @importFrom methods callNextMethod
setMethod("initialize", "NitroTreeSearch",
  function (.Object, matrix, ordered_characters, inactive_taxa,
            inactive_characters, collapse, outgroup, constraints,
            method, weights) {
    if (class(collapse) == "numeric") {
      collapse <- as.integer(collapse)
    }
    if (is.null(outgroup)) {
      outgroup <- 1L
    } else if (is.character(outgroup)) {
      outgroup <- as.integer(which(rownames(matrix) == outgroup)[1])
    }
    if (is.na(outgroup)) {
      stop("outgroup must be a valid taxon name")
    }
    if (class(ordered_characters) %in% c("numeric", "integer")) {
      if (length(ordered_characters)) {
        ordered_characters <- 1:ncol(matrix) %in% ordered_characters
      } else {
        ordered_characters <- rep(FALSE, ncol(matrix))
      }
    }
    if (class(inactive_taxa) == "character") {
      if (any(!inactive_taxa %in% rownames(matrix))) {
        stop("taxa in inactive_taxa not present in matrix: ",
             paste(inactive_taxa[!inactive_taxa %in% rownames(matrix)],
                   collapse = ", "))
      }
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
    if (class(method) == "NitroDriven") {
      sect_classes <- sapply(method@sectorial_search, class)
      if ("NitroRandomSectorialSearch" %in% sect_classes) {
        def_size <- min(c(as.integer(ceiling(nrow(matrix) / 2)), 45L))
        idx <- which(sect_classes == "NitroRandomSectorialSearch")
        if (method@sectorial_search[[idx]]@min_size == 0) {
          method@sectorial_search[[idx]]@min_size <- def_size
        }
        if (method@sectorial_search[[idx]]@max_size == 0) {
          method@sectorial_search[[idx]]@max_size <-def_size
        }
      }
    }
    .Object <- callNextMethod(.Object, matrix = matrix,
      ordered_characters = ordered_characters, inactive_taxa = inactive_taxa,
      inactive_characters = inactive_characters, collapse = collapse,
      outgroup = outgroup, constraints = constraints, method = method,
      weights = weights)
    .Object
  })

#' Zero-length branch collapse rule
#'
#' Function to return or set the rule used for collapsing of zero-length
#' branches.
#' @param n a \code{"\linkS4class{NitroTreeSearch}"} object.
#' @return a numeric vector indicating the rule used for collapsing of
#' zero-length branches.
#' @export
#' @rdname collapse
setGeneric("collapse", function (n) standardGeneric("collapse"))

#' @aliases collapse
#' @rdname collapse
setMethod("collapse", c("NitroTreeSearch"), function (n) n@collapse)

#' @templateVar isgeneric TRUE
#' @template collapse-template
#' @aliases collapse
#' @export
#' @rdname collapse
setGeneric("collapse<-", function (n, value) standardGeneric("collapse<-"))

#' @importFrom methods validObject
#' @aliases collapse
#' @rdname collapse
setMethod("collapse<-", signature("NitroTreeSearch", "numeric"), function (n, value) {
  n@collapse <- as.integer(value)
  validObject(n)
  n
})

#' Outgroup
#'
#' Function to return or set the outgroup taxon.
#' @param n a \code{"\linkS4class{NitroTreeSearch}"} object.
#' @return a character vector indicating the name of the outgroup taxon.
#' @export
#' @rdname outgroup
setGeneric("outgroup", function (n) standardGeneric("outgroup"))

#' @rdname outgroup
setMethod("outgroup", c("NitroTreeSearch"), function (n) rownames(n@matrix)[n@outgroup])

#' @templateVar isgeneric TRUE
#' @template outgroup-template
#' @export
#' @rdname outgroup
setGeneric("outgroup<-", function (n, value) standardGeneric("outgroup<-"))

#' @importFrom methods validObject
#' @rdname outgroup
setMethod("outgroup<-", signature("NitroTreeSearch", "character"),
          function (n, value) {
  n@outgroup <- as.integer(which(rownames(n@matrix) == value)[1])
  if (is.na(n@outgroup)) {
    stop("outgroup must be a valid taxon name")
  }
  validObject(n)
  n
})

#' List ordered characters
#'
#' Function to return or set indexes of characters as ordered.
#' @title Character modification functions
#' @param n a \code{"\linkS4class{NitroTreeSearch}"} object.
#' @return a numeric vector indicating the characters defined as ordered.
#' @export
#' @rdname character-methods
setGeneric("ordered_characters", function (n)
  standardGeneric("ordered_characters"))

#' @rdname character-methods
setMethod("ordered_characters", signature("NitroTreeSearch"), function (n)
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
setMethod("ordered_characters<-", signature("NitroTreeSearch", "numeric"),
          .ordered_characters_body)

#' @rdname character-methods
setMethod("ordered_characters<-", signature("NitroTreeSearch", "NULL"),
          .ordered_characters_body)

#' List inactive characters
#'
#' Function that returns the indexes of characters defined as inactive.
#' @title Character modification functions
#' @param n a \code{"\linkS4class{NitroTreeSearch}"} object.
#' @return a numeric vector indicating the characters defined as inactive.
#' @export
#' @rdname character-methods
setGeneric("inactive_characters", function (n)
  standardGeneric("inactive_characters"))

#' @rdname character-methods
setMethod("inactive_characters", signature("NitroTreeSearch"), function (n)
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
setMethod("inactive_characters<-", signature("NitroTreeSearch", "numeric"),
          .inactive_characters_body)

#' @rdname character-methods
setMethod("inactive_characters<-", signature("NitroTreeSearch", "NULL"),
          .inactive_characters_body)

#' List inactive taxa
#'
#' Function that returns the names of taxa defined as inactive.
#' @param n a \code{"\linkS4class{NitroTreeSearch}"} object
#' @return a character vector indicating the taxa defined as inactive.
#' @export
#' @rdname taxa-methods
setGeneric("inactive_taxa", function (n)
  standardGeneric("inactive_taxa"))

#' @rdname taxa-methods
setMethod("inactive_taxa", signature("NitroTreeSearch"), function (n)
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
setMethod("inactive_taxa<-", signature("NitroTreeSearch", "character"),
          .inactive_taxa_body)

#' @rdname taxa-methods
setMethod("inactive_taxa<-", signature("NitroTreeSearch", "NULL"),
          .inactive_taxa_body)

#' Return search method
#'
#' A function that returns and sets the current search method
#' @param n a \code{"\linkS4class{NitroTreeSearch}"} object.
#' @return an object inheriting \code{"\linkS4class{NitroMethodsBase}"}.
#' @rdname search_method
#' @export
setGeneric("search_method", function (n) standardGeneric("search_method"))

#' @rdname search_method
setMethod("search_method", "NitroTreeSearch", function (n) n@method)

#' @param n a \code{"\linkS4class{NitroTreeSearch}"} object.
#' @param value an object inheriting from \code{"\linkS4class{NitroMethodsBase}"}.
#' @rdname search_method
#' @export
setGeneric("search_method<-", function (n, value) standardGeneric("search_method<-"))

#' @rdname search_method
setMethod("search_method<-", "NitroTreeSearch", function (n, value) {
  n@method <- value
  validObject(n)
  n
})

#' Return TNT command
#'
#' A function that returns the command to perform the phylogenetic analysis for
#' a branch swapping analysis.
#' @param n an object inheriting from \code{"\linkS4class{NitroTreeSearch}"}.
#' @param ... other arguments
#' @return a character vector of the TNT command.
#' @rdname tnt_cmd
setGeneric("tnt_cmd", function (n, ...) standardGeneric("tnt_cmd"))
