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
#' @param weights an object inheriting class
#' @param start_trees an list of class \code{multiPhylo} containing trees to
#'   start the analysis with. Required for analyses using
#'   \code{"\linkS4class{NitroRatchet}"}.
#' @param combine a logical indicating whether to merge any start trees from
#'   \code{obj} with the trees obtained from the analysis, removing any
#'   duplicates. This option is useful when a large number of trees, or any
#'   number of trees with a large number of tips, from two separate analyses
#'   (i.e., such as when a set of most parsimonious trees are further explored
#'   with branch breaking) are to be combined, excluding duplicates. TNT's
#'   method for identifying duplicate trees is significantly more efficient
#'   than current R implementations (e.g., \code{\link{unique.phylo}}).
#' @param hold an integer indicating the number of trees to hold in TNTs tree
#'   buffer.
#' @param max_ram a numeric indicating the number of (binary) megabytes to
#'   allocate for use by TNT.
#'   \code{"\linkS4class{NitroWeightsBase}"}.
#' @return an object of class \code{"\linkS4class{NitroTreeSearch}"}.
#' @export
newTreeSearch <- function (matrix, method, ordered_characters = numeric(),
                           inactive_taxa = character(),
                           inactive_characters = numeric(), outgroup = NULL,
                           collapse = 3, weights = NULL, start_trees = NULL,
                           combine = FALSE, hold = 100, max_ram = 16) {
  if (class(matrix) != "phyDat") {
    stop("matrix must be of class phyDat")
  }
  if (!inherits(method, "NitroMethodsBase")) {
    stop("method must inherit from class NitroMethodsBase")
  }
  matrix <- PhyDatToMatrix(matrix)
  if (is.null(weights)) {
    weights <- new("NitroEqualWeights")
  }
  if (is.null(start_trees)) {
    start_trees <- list()
    class(start_trees) <- "multiPhylo"
  }
  constraints_obj <- new("NitroConstraintsBase")
  tree_search <- new("NitroTreeSearch", matrix, ordered_characters,
                     inactive_taxa, inactive_characters, collapse,
                     outgroup, constraints_obj, method, weights,
                     start_trees, combine, hold, max_ram)
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
            method, weights, start_trees, combine, hold, max_ram) {
    objs <- ls()
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

    set_sel_size <- function (driven_class) {
      sect_classes <- sapply(driven_class@sectorial_search, class)
      if ("NitroRandomSectorialSearch" %in% sect_classes) {
        def_size <- min(c(as.integer(ceiling(nrow(matrix) / 2)), 45L))
        idx <- which(sect_classes == "NitroRandomSectorialSearch")
        if (driven_class@sectorial_search[[idx]]@min_size == 0) {
          slot(driven_class@sectorial_search[[idx]], "min_size") <- def_size
        }
        if (driven_class@sectorial_search[[idx]]@max_size == 0) {
          slot(driven_class@sectorial_search[[idx]], "max_size") <- def_size
        }
      }
      driven_class
    }

    if (class(method) == "NitroDriven") {
      method <- set_sel_size(method)
    }

    if (inherits(method, "NitroResampleBase")) {
      if (class(method@tree_search) == "NitroDriven") {
        method@tree_search <- set_sel_size(method@tree_search)
      }
    }

    if (class(hold) == "numeric") {
      hold <- as.integer(hold)
    }

    for (obj in objs) {
      slot(.Object, obj) <- get(obj)
    }
    .Object <- callNextMethod(.Object)
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
setMethod("search_method", signature("NitroTreeSearch"), function (n) n@method)

#' @param n a \code{"\linkS4class{NitroTreeSearch}"} object.
#' @param value an object inheriting from \code{"\linkS4class{NitroMethodsBase}"}.
#' @rdname search_method
#' @export
setGeneric("search_method<-", function (n, value) standardGeneric("search_method<-"))

#' @rdname search_method
setMethod("search_method<-", signature("NitroTreeSearch"), function (n, value) {
  n@method <- value
  validObject(n)
  n
})

#' Starting trees
#'
#' A function that returns or sets the starting trees.
#' @param n a \code{"\linkS4class{NitroTreeSearch}"} object
#' @return a \code{multiPhylo} list.
#' @rdname start_trees
#' @export
setGeneric("start_trees", function (n) standardGeneric("start_trees"))

#' @rdname start_trees
setMethod("start_trees", signature("NitroTreeSearch"), function (n) n@start_trees)

#' @param value a \code{multiPhylo} list.
#' @rdname start_trees
#' @export
setGeneric("start_trees<-", function (n, value) standardGeneric("start_trees<-"))

#' @rdname start_trees
setMethod("start_trees<-", signature("NitroTreeSearch"), function (n, value) {
  n@start_trees <- value
  validObject(n)
  n
})

#' Hold trees
#'
#' A function that returns or sets the maximum number of trees to hold in TNT's
#' tree buffer.
#' @param n a \code{"\linkS4class{NitroTreeSearch}"} object
#' @return a numeric or integer indicating the number of trees to hold in TNT's
#' tree buffer.
#' @rdname hold
#' @export
setGeneric("hold", function (n) standardGeneric("hold"))

#' @rdname hold
setMethod("hold", signature("NitroTreeSearch"), function (n) n@hold)

#' @param value a numeric or integer indicating the number of trees to hold in
#'   TNT's tree buffer.
#' @rdname hold
#' @export
setGeneric("hold<-", function (n, value) standardGeneric("hold<-"))

#' @rdname hold
setMethod("hold<-", signature("NitroTreeSearch"), function (n, value) {
  if (is.numeric(value)) {
    value <- as.integer(value)
  }
  n@start_trees <- value
  validObject(n)
  n
})

#' Return TNT command
#'
#' A function that returns the command to perform the phylogenetic analysis for
#' a branch swapping analysis.
#' @param n an object inheriting from \code{"\linkS4class{NitroTreeSearch}"}.
#' @return a character vector of the TNT command.
#' @rdname tnt_cmd
setGeneric("tnt_cmd", function (n) standardGeneric("tnt_cmd"))
