#' Define tree searches
#'
#' \code{NitroTreeSearch} is an R6 class that stores information regarding the
#' tree search method, character weighting scheme and constraints on monophyly.
#' @importFrom checkmate asInt assert assertCharacter assertClass
#'   assertInt assertLogical assertNumber assertSubset checkNull checkCharacter
#'   checkNumeric checkSubset
#' @importFrom R6 R6Class
#' @export
NitroTreeSearch <- R6Class("NitroTreeSearch",
  private = list(
    .matrix = NULL,
    .ordered_characters = NULL,
    .inactive_taxa = NULL,
    .inactive_characters = NULL,
    .outgroup = NULL,
    .collapse = NULL,
    .constraints = NULL,
    .method = NULL,
    .weights = NULL,
    .start_trees = NULL,
    .combine = NULL,
    .hold = NULL,
    .max_ram = NULL
  ),
  active = list(
    #' @field matrix A \code{phyDat} object representing a character-taxon matrix
    matrix = function (value) {
      if (missing(value)) {
        private$.matrix
      } else {
        print("`matrix` is read only")
      }
    },
    #' @field method An object that inherits from class
    #'   \code{"\link{NitroMethodsBase}"}
    method = function (value) {
      if (missing(value)) {
        private$.method
      } else {
        assertClass(value, "NitroMethodsBase")
        set_sel_size <- function (driven_class) {
          sect_classes <- sapply(driven_class$sectorial_search, class)
          if ("NitroRandomSectorialSearch" %in% sect_classes) {
            def_size <- min(c(as.integer(ceiling(nrow(matrix) / 2)), 45L))
            idx <- which(sect_classes == "NitroRandomSectorialSearch")
            if (driven_class$sectorial_search[[idx]]$max_size == 0) {
              driven_class$sectorial_search[[idx]]$max_size <- def_size
            }
            if (driven_class$sectorial_search[[idx]]$min_size == 0) {
              driven_class$sectorial_search[[idx]]$min_size <- def_size
            }
          }
          driven_class
        }

        if (inherits(value, "NitroDriven")) {
          value <- set_sel_size(value)
        }

        if (inherits(value, "NitroResampleBase")) {
          if (inherits(private$.tree_search, "NitroDriven")) {
            private$.tree_search <- set_sel_size(private$.tree_search)
          }
        }
        private$.method <- value
      }
    #' @field ordered_characters A numeric vector indicating the characters to
    #'   be treated as ordered.
    },
    ordered_characters = function (value) {
      if (missing(value)) {
        which(private$.ordered_characters)
      } else {
        assert(
          checkNull(value),
          checkNumeric(value, min.len = 1, max.len = ncol(private$.matrix),
                       any.missing = FALSE, unique = TRUE)
        )
        oc <- rep(FALSE, ncol(private$.matrix))
        if (!is.null(value)) {
          oc[value] <- TRUE
        }
        private$.ordered_characters <- oc
      }
    },
    #' @field inactive_taxa A character vector indicating the taxa to be
    #'   inactivated.
    inactive_taxa = function (value) {
      if (missing(value)) {
        private$.inactive_taxa
      } else {
        assert(
          checkNull(value),
          assert(
            checkCharacter(value, min.len = 1, max.len = nrow(private$.matrix),
                           any.missing = FALSE, unique = TRUE),
            checkSubset(value, rownames(private$.matrix)),
            combine = "and"
          )
        )
        it <- rep(FALSE, nrow(private$.matrix))
        if (!is.null(value)) {
          it[rownames(private$.matrix) %in% value] <- TRUE
        }
        constr <- Reduce(or, lapply(private$.constraints, function (c) {
          or(c$fixed_otus, c$floating_otus)
        }))
        inact_constr <- and(constr, it)
        if (any(inact_constr)) {
          stop("Requested inactive taxa are in constraint groups: ",
               rownames(private$.matrix)[inact_constr])
        }
        private$.inactive_taxa <- it
      }
    },
    #' @field inactive_characters A numeric vector indicating the characters to
    #'   be inactivated.
    inactive_characters = function (value) {
      if (missing(value)) {
        private$.inactive_characters
      } else {
        assert(
          checkNull(value),
          checkNumeric(value, min.len = 1, max.len = ncol(private$.matrix),
                       any.missing = FALSE, unique = TRUE)
        )
        ic <- rep(FALSE, ncol(private$.matrix))
        if (!is.null(value)) {
          ic[value] <- TRUE
        }
        private$.inactive_characters <- ic
      }
    },
    #' @field outgroup A single character vector indicating the taxon to be
    #'   the outgroup.
    outgroup = function (value) {
      if (missing(value)) {
        private$.outgroup
      } else {
        assert(
          checkNull(value),
          assert(
            checkCharacter(value, len = 1),
            checkSubset(value, rownames(private$.matrix)),
            combine = "and"
          )
        )
        if (is.null(value)) {
          value <- 1L
        } else {
          value <- which(value == rownames(private$.matrix))
        }
        private$.outgroup <- value
      }
    },
    #' @field collapse An integer indicating the rule for collapsing of zero
    #'   length branches. The options are:
    #'   \itemize{
    #'   \item \code{1}: collapse an interior branch of the maximum possible
    #'     length of the branch is zero;
    #'   \item \code{2}: keep zero length branches if ancestor and descendant
    #'     states differ;
    #'   \item \code{3}: collapse an interior branch if the minimum possible
    #'     length of the branch is zero (the default); and
    #'   \item \code{4}: discard all trees that must contain a zero length
    #'     branch.
    #'   }
    collapse = function (value) {
      if (missing(value)) {
        private$.collapse
      } else {
        assertInt(value, lower = 1, upper = 4)
        value <- asInt(value)
        private$.collapse <- value
      }
    },
    #' @field constraints A list of \code{"\link{NitroConstraint}"} objects.
    constraints = function (value) {
      if (missing(value)) {
        private$.constraints
      } else {
        print("`constraints` is read only; use `add_constraints` to add new
              constraints")
      }
    },
    #' @field weights An object inheriting from
    #'   \code{"\link{NitroWeightsBase}"}.
    weights = function (value) {
      if (missing(value)) {
        private$.weights
      } else {
        assert(
          checkNull(value),
          checkClass(value, "NitroWeightsBase")
        )
        private$.weights <- value
      }
    },
    #' @field start_trees A \code{phylo} or \code{multiPhylo} of trees to start
    #'   the analysis with. Required for analyses using
    #'   \code{"\link{NitroRatchet}"}.
    start_trees = function (value) {
      if (missing(value)) {
        private$.start_trees
      } else {
        assert(
          checkNull(value),
          assert(
            checkClass(value, "phylo"),
            checkClass(value, "multiPhylo")
          )
        )
        private$.start_trees <- value
      }
    },
    #' @field combine A logical indicating whether to merge any start trees
    #'   from \code{obj} with the trees obtained from the analysis, removing
    #'   any duplicates. This option is useful when a large number of trees, or
    #'   any number of trees with a large number of tips, from two separate
    #'   analyses (i.e., such as when a set of most parsimonious trees are
    #'   further explored with branch breaking) are to be combined, excluding
    #'   duplicates. TNT's method for identifying duplicate trees is
    #'   significantly more efficient than current R implementations (e.g.,
    #'   \code{\link[ape:unique.multiPhylo]{ape::unique.multiPhylo()}}).
    combine = function (value) {
      if (missing(value)) {
        private$.combine
      } else {
        assertLogical(value, len = 1)
        private$.combine <- value
      }
    },
    #' @field hold An integer indicating the number of trees to hold in TNT's
    #'   tree buffer.
    hold = function (value) {
      if (missing(value)) {
        private$.hold
      } else {
        assertInt(value, lower = 1)
        value <- asInt(value)
        private$.hold <- value
      }
    },
    #' @field max_ram A numeric indicating the number of (binary) megabytes to
    #'   allocate for use by TNT.
    #'   \code{"\link{NitroWeightsBase}"}.
    max_ram = function (value) {
      if (missing(value)) {
        private$.max_ram
      } else {
        assertNumber(value, lower = 0)
        private$.max_ram <- value
      }
    }
  ),
  public = list(
    #' @param matrix A \code{phyDat} object representing a character-taxon matrix
    #' @param method An object that inherits from class
    #'   \code{"\link{NitroMethodsBase}"}
    #' @param ordered_characters A numeric vector indicating the characters to
    #'   be treated as ordered.
    #' @param inactive_taxa A character vector indicating the taxa to be
    #'   inactivated.
    #' @param inactive_characters A numeric vector indicating the characters to
    #'   be inactivated.
    #' @param outgroup A single character vector indicating the taxon to be the
    #'   outgroup.
    #' @param collapse An integer indicating the rule for collapsing of zero
    #'   length branches. The options are:
    #'   \itemize{
    #'   \item \code{1}: collapse an interior branch of the maximum possible
    #'     length of the branch is zero;
    #'   \item \code{2}: keep zero length branches if ancestor and descendant
    #'     states differ;
    #'   \item \code{3}: collapse an interior branch if the minimum possible
    #'     length of the branch is zero (the default); and
    #'   \item \code{4}: discard all trees that must contain a zero length
    #'     branch.
    #'   }
    #' @param weights An object inheriting from \code{"\link{NitroWeightsBase}"}.
    #' @param start_trees A \code{phylo} or \code{multiPhylo} of trees to start
    #'   the analysis with. Required for analyses using
    #'   \code{"\link{NitroRatchet}"}.
    #' @param combine A logical indicating whether to merge any start trees
    #'   from \code{obj} with the trees obtained from the analysis, removing
    #'   any duplicates. This option is useful when a large number of trees, or
    #'   any number of trees with a large number of tips, from two separate
    #'   analyses (i.e., such as when a set of most parsimonious trees are
    #'   further explored with branch breaking) are to be combined, excluding
    #'   duplicates. TNT's method for identifying duplicate trees is
    #'   significantly more efficient than current R implementations (e.g.,
    #'   \code{\link[ape:unique.multiPhylo]{ape::unique.multiPhylo()}}).
    #' @param hold An integer indicating the number of trees to hold in TNTs tree
    #'   buffer.
    #' @param max_ram A numeric indicating the number of (binary) megabytes to
    #'   allocate for use by TNT.
    initialize = function (matrix, method, ordered_characters = NULL,
                           inactive_taxa = NULL, inactive_characters = NULL,
                           outgroup = NULL, collapse = 3, weights = NULL,
                           start_trees = NULL, combine = FALSE, hold = 100,
                           max_ram = 16) {
      a <- as.list(environment(), all = TRUE)
      assertClass(matrix, "phyDat")
      private$.matrix <- PhyDatToMatrix(matrix)
      a$matrix <- NULL
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cat("<NitroTreeSearch>\n")
      cat(paste("* Matrix:", nrow(private$.matrix), "taxa,",
                ncol(private$.matrix), "characters\n"))
      cat(paste("* Outgroup taxon:", rownames(private$.matrix)[private$.outgroup], "\n"))
      cat(paste("* Ordered characters:",
                ifelse(any(private$.ordered_characters),
                       paste(which(private$.ordered_characters), collapse=", "),
                       "None"), "\n"))
      cat(paste("* Inactive characters:",
                ifelse(any(private$.inactive_characters),
                       paste(which(private$.inactive_characters), collapse=", "),
                       "None"), "\n"))
      cat(paste("* Inactive taxa:",
                ifelse(any(private$.inactive_taxa),
                       paste(rownames(private$.matrix)[private$.inactive_taxa],
                             collapse=", "),
                       "None"), "\n"))
      if(!is.null(private$.constraints)) {
        cat(paste("* Constraints:", length(private$.constraints)))
      }
    },
    #' Add new constraint
    #'
    #' Function to add a new constraint to a constraint group.
    #' @param fixed_taxa a character vector of taxa to set as fixed constraints
    #' @param floating_taxa a character vector of taxa to set as floating
    #'   constraints
    #' @param type a character vector indicating whether the constraint will be
    #'   '\code{positive}' or '\code{negative}'
    add_constraint = function(fixed_taxa, floating_taxa = character(),
                               type = c("positive", "negative")) {
      is_positive <- match.arg(type) == "positive"
      mtx_taxa <- rownames(private$.matrix)
      all_c <- c(fixed_taxa, floating_taxa)
      if (!all(all_c %in% mtx_taxa)) {
        stop(paste("Could not find constraint taxa:",
                   paste(all_c[!all_c %in% mtx_taxa], sep = ", ")))
      }
      fixed_taxa <- mtx_taxa %in% fixed_taxa
      floating_taxa <- mtx_taxa %in% floating_taxa
      # Check if any constrained taxa are presently inactive
      in_fi <- and(fixed_taxa, private$.inactive_taxa)
      in_flo <- and(floating_taxa, private$.inactive_taxa)
      if (any(in_fi)) {
        stop("Requested fixed constraint contains inactive taxa: ",
             rownames(private$.matrix)[in_fi])
      }
      if (any(in_fi)) {
        stop("Requested floating constraint contains inactive taxa: ",
             rownames(private$.matrix)[in_flo])
      }
      constraint <- NitroConstraint$new(is_positive = is_positive,
                                        fixed = fixed_taxa, floating = floating_taxa)
      if (is.null(private$.constraints)) {
        private$.constraints <- c(constraint)
      } else {
        private$.constraints <- c(private$.constraints, constraint)
      }
      self
    }
  )
)
