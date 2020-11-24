#' Define a driven search
#'
#' @description
#' \code{NitroBranchSwap} is an R6 class that defines the set of parameters
#' required to perform a driven search in \code{nitro}.
#' @importFrom R6 R6Class
#' @importFrom checkmate asInt assert assertClass assertInt
#'   assertLogical checkClass checkList checkNull
#' @export
NitroDriven <- R6Class("NitroDriven",
  inherit = NitroMethodsBase,
  private = list(
    .replications = NULL,
    .hits = NULL,
    .consense_times = NULL,
    .keep_all = NULL,
    .multiply = NULL,
    .sectorial_search = NULL,
    .tree_fuse = NULL,
    .tree_hybridize = NULL,
    .tree_drift = NULL,
    .ratchet = NULL),
  active = list(
    #' @field replications An integer value indicating the number of replications.
    replications = function (value) {
      if (missing(value)) {
        private$.replications
      } else {
        assertInt(value, lower = 1)
        private$.replications <- asInt(value)
        self
      }
    },
    #' @field hits An integer value indicating the number of times the shortest
    #'   tree must be found on consecutive re-runs of the analysis before stopping.
    hits = function (value) {
      if (missing(value)) {
        private$.hits
      } else {
        assertInt(value, lower = 1)
        private$.hits <- asInt(value)
        self
      }
    },
    #' @field consense_times An integer value indicating the number of times to
    #'   consense until the consensus is stablilised.
    consense_times = function (value) {
      if (missing(value)) {
        private$.consense_times
      } else {
        assertInt(value, lower = 0)
        private$.consense_times <- asInt(value)
        self
      }
    },
    #' @field keep_all A logical value indicating whether to retain all generated
    #'   trees from each replication regardless of length. This has a different
    #'   meaning when \code{hits} = 1 and when \code{hits} > 1. When
    #'   \code{hits} = 1, it is trees from each of the RAS + TBR +  SS or DFT or
    #'   RAT, in addition to the trees resulting from fusing those. When
    #'   \code{hits} > 1, then it means the trees resulting from fusing the
    #'   initial starting trees for each of starting points.
    keep_all = function (value) {
      if (missing(value)) {
        private$.keep_all
      } else {
        assertLogical(value, len = 1, any.missing = FALSE)
        private$.keep_all <- value
        self
      }
    },
    #' @field multiply A logical value indicating whether to find additional trees
    #'   by fusing suboptimal trees with optimal trees.
    multiply = function (value) {
      if (missing(value)) {
        private$.multiply
      } else {
        assertLogical(value, len = 1, any.missing = FALSE)
        private$.multiply <- value
        self
      }
    },
    #' @field sectorial_search A list of objects of inheriting
    #'   \code{"\link{NitroSectorialSearch}"}.
    sectorial_search = function (value) {
      if (missing(value)) {
        private$.sectorial_search
      } else {
        assert(
          checkNull(value),
          checkClass(value, "NitroSectorialSearch"),
          checkList(value, types = "NitroSectorialSearch")
        )
        if (is.null(value)) {
          value <- list(NitroRandomSectorialSearch$new(),
                        NitroConstraintSectorialSearch$new())
        }
        private$.sectorial_search <- value
        self
      }
    },
    #' @field tree_fuse An object of class \code{"\link{NitroTreeFuse}"}.
    tree_fuse = function (value) {
      if (missing(value)) {
        private$.tree_fuse
      } else {
        assert(
          checkNull(value),
          checkClass(value, "NitroTreeFuse")
        )
        if (is.null(value)) {
          value <- NitroTreeFuse$new()
        }
        private$.tree_fuse <- value
        self
      }
    },
    #' @field tree_hybridize An object of class \code{"\link{NitroTreeHybridize}"}.
    tree_hybridize = function (value) {
      if (missing(value)) {
        private$.tree_hybridize
      } else {
        assert(
          checkNull(value),
          checkClass(value, "NitroTreeHybridize")
        )
        if (is.null(value)) {
          value <- NitroTreeHybridize$new(rounds = 0)
        }
        private$.tree_hybridize <- value
        self
      }
    },
    #' @field tree_drift An object of class \code{"\link{NitroTreeDrift}"}.
    tree_drift = function (value) {
      if (missing(value)) {
        private$.tree_drift
      } else {
        assert(
          checkNull(value),
          checkClass(value, "NitroTreeDrift")
        )
        if (is.null(value)) {
          value <- NitroTreeDrift$new(iterations = 5)
        }
        private$.tree_drift <- value
        self
      }
    },
    #' @field ratchet An object of class \code{"\link{NitroRatchet}"}.
    ratchet = function (value) {
      if (missing(value)) {
        private$.ratchet
      } else {
        assert(
          checkNull(value),
          checkClass(value, "NitroRatchet")
        )
        if (is.null(value)) {
          value <- NitroRatchet$new(iterations = 0)
        }
        private$.ratchet <- value
        self
      }
    }
  ),
  public = list(
    #' @param replications an integer value indicating the number of replications.
    #' @param hits An integer value indicating the number of times the shortest
    #'   tree must be found on consecutive re-runs of the analysis before stopping.
    #' @param consense_times An integer value indicating the number of times to
    #'   consense until the consensus is stablilised.
    #' @param keep_all A logical value indicating whether to retain all generated
    #'   trees from each replication regardless of length. This has a different
    #'   meaning when \code{hits} = 1 and when \code{hits} > 1. When
    #'   \code{hits} = 1, it is trees from each of the RAS + TBR +  SS or DFT or
    #'   RAT, in addition to the trees resulting from fusing those. When
    #'   \code{hits} > 1, then it means the trees resulting from fusing the
    #'   initial starting trees for each of starting points.
    #' @param multiply A logical value indicating whether to find additional trees
    #'   by fusing suboptimal trees with optimal trees.
    #' @param sectorial_search A list of objects of inheriting
    #'   \code{"\link{NitroSectorialSearch}"}.
    #' @param tree_fuse An object of class \code{"\link{NitroTreeFuse}"}.
    #' @param tree_hybridize An object of class \code{"\link{NitroTreeHybridize}"}.
    #' @param tree_drift An object of class \code{"\link{NitroTreeDrift}"}.
    #' @param ratchet An object of class \code{"\link{NitroRatchet}"}.
   initialize = function (replications = 4, hits = 1, consense_times = 0,
                          keep_all = FALSE, multiply = TRUE,
                          sectorial_search = NULL, tree_fuse = NULL,
                          tree_hybridize = NULL, tree_drift = NULL,
                          ratchet = NULL) {
     a <- as.list(environment(), all = TRUE)
     for (n in names(a)) {
       self[[n]] <- a[[n]]
     }
   },
   #' @param ... Ignored.
   print = function (...) {
     cat("<NitroDriven>\n")
     cat(paste("* Replications:", private$.replications, "\n"))
     cat(paste("* Hits:", private$.hits, "\n"))
     if (private$.consense_times > 0) {
       cat(paste("* Consense times:", private$.consense_times, "\n"))
     }
     cat(paste("* Keep all trees:", private$.keep_all, "\n"))
     cat(paste("* Multiply trees by fusing:", private$.multiply, "\n"))
   },
   #' @param ... Ignored.
   tnt_cmd = function (...) {
     driven_cmd <- c()
     if (length(private$.sectorial_search)) {
       driven_cmd <- c(driven_cmd,
                       sapply(private$.sectorial_search,
                              function (s) s$tnt_cmd(set_only = TRUE)))
     }
     if (private$.tree_fuse$rounds > 0) {
       driven_cmd <- c(driven_cmd, private$.tree_fuse$tnt_cmd())
     }
     if (private$.tree_hybridize$rounds > 0) {
       driven_cmd <- c(driven_cmd, private$.tree_hybridize$tnt_cmd())
     }
     if (private$.tree_drift$iterations > 0) {
       driven_cmd <- c(driven_cmd, private$.tree_drift$tnt_cmd(set_only = TRUE))
     }
     if (private$.ratchet$iterations > 0) {
       driven_cmd <- c(driven_cmd, private$.ratchet$tnt_cmd(set_only = TRUE))
     }
     sect_classes <- sapply(private$.sectorial_search, class)
     driven_cmd <- c(driven_cmd,
                     paste0("xmult= hits ", private$.hits,
                            " replications ", private$.replications,
                            ifelse("NitroRandomSectorialSearch" %in% sect_classes,
                                   " rss", " norss"),
                            ifelse("NitroConstraintSectorialSearch" %in% sect_classes,
                                   " css", " nocss"),
                            ifelse(private$.tree_fuse$rounds == 0, " nofuse",
                                   paste(" fuse", private$.tree_fuse$rounds)),
                            ifelse(private$.tree_hybridize$rounds == 0, " nohybrid",
                                   " hybrid"),
                            ifelse(private$.tree_drift$iterations == 0, " nodrift",
                                   paste(" drift", private$.tree_drift$iterations)),
                            ifelse(private$.ratchet$iterations == 0, " noratchet",
                                   paste(" ratchet", private$.ratchet$iterations)),
                            ifelse(private$.consense_times == 0, " noconsense",
                                   paste(" consense", private$.consense_times)),
                            ifelse(private$.keep_all, " keepall", " nokeepall"),
                            ifelse(private$.multiply, " multiply", " nomultiply"), ";"))
     driven_cmd
   }
  )
)
