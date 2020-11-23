#' Define analysis results
#'
#' @description
#' \code{NitroTrees} is an R6 class that contains the
#' \code{"\link{NitroTreeSearch}"} defining the parameters for an
#' analysis the set of trees that the analysis returned.
#' @importFrom checkmate assert assertClass assertNames assertSubset checkClass
#' @importFrom ape .compressTipLabel
#' @importFrom R6 R6Class
#' @export
NitroTrees <- R6Class("NitroTrees",
  private = list(
    .tree_search = NULL,
    .trees = NULL
  ),
  active = list(
    #' @field tree_search An object that inherits
    #'   \code{"\link{NitroTreeSearch}"}.
    tree_search = function (value) {
      if (missing(value)) {
        private$.tree_search
      } else {
        assertClass(value, "NitroTreeSearch")
        # Check that the matrix provided contains a subset of taxa from the
        # tree(s)
        if (!is.null(private$.trees)) {
          if (inherits(private$.trees, "phylo")) {
            assertSubset(rownames(value$matrix), private$.trees$tipLabel)
          } else {
            assertSubset(rownames(value$matrix), private$.trees[[1]]$tipLabel)
          }
        }
        private$.tree_search <- value
      }
    },
    #' @field trees A \code{phylo} or \code{multiPhylo} object.
    trees = function (value) {
      if (missing(value)) {
        private$.trees
      } else {
        assert(
          checkClass(value, "phylo"),
          checkClass(value, "multiPhylo")
        )

        # Check that the tree(s) provided contains a subset of taxa from the
        # matrix
        if (!is.null(private$.tree_search)) {
          if (inherits(value, "phylo")) {
            assertSubset(value$tipLabel, rownames(private$.tree_search$matrix))
          } else {
            assertSubset(value[[1]]$tip.label,
                         rownames(private$.tree_search$matrix))
            # Check that all trees have the same tips
            value <- .compressTipLabel(value)
          }
        }
        private$.trees <- value
      }
    }
  ),
  public = list(
    #' @param tree_search An object that inherits
    #'   \code{"\link{NitroTreeSearch}"}.
    #' @param trees A \code{phylo} or \code{multiPhylo} object.
    initialize = function (tree_search, trees) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cat("<NitroTrees>\n")
      cat("* Number of trees:", ifelse(inherits(private$.trees, "phylo"), 1, length(private$.trees)))
    },
    #' @param ... Ignored.
    tnt_cmd = function (...) {
      mtx_tips <- rownames(private$.tree_search$matrix)

      phy <- private$.trees
      if (inherits(phy, "phylo")) {
        phy <- c(phy)
        class(phy) <- "multiPhylo"
      }
      tnt_trees <- sapply(phy, function (p) {
        res_tips <- p$tip.label
        tips_match <- match(res_tips, mtx_tips)
        res_parens <- strsplit(as.Newick(p), "[0-9]+")[[1]]
        rbind(c("", as.character(tips_match - 1)), res_parens) %>%
          as.vector %>%
          paste(collapse = "") %>%
          gsub("([0-9]+)", "\\1 ", .) %>%
          gsub("[,;]", "", .) %>%
          gsub("\\) \\(", "\\)\\(", .)
      })
      tnt_trees <- paste(tnt_trees, "*")
      tnt_trees[length(tnt_trees)] <- sub("\\*$", ";", tnt_trees[length(tnt_trees)])
      tnt_trees <- c("tread", tnt_trees)
      tnt_trees
    }
  )
)
