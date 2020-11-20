#' Define a branch swapping analysis
#'
#' @description
#' \code{NitroBranchSwap} is an R6 class that defines the set of parameters
#' required to perform a branch swapping ('traditional', in TNTs terminology)
#' phylogenetic analysis in \code{nitro}.
#' @importFrom checkmate asInt assertInt assertLogical
#' @importFrom R6 R6Class
#' @export
NitroBranchSwap <- R6Class("NitroBranchSwap",
  inherit = NitroMethodsBase,
  private = list(
    .replications = NULL,
    .hold_rep = NULL,
    .keep_all = NULL
  ),
  active = list(
    #' @field replications An integer value indicating the number of replications.
    replications = function (value) {
      if (missing(value)) {
        private$.replications
      } else {
        assertInt(value, lower = 1)
        value <- asInt(value)
        private$.replications <- value
      }
    },
    #' @field hold_rep An integer value indicating the maximum number of trees to
    #'   retain during each replication.
    hold_rep = function (value) {
      if (missing(value)) {
        private$.hold_rep
      } else {
        assertInt(value, lower = 1)
        value <- asInt(value)
        private$.hold_rep <- value
      }
    },
    #' @field keep_all A logical value indicating whether to retain all generated
    #'   trees from each replication regardless of length.
    keep_all = function (value) {
      if (missing(value)) {
        private$.keep_all
      } else {
        assertLogical(value)
        private$.keep_all <- value
      }
    }
  ),
  public = list(
    #' @param replications An integer value indicating the number of replications.
    #' @param hold_rep An integer value indicating the maximum number of trees to
    #'   retain during each replication.
    #' @param keep_all A logical value indicating whether to retain all generated
    #'   trees from each replication regardless of length.
    initialize = function (replications = 10, hold_rep = 10,
                           keep_all = FALSE) {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cat("<NitroBranchSwap>\n")
      cat(paste("* RAS replications:", private$.replications, "\n"))
      cat(paste("* Trees to hold per replicate:", private$.hold_rep, "\n"))
      cat(paste("* Keep all trees:", private$.keep_all, "\n"))
    },
    #' @param ... Ignored.
    tnt_cmd = function (...) {
      paste0("mult= replic ", private$.replications, " hold ", private$.hold_rep,
             ifelse(private$.keep_all, " ", " no"), "keepall;")
    }
  )
)
