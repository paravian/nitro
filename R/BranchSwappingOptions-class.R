#' Define a branch swapping analysis
#'
#' @description
#' \code{BranchSwappingOptions} is an R6 class that defines the set of options
#' for performing a branch sweapping phylogenetic analysis with replications
#' in \code{nitro}.
#' @importFrom checkmate asInt assertInt assertLogical
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom R6 R6Class
#' @export
BranchSwappingOptions <- R6Class("BranchSwappingOptions",
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
      cli_text("{col_grey(\"# A TNT branch swap configuration\")}")

      options <- c("Replications:" = self$replications,
                   "Trees to hold per replicate:" = self$hold_rep,
                   "Keeping all trees:" = ifelse(self$keep_all, "yes", "no")) %>%
        data.frame()

      colnames(options) <- NULL
      print(options)
    },
    #' @param ... Ignored.
    queue = function (...) {
      queue <- CommandQueue$new()
      branchswap_opts <- glue("= replic {self$replications} hold {self$hold_rep} {ifelse(self$keep_all, \"\", \"no\")}keepall")

      queue$add("mult", branchswap_opts)
      return(queue)
    }
  )
)
