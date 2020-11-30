#' Syntactic Sugar for Search Method Construction
#'
#' A shortcut function for retrieving tree search objects (i.e., those
#' inheriting from \code{"\link{NitroMethodsBase}"} and search parameter
#' definition.
#' @param name The name of a search method. Possible values are:
#'   \itemize{
#'     \item \code{\link[nitro:NitroBootstrap]{Bootstrap}}
#'     \item \code{\link[nitro:NitroBranchBreak]{BranchSwap}}
#'     \item \code{\link[nitro:NitroBranchSwap]{BranchSwap}}
#'     \item \code{\link[nitro:NitroConstraintSectorialSearch]{ConstraintSectorialSearch}}
#'     \item \code{\link[nitro:NitroDriven]{Driven}}
#'     \item \code{\link[nitro:NitroImplicitEnum]{ImplicitEnum}}
#'     \item \code{\link[nitro:NitroJackknife]{Jackknife}}
#'     \item \code{\link[nitro:NitroRandomSectorialSearch]{RandomSectorialSearch}}
#'     \item \code{\link[nitro:NitroRatchet]{Ratchet}}
#'     \item \code{\link[nitro:NitroSymmetricResample]{SymmetricResample}}
#'     \item \code{\link[nitro:NitroTreeDrift]{TreeDrift}}
#'     \item \code{\link[nitro:NitroTreeFuse]{TreeFuse}}
#'     \item \code{\link[nitro:NitroTreeHybridize]{TreeHybridize}}
#'   }
#' @param ... Named arguments passed to the constructor for the search method,
#'   to be set as parameters.
#' @importFrom checkmate assertChoice
#' @importFrom data.table data.table
#' @export
schmd <- function (name, ...) {
  options <- data.table(
    ids = c("Bootstrap", "BranchBreak", "BranchSwap",
            "ConstraintSectorialSearch", "Driven", "ImplicitEnum",
            "Jackknife", "RandomSectorialSearch", "Ratchet",
            "SymmetricResample", "TreeDrift", "TreeFuse", "TreeHybridize"),
    objs = c(NitroBootstrap, NitroBranchBreak, NitroBranchSwap,
             NitroConstraintSectorialSearch, NitroDriven, NitroImplicitEnum,
             NitroJackknife, NitroRandomSectorialSearch, NitroRatchet,
             NitroSymmetricResample, NitroTreeDrift, NitroTreeFuse,
             NitroTreeHybridize)
  )
  assertChoice(name, options$ids)
  method <- options$objs[options$ids == name][[1]]
  method$new(...)
}
