#' Create a Character Matrix
#'
#' @description
#' Create a discrete or continuous character–taxon matrix object. The type
#' of matrix created is determined automatically from the class of `data`:
#' a [phyDat][phangorn::phyDat] object produces a [DiscreteMatrix], and a
#' `data.frame` produces a [ContinuousMatrix].
#'
#' @param data \[`phyDat` or `data.frame`\]\cr
#'   The character data. Supply a [phyDat][phangorn::phyDat] object for
#'   discrete characters, or a `data.frame` (with a `"taxon"` column and
#'   one numeric column per character) for continuous characters.
#' @param ... Optional named arguments passed to the constructor of the
#'   selected matrix class. See [DiscreteMatrix] and [ContinuousMatrix]
#'   for available parameters.
#'
#' @return A [DiscreteMatrix] or [ContinuousMatrix] object, depending on
#'   the class of `data`.
#'
#' @seealso
#' * [make_tree_analysis()] — accepts the matrix objects created here.
#' * [DiscreteMatrix] — for discrete character data; accepts `ordered` and
#'   `inactive` arguments.
#' * [ContinuousMatrix] — for continuous character data; accepts an
#'   `inactive` argument.
#' * [c.AbstractCharacterMatrix()] — combine multiple matrices for use in
#'   a [TreeAnalysis].
#' * [TreeAnalysis] — the analysis container that accepts matrix objects.
#'
#' @examples
#' library(TreeTools)
#'
#' # --- Discrete matrix ---
#' nex_path <- system.file("extdata", "canale_2022.nex", package = "nitro")
#' dm <- ReadAsPhyDat(nex_path) |> create_matrix()
#' dm
#'
#' # Mark some characters as ordered
#' dm$ordered <- c(1, 4, 9)
#'
#' # --- Continuous matrix ---
#' csv_path <- system.file("extdata", "raven_2017.csv", package = "nitro")
#' cm <- read.table(csv_path, sep = ",", header = TRUE) |> create_matrix()
#' cm
#'
#' # --- Combined discrete + continuous ---
#' nex_path2 <- system.file("extdata", "raven_2017.nex", package = "nitro")
#' dm2 <- ReadAsPhyDat(nex_path2) |> create_matrix()
#' combined <- c(dm2, cm)
#'
#' @importFrom checkmate assert check_class check_data_frame makeAssertCollection test_class test_true
#' @importFrom cli cli_abort
#' @export
create_matrix <- function(data, ...) {
  coll <- makeAssertCollection()
  assert(
    check_class(data, "phyDat"),
    check_data_frame(data)
  )
  val_check <- coll$getMessages()
  if (!coll$isEmpty()) {
    cli_abort(c("{.arg data} must be either a {.cls data.frame} or {.cls phyDat}.",
                "x" = val_check))
  }

  mtx_args <- list(...)
  mtx_args$data <- data

  if (test_class(data, "phyDat")) {
    new_mtx <- do.call(DiscreteMatrix$new, mtx_args)
  } else {
    new_mtx <- do.call(ContinuousMatrix$new, mtx_args)
  }
  new_mtx
}
