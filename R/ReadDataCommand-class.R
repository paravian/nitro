#' Read Data Command
#'
#' @description
#' An [R6][R6::R6Class] class that wraps one or more character–taxon matrix
#' objects and renders them as a TNT `xread` command for use within a
#' [TreeAnalysis] workflow.
#'
#' `ReadDataCommand` is created automatically when a matrix is supplied to
#' [TreeAnalysis] via its `$data` field. Users do not typically need to
#' instantiate this class directly.
#'
#' @details
#' ## Multiple matrices
#' Both [DiscreteMatrix] and [ContinuousMatrix] objects are supported, and
#' multiple matrices can be combined using [c.AbstractCharacterMatrix()]
#' before being passed to this command. All matrices must share at least
#' one taxon in common.
#'
#' ## Command output
#' `$render()` produces a multi-line `xread` block containing the matrix
#' dimensions, data type headers (e.g., `&[proteins]`), and the
#' character–taxon data in TNT format.
#'
#' ## Queue integration
#' Calling `$enqueue()` adds prerequisite commands before this command:
#' * A [TaxonNameCommand] at priority `210`.
#' * A [StateNumberCommand] at priority `200`.
#'
#' This command itself is added at priority `220`.
#'
#' @seealso
#' * [DiscreteMatrix], [ContinuousMatrix] — the matrix classes accepted by
#'   this command.
#' * [create_matrix()] — recommended way to create matrix objects.
#' * [TreeAnalysis] — creates this command automatically when `$data` is
#'   assigned.
#'
#' @keywords internal
#' @importFrom checkmate check_multi_class test_class test_disjunct test_null test_true
#' @importFrom cli cli_abort
#' @importFrom glue glue
#' @importFrom magrittr equals not
#' @importFrom R6 R6Class
#' @importFrom stringr str_remove str_to_lower
#' @importFrom TreeTools PhyDatToString
ReadDataCommand <- R6Class(
  "ReadDataCommand",
  inherit = BasicCommand,
  private = list(
    .data = NULL
  ),
  active = list(
    #' @field data \[`AbstractCharacterMatrix` or `MultiCharacterMatrix`\]\cr
    #'   One or more character matrix objects to be read. When a single
    #'   [AbstractCharacterMatrix] is supplied it is wrapped in a list
    #'   automatically. All matrices in a `MultiCharacterMatrix` must share
    #'   at least one taxon in common.
    data = function(value) {
      if (missing(value)) {
        return(private$.data)
      } else {
        val_check <- check_multi_class(
          value,
          c("AbstractCharacterMatrix", "MultiCharacterMatrix")
        )

        if (!test_true(val_check)) {
          cli_abort(c("{.arg data} must be an object that inherits {.cls AbstractCharacterMatrix}"))
        }

        if (test_class(value, "AbstractCharacterMatrix")) {
          value <- c(value)
        } else {
          n_mtx <- length(value)
          for (comb_a in seq(n_mtx - 1)) {
            mtx_a <- value[comb_a]
            mtx_match <- c()
            for (comb_b in (comb_a + 1):n_mtx) {
              mtx_b <- value[comb_b]
              taxa_match <- test_disjunct(mtx_a$taxa, mtx_b$taxa)
              mtx_match <- c(mtx_match, taxa_match)
            }

            if (!all(mtx_match)) {
              cli_abort(c("Matrices in {.arg data} must have at least one taxon in common."))
            }
          }
        }

        is_continuous <- sapply(value, getElement, "data_type") %>%
          equals("continuous")
        if (any(is_continuous)) {
          mtx_idx <- not(is_continuous) %>%
            as.numeric() %>%
            order()
          value <- Reduce(c, value[mtx_idx])
        }

        private$.data <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Add prerequisite commands and this command to a [CommandQueue].
    #'
    #' Enqueues a `TaxonNameCommand` and a `StateNumberCommand` before
    #' adding this command at priority `220`.
    #'
    #' @param .queue A [CommandQueue] object, or `NULL` to create a new
    #'   one.
    #'
    #' @return A [CommandQueue] object.
    enqueue = function(.queue = NULL) {
      .queue <- super$enqueue(.queue)

      tax_name <- TaxonNameCommand$new(
        use_names = FALSE
      )
      tax_name$enqueue(.queue)

      max_states <- sapply(self$data, getElement, "n_states") %>%
        unlist()
      if (!test_null(max_states)) {
        max_states <- max(max_states)
      }

      any_continuous <- sapply(self$data, test_class, "ContinuousMatrix") %>%
        any()

      state_num <- StateNumberCommand$new(max_states, any_continuous)
      state_num$enqueue(.queue)

      .queue$add(self, 220)
      .queue
    },
    #' @description
    #' Format the command as a summary table.
    #'
    #' Returns a data frame showing the matrix types and the number of
    #' inactive taxa.
    #'
    #' @param ... Not used.
    #'
    #' @return A `data.frame` with columns for description, current value,
    #'   and default value.
    format = function(...) {
      which_mtx <- sapply(self$data, function(x) class(x)[1]) %>%
        table()
      names(which_mtx) <- names(which_mtx) %>%
        str_to_lower() %>%
        str_remove("matrix")
      which_mtx <- glue("{which_mtx} {names(which_mtx)}") %>%
        paste(collapse = ", ")

      options <- data.frame(
        "Character matrices:",
        which_mtx,
        ""
      )
      names(options) <- c("", "Current value", "Default value")
      options
    },
    #' @description
    #' Create a new `ReadDataCommand` object.
    #'
    #' This command is created automatically by [TreeAnalysis] when `$data`
    #' is assigned. Direct instantiation is rarely necessary.
    #'
    #' @param data \[`AbstractCharacterMatrix` or `MultiCharacterMatrix`\]\cr
    #'   One or more character matrix objects. See the `$data` field.
    #'
    #' @return A new `ReadDataCommand` object.
    initialize = function(data, ...) {
      a <- as.list(environment(), all = TRUE) %>%
        head(-1)

      super$initialize(
        name = "xread",
        description = "Read character-taxon matrix data",
        provides = "matrix",
        ...
      )

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Render the TNT `xread` command block.
    #'
    #' Produces a multi-line character vector containing the matrix
    #' dimensions, data type headers for non-numeric matrices, and the
    #' character–taxon data in TNT format.
    #'
    #' @param ... Not used.
    #'
    #' @return A character vector of TNT command lines.
    render = function(...) {
      all_taxa <- sapply(self$data, getElement, "taxa") %>%
        as.vector() %>%
        unique()

      n_char <- sapply(self$data, getElement, "n_characters") %>%
        sum()

      args <- paste(n_char, length(all_taxa))

      for (mtx in self$data) {
        header <- paste("&[", mtx$data_type, "]", sep = "")
        args <- c(
          args,
          header
        )

        if (test_class(mtx, "DiscreteMatrix")) {
          tax_names <- names(mtx$data)
          taxa <- PhyDatToString(mtx$data, parentheses = "[", concatenate = FALSE)
        } else {
          tax_names <- rownames(mtx$data)
          taxa <- apply(mtx$data, 1, paste, collapse = " ")
        }
        tax_names <- format(tax_names, justify = "left")
        taxa <- paste(tax_names, taxa)

        args <- c(
          args,
          taxa
        )
      }

      cmd <- c(self$name, args, ";")
      cmd
    }
  )
)
