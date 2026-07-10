#' Discrete Character Matrix
#'
#' @description
#' An [R6][R6::R6Class] class that stores a discrete character–taxon matrix
#' and provides options for controlling character activity and ordering.
#'
#' The matrix is stored internally as a [phyDat][phangorn::phyDat] object.
#' The recommended way to create a `DiscreteMatrix` is via [create_matrix()],
#' which automatically selects the correct class based on the input data type.
#'
#' @details
#' ## Data types
#' The `$data_type` field is derived automatically from the `phyDat` object
#' and will be one of:
#'
#' | Value | Description |
#' |-------|-------------|
#' | `"dna"` | DNA sequence data. |
#' | `"rna"` | RNA sequence data. |
#' | `"proteins"` | Amino acid sequence data. |
#' | `"numeric"` | User-defined numeric states (morphological data). |
#'
#' ## Character ordering
#' Character ordering (`$ordered`) is only supported for matrices with
#' `data_type = "numeric"`. Attempting to set `$ordered` on other data
#' types raises an error.
#'
#' @seealso
#' * [create_matrix()] — recommended constructor; automatically creates a
#'   `DiscreteMatrix` when supplied a `phyDat` object.
#' * [make_tree_analysis()] — accepts these objects via its `data` argument.
#' * [ContinuousMatrix] — the equivalent class for continuous characters.
#' * [TreeAnalysis] — the analysis container that accepts matrix objects.
#'
#' @examples
#' library(TreeTools)
#'
#' # Read a discrete character matrix from a Nexus file
#' nex_path <- system.file("extdata", "canale_2022.nex", package = "nitro")
#' dm <- ReadAsPhyDat(nex_path) |> create_matrix()
#'
#' # Inspect the matrix
#' dm
#'
#' # Mark characters 3 and 7 as ordered
#' dm$ordered <- c(3, 7)
#'
#' # Deactivate characters 1 and 2
#' dm$inactive <- c(1, 2)
#'
#' @importFrom checkmate asInt assert check_class check_logical check_null check_numeric check_subset makeAssertCollection test_null test_true
#' @importFrom cli cli_abort cli_text col_grey col_red style_italic
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom stringr str_pad str_replace_all str_to_lower
#' @importFrom tibble as_tibble
#' @importFrom TreeTools PhyDatToMatrix PhyDatToString
#' @export
DiscreteMatrix <- R6Class(
  "DiscreteMatrix",
  inherit = AbstractCharacterMatrix,
  private = list(
    .is_ordered = NULL,
    .n_states = NULL,
    .symbols = NULL
  ),
  active = list(
    #' @field data \[`phyDat`\]\cr
    #'   *(Write-once.)* The character matrix as a
    #'   [phyDat][phangorn::phyDat] object. Setting this field also
    #'   populates `$taxa`, `$n_characters`, `$n_states`, `$symbols`, and
    #'   `$data_type` automatically.
    data = function(value) {
      if (missing(value)) {
        return(private$.data)
      } else {
        if (test_null(private$.data)) {
          val_check <- check_class(value, "phyDat")
          if (!test_true(val_check)) {
            cli_abort(c("Matrix must be a {.cls phyDat} object."),
              "x" = val_check
            )
          }

          private$.data <- value
          private$.n_states <- attr(value, "nc")
          private$.n_characters <- length(attr(value, "index"))
          private$.symbols <- attr(value, "levels") %>%
            as.character()
          private$.taxa <- names(value)

          data_type <- attr(value, "type") %>%
            str_to_lower()
          if (data_type == "user") {
            noncoding <- c("?", "-")
            symbols <- value$symbols %>%
              {
                .[!. %in% noncoding]
              }
            val_check <- check_subset(symbols, as.character(0:9))
            if (!test_true(val_check)) {
              val_check <- str_replace_all(val_check, "([\\{\\}])", "\\1\\1")
              cli_abort(c("Discrete character matrices with user-defined symbols must be numeric.",
                "x" = val_check
              ))
            }
            data_type <- "numeric"
          } else if (data_type == "aa") {
            data_type <- "proteins"
          }

          private$.data_type <- data_type
        } else {
          cli_abort(c("{.arg data} is a read-only attribute."))
        }
      }
    },
    #' @field n_states \[`integer(1)`\]\cr
    #'   *(Read-only.)* The number of unique character states in the matrix.
    n_states = function(value) {
      if (missing(value)) {
        return(private$.n_states)
      } else {
        cli_abort(c("{.arg n_states} is a read-only attribute."))
      }
    },
    #' @field symbols \[`character`\]\cr
    #'   *(Read-only.)* The unique character state symbols present in the
    #'   matrix.
    symbols = function(value) {
      if (missing(value)) {
        return(private$.symbols)
      } else {
        cli_abort(c("{.arg symbols} is a read-only attribute."))
      }
    },
    #' @field ordered \[`numeric` or `NULL`\]\cr
    #'   The indices of characters to mark as ordered. Must be unique
    #'   integer indices between 1 and `$n_characters`. Only applicable
    #'   when `$data_type` is `"numeric"`. Set to `NULL` to mark all
    #'   characters as unordered. Returns `NULL` when no characters are
    #'   ordered.
    ordered = function(value) {
      if (missing(value)) {
        if (any(private$.is_ordered)) {
          return(which(private$.is_ordered))
        }
        return(NULL)
      } else {
        n_chars <- attr(private$.data, "index") %>%
          length()
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_numeric(value,
            min.len = 1, lower = 1, upper = n_chars,
            unique = TRUE, any.missing = FALSE
          ),
          add = coll
        )
        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg ordered} must contain valid unique character indices.",
            "x" = val_check
          ))
        }

        if (self$data_type != "numeric" & !test_null(value)) {
          cli_abort(c("Ordering can only be applied to a matrix with numeric data type."))
        }

        is_ordered <- rep(FALSE, self$n_characters)
        if (!is.null(value)) {
          is_ordered[value] <- TRUE
        }
        private$.is_ordered <- is_ordered
      }
    }
  ),
  public = list(
    #' @description
    #' Format the matrix as a summary table.
    #'
    #' @param ... Not used.
    #'
    #' @return A `data.frame` summarising the matrix properties.
    format = function(...) {
      log_lists <- list(ordered = self$ordered, inactive = self$inactive) %>%
        {
          lapply(., function(x) ifelse(is.null(x), 0, length(x)))
        }

      options <- data.frame(
        c(
          "Data type:",
          "Number of taxa:",
          "Number of characters:",
          "Number of inactive characters:"
        ),
        c(
          self$data_type,
          length(private$.data),
          length(attr(private$.data, "index")),
          log_lists$inactive
        )
      )

      if (self$data_type == "numeric") {
        options <- rbind(
          options,
          c("Number of ordered characters", log_lists$ordered)
        )
      }

      names(options) <- c("", "Current value")
      options[, 1] <- format(options[, 1], justify = "left")
      options
    },
    #' @description
    #' Create a new `DiscreteMatrix` object.
    #'
    #' It is recommended to use [create_matrix()] rather than calling this
    #' constructor directly.
    #'
    #' @param data \[`phyDat`\]\cr
    #'   A [phyDat][phangorn::phyDat] discrete character matrix. See the
    #'   `$data` field.
    #' @param ordered \[`numeric` or `NULL`\]\cr
    #'   Indices of characters to mark as ordered (default: `NULL`). Only
    #'   applicable for `"numeric"` data type. See the `$ordered` field.
    #' @param inactive \[`numeric` or `NULL`\]\cr
    #'   Indices of characters to mark as inactive (default: `NULL`). See
    #'   the `$inactive` field.
    #'
    #' @return A new `DiscreteMatrix` object.
    initialize = function(data, ordered = NULL, inactive = NULL) {
      a <- as.list(environment(), all = TRUE)

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Print a summary of the matrix.
    #'
    #' @param ... Not used.
    print = function(...) {
      cli_text(col_grey("# A ", style_italic(col_red("nitro")), " discrete matrix"))

      options <- format(self)
      names(options) <- NULL
      print(options, row.names = FALSE)
    }
  )
)
