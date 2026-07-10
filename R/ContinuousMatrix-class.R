#' Continuous Character Matrix
#'
#' @description
#' An [R6][R6::R6Class] class that stores a continuous character–taxon matrix
#' and provides options for controlling character activity.
#'
#' The matrix is stored internally as a `data.frame` with one row per taxon
#' and one column per character. The recommended way to create a
#' `ContinuousMatrix` is via [create_matrix()], which automatically selects
#' the correct class based on the input data type.
#'
#' @details
#' ## Input format
#' The `data.frame` supplied to `$data` must have:
#' * A column named `"taxon"` containing taxon names.
#' * All remaining columns numeric, each representing one character.
#'
#' Whitespace in taxon names is replaced with underscores. Where a taxon
#' appears in multiple rows, values are summarised as a range
#' (e.g., `"1.2-3.4"`); missing values are encoded as `"?"`.
#'
#' @seealso
#' * [create_matrix()] — recommended constructor; automatically creates a
#'   `ContinuousMatrix` when supplied a `data.frame`.
#' * [make_tree_analysis()] — accepts these objects via its `data` argument.
#' * [DiscreteMatrix] — the equivalent class for discrete characters.
#' * [TreeAnalysis] — the analysis container that accepts matrix objects.
#'
#' @examples
#' # Read a continuous character matrix from a CSV file
#' csv_path <- system.file("extdata", "raven_2017.csv", package = "nitro")
#' cm <- read.table(csv_path, sep = ",", header = TRUE) |> create_matrix()
#'
#' # Inspect the matrix
#' cm
#'
#' # Deactivate character 1
#' cm$inactive <- 1
#'
#' @importFrom checkmate asInt assert check_data_frame check_null check_numeric makeAssertCollection test_null test_true
#' @importFrom cli cli_abort cli_text col_grey col_red style_italic
#' @importFrom dplyr across everything group_by mutate summarise where
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom stats na.omit
#' @importFrom stringr str_pad str_replace_all str_replace_na
#' @export
ContinuousMatrix <- R6Class(
  "ContinuousMatrix",
  inherit = AbstractCharacterMatrix,
  active = list(
    #' @field data \[`data.frame`\]\cr
    #'   *(Write-once.)* The character matrix as a `data.frame` with taxon
    #'   names as row names and one column per character. Setting this field
    #'   also populates `$taxa` and `$n_characters` automatically.
    data = function(value) {
      if (missing(value)) {
        return(private$.data)
      } else {
        if (test_null(private$.data)) {
          val_check <- check_data_frame(value,
            min.rows = 1, min.cols = 1,
            row.names = "named"
          )
          if (!test_true(val_check)) {
            cli_abort(c("{.arg data} must be a {.cls data.frame}.",
              "x" = val_check
            ))
          }

          taxon_col <- names(value) == "taxon"
          if (sum(taxon_col) != 1) {
            cli_abort(c("{.arg data} must have one column named {.val taxon}."))
          }

          val_check <- check_data_frame(value[-taxon_col], types = "numeric")
          if (!test_true(val_check)) {
            cli_abort(c("{.arg data} must contain columns of numeric type for all characters.",
              "x" = val_check
            ))
          }

          value[, taxon_col] <- str_replace_all(value[, taxon_col], "\\s+", "_")

          if (any(duplicated(value[, taxon_col]))) {
            value <- group_by(value, "taxon") %>%
              summarise(across(everything(), function(x) {
                x <- na.omit(x)
                if (length(x) > 0) {
                  x <- round(3) %>%
                    range(x) %>%
                    unique() %>%
                    paste(collapse = "-")
                } else {
                  x <- "?"
                }
                return(x)
              }))
          } else {
            value <- mutate(value, across(where(is.numeric), ~ str_replace_na(.x, "?")))
          }

          rownames(value) <- value[, taxon_col]
          value[, taxon_col] <- NULL

          private$.data <- value
          private$.n_characters <- length(value)
          private$.taxa <- rownames(value)
        } else {
          cli_abort(c("{.arg data} is a read-only attribute."))
        }
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
      n_inactive <- self$inactive %>%
        {
          ifelse(test_null(.), 0, length(.))
        }

      options <- data.frame(
        c(
          "Number of taxa:",
          "Number of characters:",
          "Number of inactive characters:"
        ),
        c(
          length(private$.taxa),
          length(private$.data),
          n_inactive
        )
      )

      names(options) <- c("", "Current value")
      options[, 1] <- format(options[, 1], justify = "left")
      options
    },
    #' @description
    #' Create a new `ContinuousMatrix` object.
    #'
    #' It is recommended to use [create_matrix()] rather than calling this
    #' constructor directly.
    #'
    #' @param data \[`data.frame`\]\cr
    #'   A `data.frame` representing a continuous character matrix, with a
    #'   `"taxon"` column and one numeric column per character. See the
    #'   `$data` field.
    #' @param inactive \[`numeric` or `NULL`\]\cr
    #'   Indices of characters to mark as inactive (default: `NULL`). See
    #'   the `$inactive` field.
    #'
    #' @return A new `ContinuousMatrix` object.
    initialize = function(data, inactive = NULL) {
      a <- as.list(environment(), all = TRUE)

      private$.data_type <- "continuous"
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Print a summary of the matrix.
    #'
    #' @param ... Not used.
    print = function(...) {
      cli_text(col_grey("# A ", style_italic(col_red("nitro")), " continuous matrix"))

      options <- format(self)
      names(options) <- NULL
      print(options, row.names = FALSE)
    }
  )
)
