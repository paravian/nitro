#' Create a discrete character matrix
#'
#' \code{ContinuousMatrix} is an R6 class that contains a continuous character
#'   matrix and functions for modifying character activity.
#' @importFrom checkmate asInt assert check_data_frame check_null check_numeric
#'   makeAssertCollection test_null
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom dplyr across everything group_by mutate where
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom stats na.omit
#' @importFrom stringr str_pad str_replace_all str_replace_na
#' @export
ContinuousMatrix <- R6Class("ContinuousMatrix",
  private = list(
    .is_inactive = NULL,
    .matrix = NULL,
    .n_characters = NULL,
    .taxa = NULL
  ),
  active = list(
    #' @field taxa The names of the taxa contained in the matrix.
    taxa = function (value) {
      if (missing(value)) {
        return(private$.taxa)
      } else {
        cli_abort(c("{.arg taxa} is a read-only attribute."))
      }
    },
    #' @field n_characters The number of the characters contained in the matrix.
    n_characters = function (value) {
      if (missing(value)) {
        return(private$.n_characters)
      } else {
        cli_abort(c("{.arg n_characters} is a read-only attribute."))
      }
    },
    #' @field inactive A numeric vector indicating which characters to mark as inactive.
    inactive = function (value) {
      if (missing(value)) {
        is_inactive <- !attr(private$.matrix, "active")
        if (any(is_inactive)) {
          return(which(is_inactive))
        }
        return(NULL)
      } else {
        coll <- makeAssertCollection()
        val_check <- assert(
          check_null(value),
          check_numeric(value, min.len = 1, lower = 1, upper = attr(private$.matrix, "nr"), unique = TRUE, any.missing = FALSE),
          add = coll)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg inactive} must contain valid unique character indices.",
                      "x" = val_check))
        }
        is_active <- rep(TRUE, self$n_characters)
        if (!test_null(value)) {
          is_active[value] <- FALSE
        }
        attr(private$.matrix, "active") <- is_active
      }
    }
  ),
  public = list(
    #' @param matrix An \code{phyDat} discrete character matrix.
    #' @param inactive A numeric vector indicating which characters to mark as inactive.
    initialize = function (matrix, inactive = NULL) {
      val_check <- check_data_frame(matrix, min.rows = 1, min.cols = 1, row.names = "named")
      if (!test_true(val_check)) {
        cli_abort(c("{.arg matrix} must be a {.cls data.frame}.",
                    "x" = val_check))
      }

      taxon_col <- names(matrix) == "taxon"
      if (sum(taxon_col) != 1) {
        cli_abort(c("{.arg matrix} must have one column named \"taxon\"."))
      }

      val_check <- check_data_frame(matrix[-taxon_col], types = "numeric")
      if (!test_true(val_check)) {
        cli_abort(c("{.arg matrix} must contain columns of numeric type for all characters.",
                    "x" = val_check))
      }

      matrix[,taxon_col] <- str_replace_all(matrix[,taxon_col], "\\s+", "_")

      if (any(duplicated(matrix[,taxon_col]))) {
        matrix <- group_by(matrix, "taxon") %>%
          summarise(across(everything(), function (x) {
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
          })
        )
      } else {
        matrix <- mutate(matrix, across(where(is.numeric), ~str_replace_na(.x, "?")))
      }

      rownames(matrix) <- matrix[,taxon_col]
      matrix[,taxon_col] <- NULL

      private$.matrix <- matrix
      private$.n_characters <- length(private$.matrix)
      private$.taxa <- rownames(private$.matrix)
      self$inactive <- inactive
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A TNT continuous matrix\")}")

      n_inactive <- self$inactive %>%
        {ifelse(is.null(.), 0, length(.))}

      options <- c("Number of characters:" = length(private$.matrix),
                   "Number of inactive characters:" = n_inactive) %>%
        data.frame()

      names(options) <- NULL
      print(options)
    },
    #' @param ... Ignored.
    queue = function (...) {
      max_num_len <- sapply(private$.matrix, nchar) %>% max()

      tax_names <- rownames(private$.matrix)
      max_tax_len <- nchar(tax_names) %>% max()
      tax_names <- str_pad(tax_names, width = max_tax_len, side = "right")

      part_len <- floor((254 - max_tax_len + 1) / (max_num_len + 1)) * (max_num_len + 1)

      all_taxa <- apply(private$.matrix, 2, str_pad, width = max_num_len, side = "left") %>%
        apply(1, paste, collapse = " ") %>%
        str_replace_all(glue("(.{{{part_len}}})"), "\\1\n") %>%
        str_split("\n") %>%
        Reduce(f = rbind) %>%
        apply(2, function (x) c("&[continuous]", paste(tax_names, x))) %>%
        as.vector()

      queue <- CommandQueue$new()
      queue$add("xread", all_taxa)
      return(queue)
    }
  )
)
