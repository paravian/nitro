#' Continuous character matrix
#'
#' @description
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
ContinuousMatrix <- R6Class("ContinuousMatrix",
  inherit = AbstractCharacterMatrix,
  public = list(
    #' @param data A \code{data.frame} representing a continuous character
    #'   matrix.
    #' @param inactive A numeric vector indicating which characters to mark as inactive.
    initialize = function (data, inactive = NULL) {
      val_check <- check_data_frame(data, min.rows = 1, min.cols = 1, row.names = "named")
      if (!test_true(val_check)) {
        cli_abort(c("{.arg data} must be a {.cls data.frame}.",
                    "x" = val_check))
      }

      taxon_col <- names(data) == "taxon"
      if (sum(taxon_col) != 1) {
        cli_abort(c("{.arg data} must have one column named \"taxon\"."))
      }

      val_check <- check_data_frame(data[-taxon_col], types = "numeric")
      if (!test_true(val_check)) {
        cli_abort(c("{.arg data} must contain columns of numeric type for all characters.",
                    "x" = val_check))
      }

      data[,taxon_col] <- str_replace_all(data[,taxon_col], "\\s+", "_")

      if (any(duplicated(data[,taxon_col]))) {
        data <- group_by(data, "taxon") %>%
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
        data <- mutate(data, across(where(is.numeric), ~str_replace_na(.x, "?")))
      }

      rownames(data) <- data[,taxon_col]
      data[,taxon_col] <- NULL

      private$.data <- data
      private$.n_characters <- length(private$.data)
      private$.taxa <- rownames(private$.data)
      self$inactive <- inactive
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A TNT continuous matrix\")}")

      n_inactive <- self$inactive %>%
        {ifelse(is.null(.), 0, length(.))}

      options <- c("Number of characters:" = length(private$.data),
                   "Number of inactive characters:" = n_inactive) %>%
        data.frame()

      names(options) <- NULL
      print(options)
    },
    #' @param ... Ignored.
    queue = function (...) {
      max_num_len <- sapply(private$.data, nchar) %>% max()

      tax_names <- rownames(private$.data)
      max_tax_len <- nchar(tax_names) %>% max()
      tax_names <- str_pad(tax_names, width = max_tax_len, side = "right")

      part_len <- floor((254 - max_tax_len + 1) / (max_num_len + 1)) * (max_num_len + 1)

      all_taxa <- apply(private$.data, 2, str_pad, width = max_num_len, side = "left") %>%
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
