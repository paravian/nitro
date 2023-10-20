#' Create a discrete character matrix
#'
#' \code{DiscreteMatrix} is an R6 class that contains a discrete character
#'   matrix and functions for modifying character activity and ordering.
#' @importFrom checkmate asInt assert check_class check_logical check_null
#'   check_numeric check_subset makeAssertCollection
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom stringr str_pad str_to_lower
#' @importFrom tibble as_tibble
#' @importFrom TreeTools PhyDatToMatrix PhyDatToString
#' @export
DiscreteMatrix <- R6Class("DiscreteMatrix",
  private = list(
    .data = NULL,
    .data_type = NULL,
    .is_inactive = NULL,
    .is_ordered = NULL,
    .n_characters = NULL,
    .n_states = NULL,
    .symbols = NULL,
    .taxa = NULL
  ),
  active = list(
    #' @field data The discrete character-taxon matrix.
    data = function (value) {
      if (missing(value)) {
        return(private$.data)
      } else {
        cli_abort(c("{.arg data} is a read-only attribute."))
      }
    },
    #' @field data_type The type of discrete character data contained in the matrix.
    data_type = function (value) {
      if (missing(value)) {
        return(private$.data_type)
      } else {
        cli_abort(c("{.arg data_type} is a read-only attribute."))
      }
    },
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
    #' @field n_states The number of unique states contained in the matrix.
    n_states = function (value) {
      if (missing(value)) {
        return(private$.n_states)
      } else {
        cli_abort(c("{.arg n_states} is a read-only attribute."))
      }
    },
    #' @field symbols The unique set of discrete characters contained in the matrix.
    symbols = function (value) {
      if (missing(value)) {
        return(private$.symbols)
      } else {
        cli_abort(c("{.arg symbols} is a read-only attribute."))
      }
    },
    #' @field ordered A numeric vector indicating which characters to mark as ordered.
    ordered = function (value) {
      if (missing(value)) {
        if (any(private$.is_ordered)) {
          return(which(private$.is_ordered))
        }
        return(NULL)
      } else {
        coll <- makeAssertCollection()
        val_check <- assert(
          check_null(value),
          check_numeric(value, min.len = 1, lower = 1, upper = attr(private$.data, "nr"), unique = TRUE, any.missing = FALSE),
          add = coll)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg ordered} must contain valid unique character indices.",
                      "x" = val_check))
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
    },
    #' @field inactive A numeric vector indicating which characters to mark as inactive.
    inactive = function (value) {
      if (missing(value)) {
        is_inactive <- !attr(private$.data, "active")
        if (any(is_inactive)) {
          return(which(is_inactive))
        }
        return(NULL)
      } else {
        coll <- makeAssertCollection()
        val_check <- assert(
          check_null(value),
          check_numeric(value, min.len = 1, lower = 1, upper = attr(private$.data, "nr"), unique = TRUE, any.missing = FALSE),
          add = coll)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg inactive} must contain valid unique character indices.",
                      "x" = val_check))
        }
        is_active <- rep(TRUE, self$n_characters)
        if (!is.null(value)) {
          is_active[value] <- FALSE
        }
        attr(private$.data, "active") <- is_active
      }
    }
  ),
  public = list(
    #' @param data An \code{phyDat} discrete character matrix.
    #' @param ordered A numeric vector indicating which characters to mark as ordered.
    #' @param inactive A numeric vector indicating which characters to mark as inactive.
    initialize = function (data, ordered = NULL, inactive = NULL) {
      val_check <- check_class(data, "phyDat")
      if (!test_true(val_check)) {
        cli_abort(c("Matrix must be of a supported class."),
                  "x" = val_check)
      }
      private$.data <- data
      private$.n_states <- attr(private$.data, "nc")
      private$.n_characters <- length(attr(private$.data, "index"))
      private$.symbols <- attr(private$.data, "levels") %>%
        as.character()
      private$.taxa <- names(private$.data)
      self$inactive <- inactive

      data_type <- attr(private$.data, "type") %>%
        str_to_lower()
      if (data_type == "user") {
        noncoding <- c("?", "-")
        symbols <- data$symbols %>%
          {.[!. %in% noncoding]}
        val_check <- check_subset(symbols, as.character(0:9))
        if (!test_true(val_check)) {
          val_check <- str_replace_all(val_check, "(\\{|\\})", "\\1\\1")
          cli_abort(c("Discrete character matrices with user-defined symbols must be numeric.",
                      "x" = val_check))
        }
        data_type <- "numeric"
      } else if (data_type == "aa") {
        data_type = "proteins"
      }

      private$.data_type <- data_type
      self$ordered <- ordered
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A TNT discrete matrix\")}")

      log_lists <- list(ordered = self$ordered, inactive = self$inactive) %>%
        {lapply(., function (x) ifelse(is.null(x), 0, length(x)))}

      options <- c("Data type:" = self$data_type,
                   "Number of taxa:" = length(private$.data),
                   "Number of characters:" = length(attr(private$.data, "index")),
                   "Number of inactive characters:" = log_lists$inactive)

      if (self$data_type == "numeric") {
        options <- c(options,
                     "Number of ordered characters:" = log_lists$ordered)
      }

      options <- data.frame(options)
      names(options) <- NULL
      print(options)
    },
    #' @description
    #' Generate the command queue
    #'
    #' @param interleave A logical value indicating whether to interleave the
    #'   command arguments for reading in the character states for each
    #'   taxon. The default (\code{FALSE}) is suitable when only reading in a
    #'   discrete character matrix; \code{TRUE} is required when a continuous
    #'   matrix is combined with a discrete character matrix.
    queue = function (interleave = FALSE) {
      tax_names <- names(private$.data)
      max_tax_len <- nchar(tax_names) %>% max()
      tax_names <- tax_names %>%
        {str_pad(., max_tax_len, side = "right")}

      data_type <- private$.data_type

      if (interleave) {
        tokens <- PhyDatToMatrix(private$.data, parentheses = c("[", "]"))

        # Calculate the maximum token length for each character and apply padding
        max_token_lens <- apply(tokens, 2, function (x) nchar(x) %>% max())
        for (n_token in seq(max_token_lens)) {
          char_len <- max_token_lens[n_token]
          if (char_len > 1) {
            tokens[,n_token] <- str_pad(tokens[,n_token], width = char_len, side = "left")
          }
        }

        # Write tokens into interleaved matrices given the maximum allowed space
        max_char_len <- 254 - (max_tax_len + 1)
        taxa <- c()
        while (length(max_token_lens)) {
          mask <- cumsum(max_token_lens) < max_char_len
          mask_tokens <- apply(tokens[,mask], 1, paste, collapse = "") %>%
            {glue("{tax_names} {.}")}
          taxa <- c(taxa, glue("&[{data_type}]"), mask_tokens)
          max_token_lens <- max_token_lens[!mask]
          tokens <- tokens[,!mask]
        }
      } else {
        taxa <- PhyDatToString(private$.data, parentheses = "[", concatenate = FALSE) %>%
          {glue("{tax_names} {.}")} %>%
          as.character()
        if (data_type != "numeric") {
          taxa <- c(glue("&[{data_type}]"), taxa)
        }
      }

      queue <- CommandQueue$new()
      queue$add("xread", taxa)
      return(queue)
    }
  )
)
