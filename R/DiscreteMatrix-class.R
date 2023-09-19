#' Create a discrete character matrix
#'
#' \code{DiscreteMatrix} is an R6 class that contains a discrete character
#'   matrix and functions for modifying character activity and ordering.
#' @importFrom checkmate asInt assert check_class check_int check_logical
#'   check_null check_numeric makeAssertCollection reportAssertions
#' @importFrom cli cli_abort
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom glue glue
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom stringr str_pad str_to_lower
#' @importFrom TreeTools PhyDatToString
#' @export
DiscreteMatrix <- R6Class("DiscreteMatrix",
  private = list(
    .data_type = NULL,
    .is_inactive = NULL,
    .is_ordered = NULL,
    .matrix = NULL,
    .n_characters = NULL,
    .n_states = NULL,
    .symbols = NULL,
    .taxa = NULL
  ),
  active = list(
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
          check_numeric(value, min.len = 1, lower = 1, upper = attr(private$.matrix, "nr"), unique = TRUE, any.missing = FALSE),
          add = coll)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg ordered} must contain valid unique character indices.",
                      "x" = val_check))
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
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg inactive} must contain valid unique character indices.",
                      "x" = val_check))
        }
        is_active <- rep(TRUE, self$n_characters)
        if (!is.null(value)) {
          is_active[value] <- FALSE
        }
        attr(private$.matrix, "active") <- is_active
      }
    }
  ),
  public = list(
    #' @param matrix An \code{phyDat} discrete character matrix.
    #' @param ordered A numeric vector indicating which characters to mark as ordered.
    #' @param inactive A numeric vector indicating which characters to mark as inactive.
    initialize = function (matrix, ordered = NULL, inactive = NULL) {
      val_check <- check_class(matrix, "phyDat")
      if (!isTRUE(val_check)) {
        cli_abort(c("Matrix must be of a supported class."),
                  "x" = val_check)
      }
      private$.matrix <- matrix
      private$.n_states <- attr(private$.matrix, "nc")
      private$.n_characters <- length(attr(private$.matrix, "index"))
      private$.symbols <- attr(private$.matrix, "levels") %>%
        as.character()
      private$.taxa <- names(private$.matrix)
      self$ordered <- ordered
      self$inactive <- inactive

      data_type <- attr(private$.matrix, "type") %>%
        str_to_lower()
      if (data_type == "user") {
        noncoding <- c("?", "-")
        symbols <- matrix$symbols %>%
          {.[!. %in% noncoding]}
        val_check <- check_subset(symbols, as.character(0:9))
        if (!isTRUE(val_check)) {
          val_check <- str_replace_all(val_check, "(\\{|\\})", "\\1\\1")
          cli_abort(c("Discrete character matrices with user-defined symbols must be numeric.",
                      "x" = val_check))
        }
        data_type <- "numeric"
      } else if (data_type == "aa") {
        data_type = "proteins"
      }

      private$.data_type <- data_type
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A TNT matrix\")}")

      log_lists <- as.list(self) %$%
        list(self$ordered, self$inactive) %>%
        {sapply(., function (x) ifelse(is.null(x), 0, length(x)))}

      options <- c(self$data_type, length(private$.matrix),
                   attr(private$.matrix, "nr"), log_lists) %>%
        data.frame()

      rownames(options) <- c("Data type:", "Number of taxa:",
                             "Number of characters:", "Number of ordered characters:",
                             "Number of inactive characters:")
      names(options) <- NULL

      print(options)
    },
    #' @param ... Ignored.
    queue = function (...) {
      all_taxa <- c()
      data_type <- self$data_type
      if (isTRUE(check_class(private$.matrix, "phyDat"))) {
        data_type <- private$.data_type
        tax_names <- names(private$.matrix) %>%
          {str_pad(., nchar(.) %>% max(), side = "right")}
        taxa <- PhyDatToString(private$.matrix, parentheses = "[", concatenate = FALSE) %>%
          {glue("{tax_names} {.}")} %>%
          as.character()
        all_taxa <- c(all_taxa, taxa)
      } else {
        cli_abort(c("Data type must be supported."))
      }

      queue <- CommandQueue$new()
      queue$add("xread", all_taxa)
      return(queue)
    }
  )
)
