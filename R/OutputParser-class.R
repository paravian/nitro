#' Output parser for TNT
#'
#' @description
#' \code{OutputParser} is an R6 class that parses output from TNT.
#' @importFrom ape read.tree .compressTipLabel
#' @importFrom checkmate assert check_character check_choice check_list
#'   check_string check_subset makeAssertCollection test_matrix
#' @importFrom cli cli_abort
#' @importFrom dplyr bind_cols select mutate
#' @importFrom magrittr %>% extract extract2 set_names
#' @importFrom R6 R6Class
#' @importFrom stringr str_detect str_extract_all str_length str_match
#'   str_match_all str_replace_all str_split str_to_lower str_trim str_which
#' @export
OutputParser <- R6Class("OutputParser",
  private = list(
    content = NULL,
    newline = NULL,
    platform = NULL,
    target = NULL
  ),
  public = list(
    #' @param ... Ignored.
    initialize = function (...) {
      platform <- .Platform$OS.type
      val_check <- check_choice(platform, c("unix", "windows"))
      if (!isTRUE(val_check)) {
        cli_abort(c("{.arg platform} must be a valid platform.",
                    "x" = val_check))
      }

      if (platform == "unix") {
        private$newline <- "\n"
      } else {
        private$newline <- "\r\n"
      }
      private$platform <- platform

      # Define content matches for stream output
      private$content <- list(
        error = "\a+",
        lengths = "Tree lengths",
        min_length = "Minimum steps for [0-9]+ active taxa \\(total = ([0-9\\.]+)\\)",
        max_length = "Maximum steps for [0-9]+ active taxa \\(total = ([0-9\\.]+)\\)",
        legend = "Copied legends: \"(.+)\"",
        adjusted_homoplasy_scores = "Adjusted homoplasy",
        tags = "Tree with tags",
        phy = "Tread 'set of [0-9]+ trees'"
      )
    },
    #' @param value A character vector.
    clean = function (value) {
      val_check <- check_character(value, min.len = 1, any.missing = FALSE)
      if (!isTRUE(val_check)) {
        cli_abort(c("A character vector must be supplied.",
                    "x" = val_check))
      }

      if (!is.null(private$escapes)) {
        cleaned <- str_replace_all(value, private$escapes, "")
      }
      cleaned <- str_split(cleaned, private$newline) %>%
        unlist() %>%
        {.[nchar(.) > 0]}
      return(cleaned)
    },
    #' @param value A character vector.
    content_detect = function (value) {
      val_check <- check_character(value, min.len = 1, any.missing = FALSE)
      if (!isTRUE(val_check)) {
        cli_abort(c("A properly formatted character vector must be supplied.",
                    "x" = val_check))
      }

      content_type <- "text"

      content_detects <- sapply(private$content, function (x) any(str_detect(value, pattern = x)))
      if (any(content_detects)) {
        content_type <- names(private$content)[which(content_detects)[1]]
      }
      return(content_type)
    },
    #' Parse raw TNT tree output
    #'
    #' @param output A character vector of raw TNT output.
    #' @param content_type A character vector indicating the content type of \code{output}.
    transform = function (output, content_type) {
      val_check <- check_character(output, min.chars = 1, any.missing = FALSE, min.len = 1)
      if (!isTRUE(val_check)) {
        cli_abort(c("{.arg output} must be a character vector.",
                    "x" = val_check))
      }

      coll <- makeAssertCollection()
      assert(
        check_string(content_type, min.chars = 1),
        check_choice(content_type, names(private$content)),
        combine = "and", add = coll
      )

      val_check <- coll$getMessages()
      if (!coll$isEmpty()) {
        cli_abort(c("{.arg content_type} must be a valid character vector.",
                    "x" = val_check))
      }

      output_re <- private$content[[content_type]]

      if (content_type == "phy") {
        output <- output %>%
          {.[nchar(str_trim(.)) > 0]} %>%
          paste(collapse = "") %>%
          str_match(glue("{output_re}([^;]+)")) %>%
          extract(2) %>%
          str_split_1("\\*") %>%
          str_trim() %>%
          str_replace_all(c(" " = ",", ",\\)" = "\\)", "\\)\\(" = "\\),\\(", "$" = ";")) %>%
          lapply(read.tree, file = NULL) %>%
          .compressTipLabel()
      } else if (content_type == "tags") {
        phy <- str_match(output, "^[0-9\\(\\) ]+\\;") %>%
          na.omit() %>%
          as.vector() %>%
          str_replace_all(c(" " = ",", ",\\)" = "\\)", "\\)\\(" = "\\),\\(")) %>%
          read.tree(file = NULL) %>%
          list() %>%
          .compressTipLabel()

        tags <- str_match_all(output, "ttag \\+(?<node>[0-9]+) (?<tag>[0-9\\./\\-\\[\\]]+)") %>%
          Reduce(f = rbind) %>%
          as.data.frame()

        tag_cols <- tags$tag %>%
          str_replace_all("\\[([0-9\\.]+)\\]", "-\\1") %>%
          str_split("/", simplify = TRUE) %>%
          data.frame()

        tags <- tags %>%
          select(node) %>%
          apply(2, as.numeric) %>%
          data.frame() %>%
          bind_cols(tag_cols)

        output <- list(phy = phy, tags = tags)
      } else if (content_type == "legend") {
        summ_pattern <- c("Group freqs." = "absolute", "GC values" = "difference")
        type_pattern <- c("Standard B" = "b", "Jacknifing" = "jackknife", "(Relative )*[Bb]remer" = "branch")

        legend_re <- c("^(?<summary>[^,]+), [0-9]+ replicates, cut=[0-9]+ \\(tree [0-9]\\) - (?<type>(?:[A-Za-z]+ *?){1,2})(?: \\(P=[0-9]+\\))*$",
                      "(?<type>[A-Za-z ]+) \\(from [0-9]+ trees, cut [0-9\\.]+\\)")

        legend <- str_match_all(output, output_re) %>%
          Reduce(f = rbind) %>%
          extract(,2) %>%
          data.frame(legend = .) %>%
          mutate(re = sapply(legend_re, str_detect, string = legend) %>%
                   {if (test_matrix(.)) . else t(.)} %>%
                   apply(1, which) %>%
                   legend_re[.])

        output <- apply(legend, 1, function (x) str_match_all(x[1], x[2])) %>%
          unlist(recursive = F) %>%
          Reduce(f = rbind) %>%
          extract(,-1) %>%
          {if (test_matrix(.)) . else t(.)} %>%
          data.frame() %>%
          mutate(summary = {if ("summary" %in% names(.)) str_replace_all(summary, summ_pattern) else NA},
                 type = str_replace_all(type, type_pattern))

      } else if (content_type %in% c("min_length", "max_length")) {
        output <- str_which(output, pattern = output_re) %>%
          {output[.]} %>%
          str_match(pattern = output_re) %>%
          extract(2) %>%
          as.numeric()
      } else {
        output <- paste(output, collapse = " ") %>%
          str_match(glue("{output_re}([^A-Za-z]+)")) %>%
          extract2(2) %>%
          str_trim() %>%
          str_split_1("(\\n\\s+)+") %>%
          extract(-1) %>%
          str_replace("^[0-9]+\\s+", "") %>%
          str_extract_all("[^A-Za-z ]+") %>%
          unlist() %>%
          as.numeric()
      }
      return(output)
    }
  )
)
