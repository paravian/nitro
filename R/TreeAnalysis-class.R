#' Tree Analysis Configuration
#'
#' @description
#' An [R6][R6::R6Class] class that stores the complete configuration for a
#' phylogenetic tree analysis in \pkg{nitro}, including the character–taxon
#' matrix, taxon activity, outgroup selection, zero-length branch handling,
#' and all commands to be executed.
#'
#' `TreeAnalysis` is the central object in a \pkg{nitro} workflow. Data and
#' analysis options are set at construction time or via active bindings, and
#' commands are added with helper functions such as [set_tree_search()],
#' [set_weighting()], and [set_support()].
#'
#' @details
#' ## Typical workflow
#' ```r
#' # 1. Create an analysis with data
#' ta <- TreeAnalysis$new(data = my_matrix)
#'
#' # 2. Optionally configure analysis settings
#' ta$outgroup <- "Taxon_A"
#' ta$inactive_taxa <- c("Taxon_X", "Taxon_Y")
#'
#' # 3. Add commands
#' ta <- set_tree_search(ta, "branch_swapping", replications = 100)
#' ta <- set_weighting(ta, "implied_weighting", weighting_constant = 6)
#' ta <- set_support(ta, "bootstrap", replications = 1000)
#'
#' # 4. Inspect the configuration
#' ta
#' ```
#'
#' ## Adding commands
#' Commands are added via helper functions rather than directly through
#' `$add_command()`. Each command declares what it `$provides` (e.g.,
#' `"tree search"`), and adding a new command that provides the same thing
#' replaces the previous one.
#'
#' | Helper | Adds |
#' |--------|------|
#' | [set_tree_search()] | A tree search command. |
#' | [set_weighting()] | An implied weighting command. |
#' | [set_support()] | A resampling support command. |
#'
#' ## Zero-length branch rules
#' The `$zlb_rule` field controls how zero-length branches are handled when
#' collapsing trees. See [CollapseRuleCommand] for the full list of
#' available options. Note that resampling analyses require `$zlb_rule` to
#' be one of `"minimum"`, `"maximum"`, or `"identical_states"`;
#' [set_support()] validates this automatically.
#'
#' @seealso
#' * [create_matrix()] — create the matrix objects required by `$data`.
#' * [execute_analysis()] — runs the analysis.
#' * [make_tree_analysis()] — recommended constructor.
#' * [set_tree_search()], [new_tree_search()] — add or create tree search
#'   commands.
#' * [set_weighting()], [new_weighting()] — add or create weighting
#'   commands.
#' * [set_support()], [new_support()] — add or create support analysis
#'   commands.
#' * [CollapseRuleCommand] — documents the available zero-length branch
#'   rules.
#' * [OutgroupCommand] — documents outgroup configuration.
#' * [TaxonActivityCommand] — documents taxon activity configuration.
#' * [TreeAnalysisResults] — the object returned by execution.
#'
#' @examples
#' \dontrun{
#' nex_path <- system.file("extdata", "canale_2022.nex", package = "nitro")
#' dm <- ReadAsPhyDat(nex_path) |> create_matrix()
#'
#' # Create an analysis
#' ta <- make_tree_analysis(
#'   data     = dm,
#'   outgroup = "Herrerasaurus",
#'   zlb_rule = "minimum"
#' )
#'
#' # Add a branch swapping search
#' ta <- set_tree_search(ta, "branch_swapping", replications = 50)
#'
#' # Add implied weighting
#' ta <- set_weighting(ta, "implied_weighting", weighting_constant = 6)
#'
#' # View the configuration
#' ta
#'
#' # Check dimensions
#' ta$n_taxa
#' ta$n_characters
#' }
#'
#' @importFrom ape write.tree .compressTipLabel
#' @importFrom checkmate asInt assert check_character check_class check_choice check_disjunct check_int check_flag check_multi_class check_null check_number check_numeric check_subset check_false makeAssertCollection test_class test_disjunct test_list test_multi_class test_null test_true
#' @importFrom cli cli_abort cli_alert_info cli_text col_grey col_red style_italic symbol
#' @importFrom dplyr bind_rows if_else
#' @importFrom glue glue
#' @importFrom lubridate day hour minute second seconds_to_period
#' @importFrom magrittr %>% %$% add and extract extract2
#' @importFrom R6 R6Class
#' @importFrom tibble tibble
#' @importFrom tidytree as.treedata
#' @importFrom stringr str_extract_all str_replace str_replace_all str_split_1 str_starts str_to_sentence str_trim str_wrap
#' @export
TreeAnalysis <- R6Class(
  "TreeAnalysis",
  private = list(
    .data = NULL,
    .inactive_taxa = NULL,
    .commands = NULL,
    .outgroup = NULL,
    .zlb_rule = NULL,
    deep_clone = function(name, value) {
      if (name == ".commands") {
        sapply(value, function (x) x$clone())
      } else {
        value
      }
    }
  ),
  active = list(
    #' @field commands \[`list`\]\cr
    #'   *(Read-only.)* A named list of [BasicCommand] objects registered
    #'   for this analysis, keyed by what each command `$provides`.
    commands = function(value) {
      if (!missing(value)) {
        cli_abort(c("{.val commands} is a read-only attribute."))
      }

      private$.commands
    },
    #' @field data \[`AbstractCharacterMatrix` or `MultiCharacterMatrix`\]\cr
    #'   *(Write-once.)* One or more character–taxon matrix objects
    #'   containing the data for the analysis.
    data = function(value) {
      if (missing(value)) {
        return(private$.data)
      } else {
        if (is.null(private$.data)) {
          val_check <- check_multi_class(
            value,
            c(
              "AbstractCharacterMatrix",
              "MultiCharacterMatrix"
            )
          )

          if (!test_true(val_check)) {
            cli_abort(c("{.arg data} must be a valid matrix object.",
                        "x" = val_check
            ))
          }

          if (!test_list(value)) {
            value <- c(value)
          }

          private$.data <- value
          read_data_config(self, value)
        } else {
          cli_abort(c("{.val data} is a read-only attribute."))
        }
      }
    },
    #' @field inactive_taxa \[`character` or `NULL`\]\cr
    #'   A character vector of taxon names to deactivate (exclude from the
    #'   analysis), or `NULL` to include all taxa. All names must be present
    #'   in `$data`.
    inactive_taxa = function(value) {
      if (missing(value)) {
        return(private$.inactive_taxa)
      } else {
        all_taxa <- sapply(private$.data, getElement, "taxa") %>%
          as.vector() %>%
          unique()

        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_character(value,
                          min.len = 1, max.len = length(all_taxa),
                          any.missing = FALSE, unique = TRUE
          ),
          add = coll
        )

        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg inactive_taxa} must be either a character vector or {.val NULL}.",
                      "x" = val_check
          ))
        }

        if (test_null(value)) {
          private$.inactive_taxa <- NULL
          return()
        }

        val_check <- check_subset(value, all_taxa)
        if (!test_true(val_check)) {
          val_check <- str_replace_all(val_check, "([{}])", "\\1\\1")
          cli_abort(c("{.arg inactive_taxa} must contain taxa present in {.arg matrix}.",
                      "x" = val_check
          ))
        }

        private$.inactive_taxa <- value
      }
    },
    #' @field n_characters \[`integer(1)`\]\cr
    #'   *(Read-only.)* The total number of characters across all matrices
    #'   in `$data`.
    n_characters = function(value) {
      if (missing(value)) {
        n_char <- sapply(self$data, getElement, "n_characters") %>%
          sum()
      } else {
        cli_abort(c("{.val n_taxa} is a read-only attribute."))
      }
    },
    #' @field n_taxa \[`integer(1)`\]\cr
    #'   *(Read-only.)* The number of unique taxa across all matrices in
    #'   `$data`.
    n_taxa = function(value) {
      if (missing(value)) {
        sapply(self$data, getElement, "taxa") %>%
          as.vector() %>%
          unique() %>%
          length()
      } else {
        cli_abort(c("{.val n_taxa} is a read-only attribute."))
      }
    },
    #' @field outgroup \[`character(1)` or `NULL`\]\cr
    #'   The name of the taxon to use as the outgroup for rooting. Must be
    #'   a taxon present in `$data`. If `NULL` is supplied, defaults to the
    #'   first taxon in the matrix.
    outgroup = function(value) {
      if (missing(value)) {
        return(private$.outgroup)
      } else {
        all_taxa <- sapply(self$data, getElement, "taxa") %>%
          as.vector() %>%
          unique()

        coll <- makeAssertCollection()
        assert(
          check_null(value),
          assert(
            check_string(value, min.chars = 1),
            check_choice(value, all_taxa),
            combine = "and", add = coll
          ),
          add = coll
        )

        if (!coll$isEmpty()) {
          val_check <- str_replace_all(coll$getMessages(), "(\\{|\\})", "\\1\\1")
          cli_abort(c("{.arg outgroup} must be a taxon present in the data.",
                      "x" = val_check
          ))
        }

        if (test_null(value)) {
          value <- all_taxa[1]
        }
        private$.outgroup <- value
      }
    },
    #' @field zlb_rule \[`character(1)`\]\cr
    #'   *(Write-once.)* The rule for handling zero-length branches when
    #'   collapsing trees. See **Details** for the available options.
    #'   Defaults to `"minimum"`.
    zlb_rule = function(value) {
      options <- c(
        "maximum",
        "identical_states",
        "minimum",
        "discard_tree",
        "spr",
        "tbr"
      )

      if (missing(value)) {
        options[private$.zlb_rule]
      } else {
        value <- match.arg(value, options)
        val_check <- check_choice(value, options)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg collapse} choice must be valid.",
                      "x" = val_check,
                      "i" = "Set {.arg zlb_rule} to {options}."
          ))
        }

        value <- which(value == options)
        private$.zlb_rule <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Register a command for this analysis.
    #'
    #' The command is stored under the key returned by its `$provides`
    #' field. If a command providing the same thing already exists, it is
    #' replaced. Use [set_tree_search()] as a convenient alternative for
    #' tree search commands.
    #'
    #' @param command A [BasicCommand] object or a `CommandList`.
    add_command = function(command) {
      val_check <- check_multi_class(command, c("BasicCommand", "CommandList"))

      if (!test_true(val_check)) {
        cli_abort(c("{.arg command} must be an object inheriting from {.cls BasicCommand}.",
                    "x" = val_check))
      }

      if (inherits(command, "BasicCommand")) {
        command <- c(command)
      }
      for (cmd in command) {
        private$.commands[[cmd$provides]] <- cmd
      }
    },
    #' @description
    #' Format the analysis configuration as a summary table.
    #'
    #' Returns a data frame showing the parameters of every registered
    #' command, grouped by command with header rows.
    #'
    #' @param ... Not used.
    #'
    #' @return A `data.frame` with columns for description, current value,
    #'   and default value.
    format = function(...) {
      parse_names <- function(name) {
        str_extract_all(name, "[A-Z][a-z]+") %>%
          extract2(1) %>%
          paste(collapse = " ") %>%
          str_to_sentence()
      }

      empty_row <- data.frame(matrix("", 1, 3))
      names(empty_row) <- c("", "Current value", "Default value")

      options <- NULL

      for (command in self$commands) {
        cmd_opts <- format(command)
        names(cmd_opts) <- str_trim(names(cmd_opts))
        cmd_opts[, 1] <- str_trim(cmd_opts[, 1])

        cmd_header_row <- empty_row
        cmd_header_row[, 1] <- command$description

        if (ncol(cmd_opts) == 2) {
          cmd_opts$`Default value` <- ""
        }

        if (test_null(options)) {
          options <- rbind(cmd_header_row, cmd_opts)
        } else {
          options <- rbind(options, empty_row, cmd_header_row, cmd_opts)
        }
      }

      options[, 1] <- format(options[, 1], justify = "left")
      options
    },
    #' @description
    #' Create a new `TreeAnalysis` object.
    #'
    #' Initialises the analysis with a character–taxon matrix and optional
    #' settings. The outgroup defaults to the first taxon in the matrix if
    #' not specified.
    #'
    #' @param data One or more `DiscreteMatrix` or `ContinuousMatrix`
    #'   objects. See the `$data` field.
    #' @param inactive_taxa \[`character` or `NULL`\]\cr
    #'   Taxa to exclude from the analysis (default: `NULL`). See the
    #'   `$inactive_taxa` field.
    #' @param outgroup \[`character(1)` or `NULL`\]\cr
    #'   The outgroup taxon name (default: `NULL`, uses the first taxon).
    #'   See the `$outgroup` field.
    #' @param zlb_rule \[`character(1)`\]\cr
    #'   Rule for collapsing zero-length branches (default: `"minimum"`).
    #'   See the `$zlb_rule` field.
    #'
    #' @return A new `TreeAnalysis` object.
    initialize = function(data, inactive_taxa = NULL, outgroup = NULL,
                          zlb_rule = "minimum") {
      a <- as.list(environment(), all = TRUE)

      private$.commands <- list()

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }

      if (test_null(outgroup)) {
        self$outgroup <- self$data %>%
          extract2(1) %$%
          taxa %>%
          extract(1)
      }
    },
    #' @description
    #' Print a summary of the analysis configuration.
    #'
    #' @param ... Not used.
    print = function(...) {
      cli_text(col_grey("# A ", style_italic(col_red("nitro")), " tree analysis"))

      options <- format(self)
      print(options, row.names = FALSE)
    }
  )
)
