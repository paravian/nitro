#' Tree Tagging Command
#'
#' @description
#' An [R6][R6::R6Class] class that controls tree tagging operations in TNT,
#' used to store and retrieve node-level support values during resampling
#' analyses.
#'
#' This command is enqueued automatically by [AbstractGroupSupportCommand] at
#' the start, end, and display stages of a support analysis. Users do not
#' typically need to instantiate it directly.
#'
#' @details
#' ## Actions
#' | Action | `$action` value | TNT command |
#' |--------|-----------------|-------------|
#' | Start storing tags | `"start_tagging"` | `ttags =;` |
#' | Stop storing tags | `"stop_tagging"` | `ttags );` |
#' | Display stored tags | `"display_tags"` | `ttags /;` |
#'
#' Partial matches are accepted (e.g., `"start"` resolves to
#' `"start_tagging"`).
#'
#' ## Output parsing
#' `$transform()` parses the raw TNT tag output and returns a
#' [tibble][tibble::tibble] with one row per internal node, containing
#' the node index and the associated support values. Node indices are
#' remapped from TNT's internal ordering to the standard `ape` ordering.
#'
#' ## Queue integration
#' This command has no `$enqueue()` override; it is added to a
#' [CommandQueue] directly by [AbstractGroupSupportCommand] at priorities `500`,
#' `520`, and `601`.
#'
#' @seealso
#' * [AbstractGroupSupportCommand] — enqueues this command at multiple stages
#'   of a support analysis.
#'
#' @keywords internal
#' @importFrom ape ladderize Ntip reorder.phylo rotateConstr
#' @importFrom checkmate check_choice test_true
#' @importFrom cli cli_abort
#' @importFrom dplyr arrange mutate rowwise across everything
#' @importFrom magrittr %>% use_series set_colnames
#' @importFrom R6 R6Class
#' @importFrom stringr str_extract str_match_all str_replace_all str_split
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest_wider
#' @importFrom TreeTools RenumberTips TntOrder
TreeTaggingCommand <- R6Class(
  "TreeTaggingCommand",
  inherit = BasicCommand,
  private = list(
    actions = c(
      "/" = "display_tags",
      "=" = "start_tagging",
      ")" = "stop_tagging"
    ),
    .action = NULL
  ),
  active = list(
    #' @field action \[`character(1)`\]\cr
    #'   The tree tagging operation to perform. Partial matches are
    #'   accepted. See **Details** for the available options.
    action = function(value) {
      label <- "action"
      if (missing(value)) {
        return(private$.action)
      } else {
        value <- pmatch(value, private$actions) %>%
          na.omit() %>%
          private$actions[.]

        val_check <- check_choice(value, private$actions)

        if (!test_true(val_check)) {
          cli_abort(c("{.arg {label}} must be a valid choice.",
                      "x" = str_replace_all(val_check, "([{}])", "\\1\\1")))
        }

        names(value) <- NULL
        private$.action <- value
      }
    }
  ),
  public = list(
    #' @description
    #' Create a new `TreeTaggingCommand` object.
    #'
    #' This command is created automatically by [AbstractGroupSupportCommand].
    #' Direct instantiation is rarely necessary.
    #'
    #' @param action \[`character(1)`\]\cr
    #'   The tagging operation to perform. See the `$action` field and
    #'   **Details**.
    #' @param ... Optional named arguments passed to the constructor of the
    #'   command class.
    #'
    #' @return A new `TreeTaggingCommand` object.
    initialize = function(action, ...) {
      a <- as.list(environment(), all = TRUE) %>%
        head(-1)

      super$initialize(
        name = "ttags",
        description = "Tree tag actions",
        outputs = "node labels",
        ...
      )

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @description
    #' Render the TNT `ttags` command string.
    #'
    #' @param ... Not used.
    #'
    #' @return A single-element character vector containing the TNT
    #'   command.
    render = function(...) {
      action_arg <- names(private$actions[private$actions == self$action])
      cmd <- paste(self$name, " ", action_arg, ";", sep = "")
      cmd
    },
    #' @description
    #' Parse raw TNT tag output into a node support tibble.
    #'
    #' Extracts node tags and the tree topology from the TNT output,
    #' remaps node indices from TNT's internal ordering to standard `ape`
    #' ordering, and returns a tidy tibble with one row per internal node.
    #'
    #' @param output \[`character`\]\cr
    #'   A character vector of raw output lines from the TNT executable.
    #'
    #' @return A [tibble][tibble::tibble] with columns `node` and one
    #'   column per support value type.
    transform = function(output) {
      output <- super$transform(output)

      phy <- str_extract(
        output,
        "[\\(\\)0-9 ]*"
      ) %>%
        paste(collapse = "") %>%
        str_replace_all(
          c(
            " " = ",",
            ",\\)" = "\\)",
            "\\)\\(" = "\\),\\(",
            "$" = ";"
          )
        ) %>%
        read.tree(file = NULL)

      tags <- str_replace(output, "\\[([0-9\\.]+)\\]", "-\\1") %>%
        str_match_all("ttag \\+(?<node>[0-9]+) (?<tag>[0-9\\?\\.\\-]+[/0-9\\?\\.\\-]*)") %>%
        Reduce(f = rbind) %>%
        extract(,-1) %>%
        as_tibble() %>%
        mutate(
          node = seq(Nnode(phy) - 1) + Ntip(phy) + 1,
        )

      output <- tags %>%
        rowwise() %>%
        mutate(
          tag = str_split(tag, "/")
        ) %>%
        unnest_wider(
          col = "tag",
          names_sep = "_"
        )

      phy_labels <- phy$tip.label
      phy <- as.numeric(phy_labels) %>%
        sort() %>%
        as.character() %>%
        RenumberTips(tree = phy)

      tnt_phy <- TntOrder(phy) %>%
        rotateConstr(phy_labels)

      get_ancs <- function(phy) {
        edges <- reorder(phy) %>%
          ladderize(right = FALSE) %>%
          use_series("edge")

        n_tips <- Ntip(phy)
        trav_node <- n_tips + 1
        ancs <- numeric()

        while (length(trav_node) > 0) {
          node <- trav_node[1]

          children_idx <- which(edges[, 1] == node)
          for (child_idx in children_idx) {
            child_node <- edges[child_idx, 2]
            if (child_node > n_tips) {
              ancs <- c(ancs, child_node)
              trav_node <- c(trav_node, child_node)
            }
          }

          trav_node <- trav_node[-1]
        }

        ancs
      }

      node_map <- list(tnt_phy, phy) %>%
        sapply(get_ancs) %>%
        set_colnames(c("tnt", "node")) %>%
        as_tibble()

      output <- merge(node_map, output, by.x = "tnt", by.y = "node") %>%
        mutate(tnt = NULL) %>%
        arrange(node) %>%
        as_tibble()

      output
    }
  )
)
