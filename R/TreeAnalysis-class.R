#' Define tree searches
#'
#' \code{TreeAnalysis} is an R6 class that stores the configuration for a
#'  tree analysis, including the character-taxon matrix, taxon activity,
#'  character weighting and constraints on monophyly.
#' @importFrom ape write.tree .compressTipLabel
#' @importFrom checkmate asInt assert check_character check_class check_disjunct
#'   check_int check_flag check_multi_class check_null check_number
#'   check_numeric check_r6 check_subset check_false makeAssertCollection
#'   reportAssertions test_class test_multi_class test_null test_true
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom dplyr if_else
#' @importFrom glue glue
#' @importFrom lubridate day hour minute second seconds_to_period
#' @importFrom magrittr %>% %$% and
#' @importFrom R6 R6Class
#' @importFrom treeio as.treedata
#' @importFrom TreeTools PhyDatToMatrix RenumberTips TntOrder
#' @importFrom stringr str_replace str_replace_all str_split_1 str_starts
#'   str_to_sentence str_trim str_wrap
#' @export
TreeAnalysis <- R6Class("TreeAnalysis",
  private = list(
    .discrete_matrix = NULL,
    .continuous_matrix = NULL,
    .inactive_taxa = NULL,
    .outgroup = NULL,
    .zlb_rule = NULL,
    .constraints = NULL,
    .method = NULL,
    .weighting = NULL,
    .start_trees = NULL,
    .hold = NULL,
    .max_ram = NULL,
    .timeout = NULL
  ),
  active = list(
    #' @field discrete_matrix A \code{"\link{DiscreteMatrix}"} object.
    discrete_matrix = function (value) {
      if (missing(value)) {
        return(private$.discrete_matrix)
      } else {
        coll <- makeAssertCollection()
        val_check <- assert(
          check_null(value),
          assert(
            check_r6(value),
            check_class(value, "DiscreteMatrix"),
            combine = "and",
            add = coll
          ),
          add = coll
        )
        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg discrete_matrix} must be a valid matrix object.",
                      "x" = val_check))
        }

        if (!test_null(value)) {
          if (!test_null(self$continuous_matrix)) {
            val_check <- check_subset(value$taxa, self$continuous_matrix$taxa)
            if (!test_true(val_check)) {
              cli_abort(c("{.arg discrete_matrix} must be compatible with {.arg continuous_matrix}.",
                          "x" = val_check))
            }
          }
        }

        private$.discrete_matrix <- value
      }
    },
    #' @field continuous_matrix A \code{"\link{ContinuousMatrix}"} object.
    continuous_matrix = function (value) {
      if (missing(value)) {
        return(private$.continuous_matrix)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          assert(
            check_r6(value),
            check_class(value, "ContinuousMatrix"),
            combine = "and", add = coll
          ),
          add = coll
        )
        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg continuous_matrix} must be a valid matrix object.",
                      "x" = val_check))
        }

        if (!test_null(value)) {
          if (!test_null(self$discrete_matrix)) {
            val_check <- check_subset(value$taxa, self$discrete_matrix$taxa)
            if (!test_true(val_check)) {
              cli_abort(c("{.arg continuous_matrix} must be compatible with {.arg discrete_matrix}.",
                          "x" = val_check))
            }
          }
        }

        private$.continuous_matrix <- value
      }
    },
    #' @field method An object that contains configuration options for a tree
    #' analysis method.
    method = function (value) {
      if (missing(value)) {
        return(private$.method)
      } else {
        val_check <- check_multi_class(value, c("BootstrapOptions", "BranchBreakingOptions", "BranchSwappingOptions", "ConstraintSectorialSearchOptions", "DrivenSearchOptions", "ImplicitEnumerationOptions", "JackknifeOptions", "RandomSectorialSearchOptions", "RatchetOptions", "SymmetricResamplingOptions", "BranchSupportOptions"))
        if (!test_true(val_check)) {
          cli_abort(c("{.arg method} must contain valid tree analysis options.",
                      "x" = val_check))
        }
        set_sel_size <- function (driven_class) {
          sect_classes <- sapply(driven_class$sectorial_search, class)
          if ("RandomSectorialSearchOptions" %in% sect_classes) {
            def_size <- min(c(as.integer(ceiling(nrow(matrix) / 2)), 45L))
            idx <- which(sect_classes == "RandomSectorialSearchOptions")
            if (driven_class$sectorial_search[[idx]]$max_size == 0) {
              driven_class$sectorial_search[[idx]]$max_size <- def_size
            }
            if (driven_class$sectorial_search[[idx]]$min_size == 0) {
              driven_class$sectorial_search[[idx]]$min_size <- def_size
            }
          }
          return(driven_class)
        }

        if (test_class(value, "DrivenSearchOptions")) {
          value <- set_sel_size(value)
        }

        if (test_class(value, c("BranchBreakOptions"))) {
          val_check <- check_class(private$.start_trees, "multiPhylo")
          if (!test_true(val_check)) {
            cli_abort(c("{.arg start_trees} must be a {.cls multiPhylo} object if a {.arg method} is a {.cls {class(value)}} object.",
                        "x" = val_check))
          }
        }

        if (test_multi_class(value, c("ResampleBaseOptions", "BranchSupportOptions"))) {
          if (test_class(value$search_method, "DrivenSearchOptions")) {
            value$search_method <- set_sel_size(value$search_method)
          }

          val_check <- check_class(private$.start_trees, "phylo")
          if (!test_true(val_check)) {
            cli_abort(c("{.arg start_trees} must be a {.cls phylo} object if a {.arg method} is a resampling analysis.",
                        "x" = val_check))
          }

          valid_zlb_rule <- c("maximum", "identical_states", "minimum")
          val_check <- check_subset(self$zlb_rule, valid_zlb_rule)
          if (!test_true(val_check)) {
            val_check <- str_replace_all(val_check, "([\\{\\}])", "\\1\\1")
            cli_abort(c("{.arg zlb_rule} must be valid for a resampling {.arg method}.",
                        "x" = val_check,
                        "i" = "Set {.arg zlb_rule} to {valid_zlb_rule}."))
          }

          tree_taxa <- self$start_trees$tip.label
          all_mtx <- c(continuous = self$continuous_matrix,
                       discrete = self$discrete_matrix)
          all_taxa <- all_taxa <- sapply(all_mtx, `[[`, "taxa") %>%
            as.vector() %>%
            unique()
          inactive_taxa <- all_taxa[!all_taxa %in% tree_taxa]
          if (length(inactive_taxa) > 0) {
            self$inactive_taxa <- inactive_taxa
          }
        }
        private$.method <- value
      }
    },
    #' @field inactive_taxa A character vector indicating the taxa to be
    #'   inactivated.
    inactive_taxa = function (value) {
      if (missing(value)) {
        return(private$.inactive_taxa)
      } else {
        all_mtx <- c(continuous = self$continuous_matrix,
                     discrete = self$discrete_matrix)
        all_taxa <- sapply(all_mtx, `[[`, "taxa") %>%
          as.vector() %>%
          unique()

        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_character(value, min.len = 1, max.len = length(all_taxa),
                          any.missing = FALSE, unique = TRUE),
          add = coll
        )

        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg inactive_taxa} must be either a character vector or {.val NULL}.",
                      "x" = val_check))
        }

        # if (test_multi_class(value, c("ResampleBaseOptions", "BranchSupportOptions"))) {
        #   cli_abort(c("{.arg inactive_taxa} can't be modified for resampling analyses."))
        # }

        if (test_null(value)) {
          private$.inactive_taxa <- NULL
          return()
        }

        val_check <- check_subset(value, all_taxa)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg inactive_taxa} must contain taxa present in {.arg matrix}.",
                      "x" = val_check))
        }

        all_constrained <- Reduce(or, lapply(private$.constraints, function (c) {
          unique(c$fixed_otus, c$floating_otus)
        }))
        val_check <- check_disjunct(all_constrained, value)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg inactive_taxa} must not contain taxa that are in constraint group(s)",
                      "x" = inact_constr))
        }
        private$.inactive_taxa <- value
      }
    },
    #' @field outgroup A single character vector indicating the taxon to be
    #'   the outgroup.
    outgroup = function (value) {
      if (missing(value)) {
        return(private$.outgroup)
      } else {
        all_mtx <- c(continuous = self$continuous_matrix,
                     discrete = self$discrete_matrix)
        all_taxa <- sapply(all_mtx, `[[`, "taxa") %>%
          as.vector() %>%
          unique()

        coll = makeAssertCollection()
        assert(
          check_null(value),
          assert(
            check_string(value, min.chars = 1),
            check_choice(value, all_taxa),
            combine = "and", add = coll
          ),
          add = coll
        )
        val_check <- str_replace_all(coll$getMessages(), "(\\{|\\})", "\\1\\1")
        if (!coll$isEmpty()) {
          mtx_args <- paste(names(all_mtx), "_matrix", sep = "")
          cli_abort(c("{.arg outgroup} must be a taxon present in {.arg {mtx_args}}.",
                      "x" = val_check))
        }

        if (test_null(value)) {
          value <- all_taxa[1]
        }
        private$.outgroup <- value
      }
    },
    #' @field zlb_rule A character vector indicating the rule for handling zero
    #'   length branches. The options are:
    #'   \itemize{
    #'   \item \code{maximum}: collapse an interior branch of the maximum possible
    #'     length of the branch is zero;
    #'   \item \code{identical_states}: only collapse zero length branches if ancestor and descendant
    #'     states are the same;
    #'   \item \code{minimum}: collapse an interior branch if the minimum possible
    #'     length of the branch is zero (default);
    #'   \item \code{discard_tree}: discard all trees that must contain a zero length
    #'     branch;
    #'   \item \code{spr}: collapse an interior branch using subtree pruning and reconnection (SPR) operations; and
    #'   \item \code{tbr}: collapse an interior branch using tree bisection and reconnection (TBR) operations.
    #'   }
    zlb_rule = function (value) {
      options = c("maximum", "identical_states", "minimum", "discard_tree", "spr", "tbr")
      if (missing(value)) {
        options[private$.zlb_rule]
      } else {
        value <- match.arg(value, options)
        val_check <- check_choice(value, options)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg collapse} choice must be valid.",
                      "x" = val_check,
                      "i" = "Set {.arg zlb_rule} to {options}."))
        }

        if (test_multi_class(self$method, "ResampleBaseOptions")) {
          valid_zlb_rule <- c("maximum", "identical_states", "minimum")
          val_check <- check_subset(value, valid_zlb_rule)
          if (!test_true(val_check)) {
            val_check <- str_replace_all(val_check, "([\\{\\}])", "\\1\\1")
            cli_abort(c("{.arg zlb_rule} must be valid for a resampling {.arg method}.",
                        "x" = val_check,
                        "i" = "Set {.arg zlb_rule} to {valid_zlb_rule}."))
          }
        }

        value <- which(value == options)
        private$.zlb_rule <- value
      }
    },
    #' @field constraints One or more \code{"\link{MonophylyConstraintOptions}"} objects.
    constraints = function (...) {
      obj <- list(...)
      if (length(obj) == 0) {
        return(private$.constraints)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(unlist(obj)),
          check_list(obj, types = c("MonophylyConstraintOptions", "BackboneConstraintOptions")),
          add = coll
        )

        val_check <- coll$getMessages()
        if (!test_true(coll$isEmpty())) {
          cli_abort(c("{.arg constraints} must be one or more {.cls MonophylyConstraintOptions} or {.cls BackboneConstraintOptions} objects.",
                      "x" = val_check))
        }

        if (test_null(unlist(obj))) {
          return()
        }

        all_mtx <- c(self$continuous_matrix, self$discrete_matrix)
        all_taxa <- sapply(all_mtx, `[[`, "taxa") %>%
          as.vector() %>%
          unique()

        for (constraint in obj) {
          if (test_class(obj, "MonophylyConstraintOptions")) {
            all_const <- c(constraint$fixed_otus, constraint$floating_otus)

            val_check <- check_subset(all_const, all_taxa)
            if (!test_true(val_check)) {
              val_check <- str_replace_all(val_check, "([\\{\\}])", "\\1\\1")
              cli_abort(c("{.arg constraints} must contain taxa that are present in the matrix.",
                        "x" = val_check))
            }

            # Check if any constrained taxa are presently inactive
            val_check <- check_disjunct(constraint$fixed_otus, self$inactive_taxa)
            if (!test_true(val_check)) {
              val_check <- str_replace_all(val_check, "([\\{\\}])", "\\1\\1")
              cli_abort("Fixed constraints must not contain inactive taxa.",
                        "x" = val_check)
            }
            val_check <- check_disjunct(constraint$floating_otus, all_taxa)
            if (!test_true(val_check)) {
              val_check <- str_replace_all(val_check, "([\\{\\}])", "\\1\\1")
              cli_abort("Floating constraints must not contain inactive taxa.",
                        "x" = val_check)
            }
          }
          if (test_class(obj, "BackboneConstraintOptions")) {
            val_check <- check_subset(constraint$topology$tip.label, self$inactive_taxa)
            if (!test_true(val_check)) {
              val_check <- str_replace_all(val_check, "([\\{\\}])", "\\1\\1")
              cli_abort("Backbone constraints must contain taxa that are present in the matrix.",
                        "x" = val_check)
            }
          }
        }

        private$.constraints <- obj
      }
    },
    #' @field weighting An object containing configuration options for character
    #'  weighting.
    weighting = function (value) {
      if (missing(value)) {
        return(private$.weighting)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_class(value, "ImpliedWeightingOptions"),
          add = coll
        )
        val_check <- reportAssertions(coll)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg weighting} must contain a valid weighting configuration.",
                      "x" = val_check))
        }

        private$.weighting <- value
      }
    },
    #' @field start_trees A \code{phylo} or \code{multiPhylo} of trees to load
    #'  prior to starting the tree analysis.
    start_trees = function (value) {
      if (missing(value)) {
        return(private$.start_trees)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          assert(
            check_class(value, "phylo"),
            check_class(value, "multiPhylo")
          ),
          add = coll
        )
        val_check <- coll$getMessages()
        if (!coll$isEmpty()) {
          cli_abort(c("{.arg start_trees} must be either {.val NULL}, or a {.cls phylo} or {.cls multiPhylo} object.",
                      "x" = val_check))
        }

        if (test_null(value) & test_multi_class(self$method, c("ResampleBaseOptions", "BranchSupportOptions", "BranchBreakOptions"))) {
          cli_abort(c("{.arg start_trees} can't be {.val null} if {.arg method} is a {.cls {class(self$method)}} object."))
        }

        if (test_class(value, "phylo")) {
          value$node.label <- NULL
        } else if (test_class(value, "multiPhylo")) {
          value <- lapply(value, function (x) {
            x$node.label <- NULL
            return(x)
          }) %>%
            .compressTipLabel()
        }

        private$.start_trees <- value
      }
    },
    #' @field hold An integer indicating the number of trees to hold in TNT's
    #'   tree buffer.
    hold = function (value) {
      if (missing(value)) {
        return(private$.hold)
      } else {
        val_check <- check_int(value, lower = 1)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg hold} must be a valid numeric value.",
                      "x" = val_check))
        }

        value <- asInt(value)
        private$.hold <- value
      }
    },
    #' @field max_ram A numeric indicating the number of (binary) megabytes to
    #'   allocate for use by TNT.
    max_ram = function (value) {
      if (missing(value)) {
        return(private$.max_ram)
      } else {
        val_check <- check_number(value, lower = 0)
        if (!test_true(val_check)) {
          cli_abort(c("{.max_ram} must be a valid numeric value.",
                      "x" = val_check))
        }
        private$.max_ram <- value
      }
    },
    #' @field timeout A positive integer indicating the number of seconds to
    #'   allow a search to run for before terminating.
    timeout = function (value) {
      if (missing(value)) {
        return(private$.timeout)
      } else {
        coll <- makeAssertCollection()
        assert(
          check_null(value),
          check_int(value, lower = 1)
        )
        val_check <- reportAssertions(coll)
        if (!test_true(val_check)) {
          cli_abort(c("{.arg timeout} must be a valid numeric value.",
                      "x" = val_check))
        }
        private$.timeout <- value
      }
    }
  ),
  public = list(
    #' @param method An object that contains configuration options for the tree
    #'   analysis method.
    #' @param discrete_matrix A \code{"\link{DiscreteMatrix}"} object.
    #' @param continuous_matrix A \code{"\link{ContinuousMatrix}"} object.
    #' @param inactive_taxa A character vector indicating the taxa to be
    #'   inactivated.
    #' @param outgroup A single character vector indicating the taxon to be the
    #'   outgroup.
    #' @param zlb_rule An integer indicating the rule for collapsing of zero
    #'   length branches. The options are:
    #'   \itemize{
    #'   \item \code{maximum}: collapse an interior branch of the maximum
    #'    possible length of the branch is zero;
    #'   \item \code{identical_states}: only collapse zero length branches if
    #'    ancestor and descendant states are the same;
    #'   \item \code{minimum}: collapse an interior branch if the minimum
    #'    possible length of the branch is zero (the default); and
    #'   \item \code{discard_tree}: discard all trees that must contain a zero
    #'    length branch.
    #'   \item \code{spr}: collapse an interior branch using subtree pruning and
    #'    reconnection (SPR) operations; and
    #'   \item \code{tbr}: collapse an interior branch using tree bisection and
    #'    reconnection (TBR) operations.
    #'   }
    #' @param constraints One or more \code{"\link{MonophylyConstraintOptions}"} objects.
    #' @param weighting An object containing configuration options for character
    #'  weighting.
    #' @param start_trees A \code{phylo} or \code{multiPhylo} of trees to load
    #'  prior to starting the tree analysis.
    #' @param hold An integer indicating the number of trees to hold in TNTs tree
    #'   buffer.
    #' @param max_ram A numeric indicating the number of (binary) megabytes to
    #'   allocate for use by TNT.
    #' @param timeout A positive integer indicating the number of seconds to
    #'   allow a search to run for before terminating.
    initialize = function (method, discrete_matrix = NULL,
                           continuous_matrix = NULL, inactive_taxa = NULL,
                           outgroup = NULL, zlb_rule = "minimum",
                           constraints = NULL, weighting = NULL,
                           start_trees = NULL, hold = 100, max_ram = 16,
                           timeout = NULL) {
      a <- as.list(environment(), all = TRUE)

      # Check that at least one matrix type has been passed
      coll <- makeAssertCollection()
      val_check <- assert(
        check_false(test_null(discrete_matrix)),
        check_false(test_null(continuous_matrix)),
        add = coll
      )
      val_check <- coll$getMessages()
      if (!coll$isEmpty()) {
        val_check <- str_split_1(val_check, "\n") %>% str_trim()
        is_ul <- str_starts(val_check, "\\*")
        val_check <- str_replace(val_check, "\\* ", "")
        names(val_check) <- if_else(is_ul, "*", "x")
        cli_abort(c("At least one matrix object must be provided.",
                    val_check))
      }

      # Check that resampling analyses have a start tree
      if (test_multi_class(method, c("ResampleBaseOptions", "BranchSupportOptions"))) {
        val_check <- check_class(start_trees, "phylo")
        if (!test_true(val_check)) {
          cli_abort(c("{.arg start_trees} must be a {.cls phylo} object when a {.arg method} is a resampling analysis.",
                      "x" = val_check))
        }
        self$start_trees <- start_trees
        a$start_trees <- NULL
      }

      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A TNT tree analysis\")}")

      config <- c()
      all_mtx <- c(continuous = self$continuous_matrix,
                   discrete = self$discrete_matrix)
      which_mtx <- (!sapply(all_mtx, test_null)) %>%
        {glue("{sum(.)} ({paste(names(all_mtx)[.], collapse = \", \")})")}
      inactive_taxa <- self$inactive_taxa %>%
        {ifelse(!test_null(.), as.character(length(.)), "None")}

      n_constraints <- 0
      if(!test_null(private$.constraints)) {
        n_constraints <- length(private$.constraints)
      }

      weighting <- "Equal"
      if (test_class(self$weighting, "ImpliedWeightingOptions")) {
        weighting <- "Implied"
      }

      ta_method <- class(self$method)[1] %>%
        str_replace_all(c("Options" = "", "([a-z])([A-Z])" = "\\1 \\2")) %>%
        str_to_sentence()

      config <- c("Character matrices:" = which_mtx,
                  "Inactive taxa:" = inactive_taxa,
                  "Outgroup:" = self$outgroup,
                  "Tree analysis method:" = ta_method,
                  "Zero-length branch rule:" = str_to_sentence(self$zlb_rule))

      if (n_constraints > 0) {
        config <- c(config,
                    "Constraints:" = n_constraints)
      }

      config <- c(config,
                  "Weighting:" = weighting) %>%
        data.frame()

      names(config) <- NULL
      print(config)
    },
    #' @param ... Ignored.
    queue = function (...) {
      queue <- CommandQueue$new()

      queue$add("echo", "=")
      queue$add("screen", "25x10000")
      queue$add("log", "stdout")
      queue$add("silent", "=all")
      queue$add("silent", "-console")
      queue$add("collapse", private$.zlb_rule)

      weight_queue <- NULL
      if (test_class(self$weighting, c("ImpliedWeightingOptions", "R6"))) {
        weight_queue <- self$weighting$queue()
        weight_cmd <- weight_queue$read_next()
        queue$add(weight_cmd$name, weight_cmd$arguments)
      }

      all_mtx <- c(
        continuous = self$continuous_matrix,
        discrete = self$discrete_matrix
      )

      all_taxa <- sapply(all_mtx, `[[`, "taxa") %>%
        as.vector() %>%
        unique()

      queue$add("taxname", glue("+{nchar(all_taxa) %>% max() + 1}"))

      mtx_chars <- sapply(all_mtx, `[[`, "n_characters")

      xread <- glue("{n_char} {n_tax}",
                    n_char = sum(mtx_chars),
                    n_tax = length(all_taxa))

      ccode <- c()

      n <- 0

      for (n_mtx in seq(all_mtx)) {
        mtx <- all_mtx[[n_mtx]]
        is_discrete <- test_class(mtx, c("DiscreteMatrix", "R6"))
        is_continuous <- test_class(mtx, c("ContinuousMatrix", "R6"))

        if (is_continuous) {
          queue$add("nstates", "cont")
        }

        xread <- c(xread, mtx$queue()$read_next()$arguments)

        if (is_discrete) {
          if (!test_null(mtx$ordered)) {
            ccode <- c(ccode, glue("+ {paste(mtx$ordered + n - 1, collapse = \" \")}"))
          }
        }

        if (!test_null(mtx$inactive)) {
          ccode <- c(ccode, glue("] {paste(mtx$inactive + n - 1, collapse = \" \")}"))
        }
        n <- n + mtx$n_characters
      }

      queue$add("xread", xread)
      if (length(ccode) > 0) {
        queue$add("ccode", paste(ccode, collapse = " "))
      }
      if (!test_null(private$.inactive_taxa)) {
        queue$add("taxcode", glue("-{taxa}", taxa = paste(private$.inactive_taxa, collapse = " ")))
      }
      queue$add("hold", private$.hold)
      queue$add("outgroup", private$.outgroup)

      if (!test_null(weight_queue)) {
        if (weight_queue$length() > 0) {
          queue <- c(queue, weight_queue)
        }
      }

      if (!test_null(private$.constraints)) {
        all_args <- sapply(private$.constraints, function (x) x$queue()$read_next()$arguments)
        queue$add("force", all_args)
        queue$add("constrain", "=")
      }

      # if (!test_null(private$.timeout)) {
      #   td <- seconds_to_period(private$.timeout)
      #   tstr <- sprintf("%d:%02d:%02d", day(td) * 24 + hour(td), minute(td), second(td))
      #   tnt_cmds <- c(tnt_cmds, paste("timeout ", tstr, ";", collapse = ""))
      # }

      start_trees <- NULL

      if (!test_null(self$start_trees)) {
        start_trees <- write.tree(self$start_trees) %>%
          paste(collapse = " ") %>%
          str_replace_all(c("; " = "*;", ";$" = "", "\\),\\(" = "\\)\\(", "(,[^\\)]+)" = "\\1,", "\\)," = "\\)", "," = " ")) %>%
          str_split_1(";")
        queue$add("tread", start_trees)
      }

      if (test_multi_class(private$.method, c("ResampleBaseOptions", "BranchSupportOptions"))) {
        queue$add("ttags", "=")
        queue <- c(queue, private$.method$queue())
        queue$add("ttags", ")")
        if (test_multi_class(private$.method, "ResampleBaseOptions")) {
          queue$add("unique")
        }
        queue$add("ttags", "/")
      } else {
        queue <- c(queue, private$.method$queue())
        queue$add("condense")
        queue$add("tplot", "*")
      }

      queue$add("length")
      if (test_class(self$weighting, c("ImpliedWeightingOptions", "R6"))) {
        queue$add("score")
      }
      queue$add("minmax", "-<")
      queue$add("minmax", "->")
      queue$add("zzz")
      return(queue)
    },
    #' @param .envir The environment that TNT has been attached to.
    run = function (.envir = parent.frame()) {
      val_check <- check_environment(.envir)
      if (!test_true(val_check)) {
        cli_abort(c("{.arg .envir} must be an environment.",
                    "x" = val_check))
      }

      output <- execute_analysis(self, .envir)
      output$queue <- self$queue()

      all_taxa <- c(continuous = self$continuous_matrix,
                    discrete = self$discrete_matrix) %>%
        sapply(`[[`, "taxa") %>%
        as.vector() %>%
        unique()

      if (!test_null(output$tags)) {
        output$phy <- output$tags$phy
        output$tags <- output$tags$tags
        output$tags$node <- output$tags$node + 1
      }

      output$phy <- lapply(output$phy, function (x) {
        tip_order <- x$tip.label %>%
          as.numeric() + 1
        x$tip.label <- all_taxa[tip_order]
        x <- RenumberTips(x, all_taxa[sort(tip_order)]) %>%
          TntOrder()
        return(x)
      }) %>%
        .compressTipLabel()

      if (!test_null(output$legend)) {
        # weight_legend <- ifelse(test_class(self$weighting, "ImpliedWeightingOptions"), "adjusted homoplasy score", "steps")
        output$legend <- mutate(
          output$legend,
          # weight = weight_legend,
          summary = if (test_class(self$method, "BranchSupportOptions")) ta$method$index_type else summary,
          legend = glue("{type} ({summary})") %>% str_to_sentence())
        names(output$tags)[-1] <- output$legend$legend
        output$legend <- NULL
      }

      res <- do.call(TreeAnalysisResults$new, output)
      return(res)
    }
  )
)
