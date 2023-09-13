#' Set options for bootstrap resampling
#'
#' @description
#' \code{BootstrapOptions} is an R6 class that defines options for bootstrap
#'   resampling analyses.
#' @importFrom cli cli_abort cli_text
#' @importFrom glue glue
#' @importFrom R6 R6Class
#' @export
BootstrapOptions <- R6Class("BootstrapOptions",
  inherit = ResampleBaseOptions,
  public = list(
    #' @param search_method  A valid tree search configuration.
    #' @param replications An integer value indicating the number of resampling
    #'   replications to perform.
    #' @param cutoff An integer value indicating the cutoff value for
    #'   frequencies.
    #' @param frequency_summary A character vector indicating which method(s) to
    #'   use to summarize supports. More than one option can be specified. The
    #'   options are:
    #' \itemize{
    #'   \item \code{absolute}: absolute frequencies, default;
    #'   \item \code{difference}: frequency differences (i.e., group supported/contradicted);
    #'   \item \code{slope}: frequency slopes.
    #' }
    initialize = function (search_method = NULL, replications = 100, cutoff = 50,
                           frequency_summary = "absolute") {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A TNT bootstrap resampling configuration\")}")

      options <- c(
        "Replications:" = self$replications,
        "Cutoff frequency:" = self$cutoff,
        "Frequency summary:" = paste(self$frequency_summary, collapse = ", ")
      ) %>%
        data.frame()

      names(options) <- NULL
      print(options)
    },
    #' @param ... Ignored.
    queue = function (...) {
      queue <- CommandQueue$new()

      choices <- c(gc = "difference", frequency = "absolute", slope = "slope")
      freq_summ <- pmatch(self$frequency_summary, choices) %>%
        na.omit() %>%
        {names(choices)[.]} %>%
        paste(collapse = " ")
      if (!"difference" %in% self$frequency_summary) {
        freq_summ <- glue("nogc {freq_summ}")
      }

      boot_args <- glue("boot from 0 replications {self$replications} cut {self$cutoff} {freq_summ} [")

      sm_queue <- self$search_method$queue()
      while(sm_queue$length() > 0) {
        cmd <- sm_queue$read_next() %$%
          paste(name, arguments, sep = " ", collapse = " ") %>%
          glue(";")
        boot_args <- c(boot_args, cmd)
      }
      boot_args <- c(boot_args, "]")

      queue$add("resample", boot_args)
      return(queue)
    }
  )
)
