#' Set options for jackknife resampling
#'
#' @description
#' \code{JackknifeOptions} is an R6 class that defines options for jackknife
#'   resampling analyses.
#' @importFrom checkmate check_int
#' @importFrom cli cli_abort cli_text col_grey
#' @importFrom R6 R6Class
#' @export
JackknifeOptions <- R6Class("JackknifeOptions",
  inherit = ResampleBaseOptions,
  private = list(
    .probability = NULL
  ),
  active = list(
    #' @field probability An integer value indicating the change probability.
    probability = function (value) {
      if (missing(value)) {
        return(private$.probability)
      } else {
        val_check <- check_int(value, lower = 0, upper = 99)
        if (!isTRUE(val_check)) {
          cli_abort(c("{.arg probability} must be an integer.",
                      "x" = val_check))
        }
        private$.probability <- asInt(value)
      }
    }
  ),
  public = list(
    #' @param probability An integer value indicating the change probability.
    #' @param search_method An object that contains configuration options for a
    #'   tree analysis method.
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
    initialize = function (probability = 36, search_method = NULL, replications = 100,
                           cutoff = 0, frequency_summary = "absolute") {
      a <- as.list(environment(), all = TRUE)
      for (n in names(a)) {
        self[[n]] <- a[[n]]
      }
    },
    #' @param ... Ignored.
    print = function (...) {
      cli_text("{col_grey(\"# A TNT jackknife resampling configuration\")}")

      options <- c(
        "Removal probability:" = self$probability,
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

      jack_args <- glue("jak from 0 replications {self$replications} probability {self$probability} cut {self$cutoff} {freq_summ} [")

      sm_queue <- self$search_method$queue()
      while(sm_queue$length() > 0) {
        cmd <- sm_queue$read_next() %$%
          paste(name, arguments, sep = " ", collapse = " ") %>%
          glue(";")
        jack_args <- c(jack_args, cmd)
      }
      jack_args <- c(jack_args, "]")

      queue$add("resample", jack_args)
      return(queue)
    }
  )
)
