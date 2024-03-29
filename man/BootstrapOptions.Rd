% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BootstrapOptions-class.R
\name{BootstrapOptions}
\alias{BootstrapOptions}
\title{Set options for bootstrap resampling}
\description{
\code{BootstrapOptions} is an R6 class that defines options for bootstrap
  resampling analyses.
}
\section{Super class}{
\code{\link[nitro:ResampleBaseOptions]{nitro::ResampleBaseOptions}} -> \code{BootstrapOptions}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-BootstrapOptions-new}{\code{BootstrapOptions$new()}}
\item \href{#method-BootstrapOptions-print}{\code{BootstrapOptions$print()}}
\item \href{#method-BootstrapOptions-queue}{\code{BootstrapOptions$queue()}}
\item \href{#method-BootstrapOptions-clone}{\code{BootstrapOptions$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BootstrapOptions-new"></a>}}
\if{latex}{\out{\hypertarget{method-BootstrapOptions-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BootstrapOptions$new(
  search_method = NULL,
  replications = 100,
  cutoff = 50,
  frequency_summary = "absolute"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{search_method}}{A valid tree search configuration.}

\item{\code{replications}}{An integer value indicating the number of resampling
replications to perform.}

\item{\code{cutoff}}{An integer value indicating the cutoff value for
frequencies.}

\item{\code{frequency_summary}}{A character vector indicating which method(s) to
  use to summarize supports. More than one option can be specified. The
  options are:
\itemize{
  \item \code{absolute}: absolute frequencies, default;
  \item \code{difference}: frequency differences (i.e., group supported/contradicted);
  \item \code{slope}: frequency slopes.
}}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BootstrapOptions-print"></a>}}
\if{latex}{\out{\hypertarget{method-BootstrapOptions-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BootstrapOptions$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Ignored.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BootstrapOptions-queue"></a>}}
\if{latex}{\out{\hypertarget{method-BootstrapOptions-queue}{}}}
\subsection{Method \code{queue()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BootstrapOptions$queue(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Ignored.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-BootstrapOptions-clone"></a>}}
\if{latex}{\out{\hypertarget{method-BootstrapOptions-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{BootstrapOptions$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
