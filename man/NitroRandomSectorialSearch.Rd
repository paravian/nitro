% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NitroRandomSectorialSearch-class.R
\name{NitroRandomSectorialSearch}
\alias{NitroRandomSectorialSearch}
\title{Define a random sectorial search analysis}
\description{
\code{NitroRandomSectorialSearch} is an R6 class that defines the set of
parameters required to perform sectorial searching analyses in
\code{nitro}.
}
\section{Super classes}{
\code{\link[nitro:NitroMethodsBase]{nitro::NitroMethodsBase}} -> \code{\link[nitro:NitroSectorialSearch]{nitro::NitroSectorialSearch}} -> \code{NitroRandomSectorialSearch}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{min_size}}{An integer value indicating the minimum size of random
selections. If left as 0 (the default), this value will be
automatically set according to the number of taxa in the matrix when a
new \code{"\link{NitroTreeSearch}"} object is initialized.}

\item{\code{max_size}}{An integer value indicating the maximum size of random
selections. If left at 0 (the default), this value will be
automatically set according to the number of taxa in the matrix when a
new \code{"\link{NitroTreeSearch}"} object is initialized.}

\item{\code{selection_factor}}{An integer value indicating the maximum number
of random selections for the currently active taxa.}

\item{\code{increase}}{An integer value indicating the factor to increase the
size of random selections if enough selection of the current size have
been completed.}

\item{\code{small_starts}}{An integer value indicating the number of random
addition sequence plus tree-bisection reconnection swaps to perform
for random selections below \code{min_size}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{NitroRandomSectorialSearch$new()}}
\item \href{#method-print}{\code{NitroRandomSectorialSearch$print()}}
\item \href{#method-tnt_cmd}{\code{NitroRandomSectorialSearch$tnt_cmd()}}
\item \href{#method-clone}{\code{NitroRandomSectorialSearch$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NitroRandomSectorialSearch$new(
  min_size = 0,
  max_size = 0,
  selection_factor = 50,
  increase = 100,
  small_starts = 3,
  buffer = TRUE,
  slack = 0
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{min_size}}{An integer value indicating the minimum size of random
selections. If left as 0 (the default), this value will be
automatically set according to the number of taxa in the matrix when a
new \code{"\link{NitroTreeSearch}"} object is initialized.}

\item{\code{max_size}}{An integer value indicating the maximum size of random
selections. If left at 0 (the default), this value will be
automatically set according to the number of taxa in the matrix when a
new \code{"\link{NitroTreeSearch}"} object is initialized.}

\item{\code{selection_factor}}{An integer value indicating the maximum number
of random selections for the currently active taxa.}

\item{\code{increase}}{An integer value indicating the factor to increase the
size of random selections if enough selection of the current size have
been completed.}

\item{\code{small_starts}}{An integer value indicating the number of random
addition sequence plus tree-bisection reconnection swaps to perform
for random selections below \code{min_size}.}

\item{\code{buffer}}{A logical value indicating whether to use an independent}

\item{\code{slack}}{An integer value indicating the percentage to increase the
available memory during searches.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NitroRandomSectorialSearch$print(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-tnt_cmd"></a>}}
\if{latex}{\out{\hypertarget{method-tnt_cmd}{}}}
\subsection{Method \code{tnt_cmd()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NitroRandomSectorialSearch$tnt_cmd(set_only = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{set_only}}{A logical indicating whether to instruct the command to
execute immediately (\code{TRUE}) or set the variables for future
execution \code{FALSE}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NitroRandomSectorialSearch$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
