% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NitroConstraint-class.R
\name{NitroConstraint}
\alias{NitroConstraint}
\title{Define constrained tree search}
\description{
\code{NitroConstraint} is an R6 class that defines a constrained tree
search.
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{is_positive}}{A logical value indicating whether the constraint is
positive (\code{TRUE}) or negative (\code{FALSE}).}

\item{\code{fixed_otus}}{A logical vector indicating which OTUs from the matrix
to assign as a fixed constraint.}

\item{\code{floating_otus}}{An optional logical vector indicating which OTUs from
the matrix to assign as floating constraints.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{NitroConstraint$new()}}
\item \href{#method-print}{\code{NitroConstraint$print()}}
\item \href{#method-tnt_cmd}{\code{NitroConstraint$tnt_cmd()}}
\item \href{#method-clone}{\code{NitroConstraint$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NitroConstraint$new(
  is_positive = TRUE,
  fixed_otus = NULL,
  floating_otus = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{is_positive}}{A logical value indicating whether the constraint is
positive (\code{TRUE}) or negative (\code{FALSE}).}

\item{\code{fixed_otus}}{A character vector indicating which OTUs from the matrix
to assign as a fixed constraint.}

\item{\code{floating_otus}}{An character logical vector indicating which OTUs from
the matrix to assign as floating constraints.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NitroConstraint$print(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<div class="r">}}\preformatted{NitroConstraint$tnt_cmd(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NitroConstraint$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
