% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DiscreteMatrix-class.R
\name{DiscreteMatrix}
\alias{DiscreteMatrix}
\title{Create a discrete character matrix}
\description{
Create a discrete character matrix

Create a discrete character matrix
}
\details{
\code{DiscreteMatrix} is an R6 class that contains a discrete character
  matrix and functions for modifying character activity and ordering.
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{data}}{The discrete character-taxon matrix.}

\item{\code{data_type}}{The type of discrete character data contained in the matrix.}

\item{\code{taxa}}{The names of the taxa contained in the matrix.}

\item{\code{n_characters}}{The number of the characters contained in the matrix.}

\item{\code{n_states}}{The number of unique states contained in the matrix.}

\item{\code{symbols}}{The unique set of discrete characters contained in the matrix.}

\item{\code{ordered}}{A numeric vector indicating which characters to mark as ordered.}

\item{\code{inactive}}{A numeric vector indicating which characters to mark as inactive.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DiscreteMatrix-new}{\code{DiscreteMatrix$new()}}
\item \href{#method-DiscreteMatrix-print}{\code{DiscreteMatrix$print()}}
\item \href{#method-DiscreteMatrix-queue}{\code{DiscreteMatrix$queue()}}
\item \href{#method-DiscreteMatrix-clone}{\code{DiscreteMatrix$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DiscreteMatrix-new"></a>}}
\if{latex}{\out{\hypertarget{method-DiscreteMatrix-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiscreteMatrix$new(data, ordered = NULL, inactive = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{An \code{phyDat} discrete character matrix.}

\item{\code{ordered}}{A numeric vector indicating which characters to mark as ordered.}

\item{\code{inactive}}{A numeric vector indicating which characters to mark as inactive.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DiscreteMatrix-print"></a>}}
\if{latex}{\out{\hypertarget{method-DiscreteMatrix-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiscreteMatrix$print(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-DiscreteMatrix-queue"></a>}}
\if{latex}{\out{\hypertarget{method-DiscreteMatrix-queue}{}}}
\subsection{Method \code{queue()}}{
Generate the command queue
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiscreteMatrix$queue(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-DiscreteMatrix-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DiscreteMatrix-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DiscreteMatrix$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
