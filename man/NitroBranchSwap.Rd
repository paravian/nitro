% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NitroBranchSwap-class.R
\name{NitroBranchSwap}
\alias{NitroBranchSwap}
\title{Define a branch swapping analysis}
\description{
\code{NitroBranchSwap} is an R6 class that defines the set of parameters
required to perform a branch swapping ('traditional', in TNTs terminology)
phylogenetic analysis in \code{nitro}.
}
\section{Super class}{
\code{\link[nitro:NitroMethodsBase]{nitro::NitroMethodsBase}} -> \code{NitroBranchSwap}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{replications}}{An integer value indicating the number of replications.}

\item{\code{hold_rep}}{An integer value indicating the maximum number of trees to
retain during each replication.}

\item{\code{keep_all}}{A logical value indicating whether to retain all generated
trees from each replication regardless of length.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{NitroBranchSwap$new()}}
\item \href{#method-print}{\code{NitroBranchSwap$print()}}
\item \href{#method-tnt_cmd}{\code{NitroBranchSwap$tnt_cmd()}}
\item \href{#method-clone}{\code{NitroBranchSwap$clone()}}
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
\if{html}{\out{<div class="r">}}\preformatted{NitroBranchSwap$new(replications = 10, hold_rep = 10, keep_all = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{replications}}{An integer value indicating the number of replications.}

\item{\code{hold_rep}}{An integer value indicating the maximum number of trees to
retain during each replication.}

\item{\code{keep_all}}{A logical value indicating whether to retain all generated
trees from each replication regardless of length.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NitroBranchSwap$print(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<div class="r">}}\preformatted{NitroBranchSwap$tnt_cmd(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<div class="r">}}\preformatted{NitroBranchSwap$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
