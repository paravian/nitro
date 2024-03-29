% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CommandQueue-class.R
\name{CommandQueue}
\alias{CommandQueue}
\title{Command Queue}
\description{
\code{CommandQueue} is an R6 class that stores a list of TNT commands to
  execute in order.
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CommandQueue-add}{\code{CommandQueue$add()}}
\item \href{#method-CommandQueue-read_next}{\code{CommandQueue$read_next()}}
\item \href{#method-CommandQueue-length}{\code{CommandQueue$length()}}
\item \href{#method-CommandQueue-print}{\code{CommandQueue$print()}}
\item \href{#method-CommandQueue-clone}{\code{CommandQueue$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CommandQueue-add"></a>}}
\if{latex}{\out{\hypertarget{method-CommandQueue-add}{}}}
\subsection{Method \code{add()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CommandQueue$add(name, arguments = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{name}}{The name of the command.}

\item{\code{arguments}}{The arguments of the command.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CommandQueue-read_next"></a>}}
\if{latex}{\out{\hypertarget{method-CommandQueue-read_next}{}}}
\subsection{Method \code{read_next()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CommandQueue$read_next(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-CommandQueue-length"></a>}}
\if{latex}{\out{\hypertarget{method-CommandQueue-length}{}}}
\subsection{Method \code{length()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CommandQueue$length(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-CommandQueue-print"></a>}}
\if{latex}{\out{\hypertarget{method-CommandQueue-print}{}}}
\subsection{Method \code{print()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CommandQueue$print(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-CommandQueue-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CommandQueue-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CommandQueue$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
