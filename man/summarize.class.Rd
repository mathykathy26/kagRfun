% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_class.R
\name{summarize.class}
\alias{summarize.class}
\title{Summary Table Creation}
\usage{
summarize.class(tmat, digits = 2, quants = c(0, 0.25, 0.5, 0.75, 1),
  SD = TRUE, nmis = FALSE, uniq = FALSE, str = FALSE, latex = FALSE,
  full.latex = FALSE, lookup.names = NULL, filename = NULL,
  screen = FALSE, caption = NULL)
}
\arguments{
\item{tmat}{Data matrix}

\item{digits}{The number of digits to round the values to.}

\item{quants}{Vector of quantiles to include in the table. Values should be in [0,1].}

\item{SD}{Logical, include a column for standard deviations. Defaults to TRUE.}

\item{nmis}{Logical, include a column for the number of missing observations.}

\item{uniq}{Logica, include a column for the number of categories in the variable. Defaults to FALSE.}

\item{str}{Logical, include a column for the class of each variable. Defaults to FALSE.}

\item{latex}{Logical, returns table in LaTeX format via the xtable package.}

\item{full.latex}{Logical, TRUE if want latex output to be a stand-alone LaTeX file.  If latex=TRUE and full.latex=FALSE, the output can be included with an \include{filename} command. This may be useful if output from this function will be included as one part of a larger set of function calls.}

\item{lookup.names}{Vector of longer descriptive variable names, where column names should match variable names in tmat.}

\item{filename}{Name of file where output should be saved. Default is NULL, not to save it to a file.}

\item{screen}{Logical, TRUE if want to view results on the screen. Defaults to FALSE.}

\item{caption}{Caption for LaTeX format}
}
\description{
This function allows you to create a summary of variables table with LaTeX formatting options.
}
\examples{
test.dat<-data.frame(x=rnorm(10),y=as.factor(rbinom(10,1,.5)))
summarize.class(test.dat)
}
\keyword{summary}
\keyword{table}
\keyword{xtable}
