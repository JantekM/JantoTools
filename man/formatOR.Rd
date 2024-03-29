% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatting.R
\name{formatOR}
\alias{formatOR}
\title{A handy function for formatting ORs (Odds Ratios) to publication-ready format}
\usage{
formatOR(OR, inf.as = "Inf", sig.digs = 3)
}
\arguments{
\item{OR}{A single number with OR to be formatted, can be +Inf.}

\item{inf.as}{A string, defining how +Inf should be displayed as, \strong{'Inf'} by default.}

\item{sig.digs}{A number of significant digits to show. By default \strong{3}.}
}
\value{
A character vector with OR formatted in a nice way
}
\description{
Especially handy when some other function has problems with displaying Inf
}
\details{
TODO: Allow vectorized input
TODO: Add a way to include Confidence Intervals
}
\examples{
formatOR(0.0026485635335) # -> "0.00265"
formatOR(Inf) # -> "Inf"
}
\seealso{
Other publishing-formatting: 
\code{\link{formatP}()},
\code{\link{signif_sym}()}
}
\concept{publishing-formatting}
