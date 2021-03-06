% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rttestps.h.R
\name{rttestPS}
\alias{rttestPS}
\title{Robust Paired Samples T-Test}
\usage{
rttestPS(data, pairs, tr = 0.2, md = FALSE, es = FALSE, ci = FALSE)
}
\arguments{
\item{data}{the data as a data frame}

\item{pairs}{a list of lists specifying the pairs of measurement in
\code{data}}

\item{tr}{a number between 0 and 0.5, (default: 0.2), the proportion of
measurements to trim from each end, when using the trim and bootstrap
methods}

\item{md}{\code{TRUE} or \code{FALSE} (default), provide means and standard
errors}

\item{es}{\code{TRUE} or \code{FALSE} (default), provide effect sizes}

\item{ci}{\code{TRUE} or \code{FALSE} (default), provide confidence
intervals}
}
\value{
A results object containing:
\tabular{llllll}{
  \code{results$ttest} \tab \tab \tab \tab \tab the table of t-test results \cr
}

Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:

\code{results$ttest$asDF}

\code{as.data.frame(results$ttest)}
}
\description{
Robust Paired Samples T-Test
}
\examples{
data(anorexia, package='MASS')
anorexiaFT <- subset(anorexia, subset = Treat == "FT")

rttestPS(anorexiaFT,
         pairs = list(
             list(i1 = 'Prewt', i2 = 'Postwt')))

#
#  ROBUST PAIRED SAMPLES T-TEST
#
#  Robust Paired Samples T-Test
#  ---------------------------------------------
#                       t        df      p
#  ---------------------------------------------
#    Prewt    Postwt    -3.83    10.0    0.003
#  ---------------------------------------------
#

}
