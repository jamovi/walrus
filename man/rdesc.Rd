% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rdesc.h.R
\name{rdesc}
\alias{rdesc}
\title{Robust Descriptives}
\usage{
rdesc(
  data,
  vars,
  splitBy = NULL,
  mean = TRUE,
  trim = TRUE,
  tr = 0.2,
  win = FALSE,
  wl = 0.2,
  mest = FALSE,
  bend = 1.28,
  med = FALSE
)
}
\arguments{
\item{data}{the data as a data frame}

\item{vars}{a vector of strings naming the variables in \code{data} of
interest}

\item{splitBy}{a string naming the variable in \code{data} to split the
data by}

\item{mean}{\code{TRUE} (default) or \code{FALSE}, provide a 'normal'
arithmetic mean}

\item{trim}{\code{TRUE} (default) or \code{FALSE}, provide a trimmed mean}

\item{tr}{a number between 0 and 0.5 (default: 0.2); the proportion of
measurements to trim from each end when producing trimmed means}

\item{win}{\code{TRUE} or \code{FALSE} (default), provide a 'Winsorized'
mean}

\item{wl}{a number between 0 and 0.5 (default: 0.2); the level of
'winsorizing' when producing winsorized means}

\item{mest}{\code{TRUE} or \code{FALSE} (default), provide an 'M-estimated'
value}

\item{bend}{a number (default: 1.28), the bending constant to use when
using M-estimators}

\item{med}{\code{TRUE} or \code{FALSE} (default), provide medians}
}
\value{
A results object containing:
\tabular{llllll}{
  \code{results$table} \tab \tab \tab \tab \tab the table of descriptives \cr
}

Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:

\code{results$table$asDF}

\code{as.data.frame(results$table)}
}
\description{
Robust Descriptives
}
\examples{

data('eurosoccer', package='WRS2')

SpainGermany <- subset(eurosoccer, eurosoccer$League == 'Spain' | eurosoccer$League == 'Germany')
SpainGermany <- droplevels(SpainGermany)

walrus::rdesc(
    data = SpainGermany,
    vars = "GoalsGame",
    splitBy = "League",
    med = TRUE)

#
#  ROBUST DESCRIPTIVES
#
#  Robust Descriptives
#  ----------------------------------------------------------
#                                                    SE
#  ----------------------------------------------------------
#    GoalsGame    Germany    Mean            1.46     0.105
#                            Trimmed mean    1.45    0.1341
#                            Median          1.43    0.1599
#
#                 Spain      Mean            1.45     0.101
#                            Trimmed mean    1.33    0.0601
#                            Median          1.30    0.0766
#  ----------------------------------------------------------
#

}
