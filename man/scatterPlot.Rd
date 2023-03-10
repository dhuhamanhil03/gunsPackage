% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Manhil-Dhuha-FinalProject-RCODE.R
\name{scatterPlot}
\alias{scatterPlot}
\title{Produces scatter plot}
\usage{
scatterPlot(dataSet, xaxis, yaxis, xaxisLabel, yaxisLabel, title)
}
\arguments{
\item{dataSet}{data}

\item{xaxis}{variable}

\item{yaxis}{variable}

\item{xaxisLabel}{character}

\item{yaxisLabel}{character}

\item{title}{character}
}
\description{
Produces and prints out scatter plot
}
\examples{
scatterPlot(virginiaData, virginiaData$totalCrime, virginiaData$incarc_rate, "Total Crime",
"Incarceration Rate", "Total Crime vs Incarceration Rate")
}
\author{
Dhuha Manhil
}
