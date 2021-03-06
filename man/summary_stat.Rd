% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_stat.R
\name{summary_stat}
\alias{summary_stat}
\title{Summary table}
\usage{
summary_stat(data, request, A2 = 0.483)
}
\arguments{
\item{data}{\code{data.frame} The dataset for the analysis (data.frame).
For this function to work, your dataset must have :
a `Request` column that has a number that identifies the request,
columns starting with `Measure` that has the values for you measurements,
a `Process Sample` column that records the number of the sample per request,
a `Target value` column that gives us the the value on which we base the specifications and
a `Tare` column for the weight of the container.}

\item{request}{\code{numeric} Number used to identify the request.}

\item{A2}{\code{numeric} constant defining the specification limits, default value 0.483.}
}
\description{
Displays a table with summary statistics for the measurements of a process.
The process should generally be measuring the weight of a substance in aontainer.
}
\author{
Özgür Aydemir, Sophie La Gennusa, Louis del Perugia, Daniel Szenes, Francesca Darino
}
