% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R_bar_chart.R
\name{R_bar_chart}
\alias{R_bar_chart}
\title{Xbar-R bar chart}
\usage{
R_bar_chart(data, request, A2 = 0.483)
}
\arguments{
\item{A2}{\code{numeric} Constant determined by a normal distribution as a function of the sample size useful for defining the control limits. We consider by default that the number of points in the sample is equal to 6, which gives a constant equal to 0.483.}

\item{\code{data.frame}}{data The data set of the analysis}

\item{\code{numeric}}{request A particular product, with specific production parameters (a value in the data.frame)}
}
\value{
A plot displaying the X_bar/R control chart
}
\description{
This function will produce a Shewhart X-bar/R control chart. The Xbar-R control chart is the most commonly used.
Xbar/R chart:
This function allows you to plots each sample mean to see between-sample variation
Useful for identifying special cause changes to the process mean (X), for detecting shifts/trends.
Control limits, related to +/- 3 standard deviations, are calculated using Rbar.
Points within the control limits are in accordance with the process variation. Those outside the limits are special causes to be analysed
}
\author{
Özgür Aydemir, Sophie La Gennusa, Louis del Perugia, Daniel Szenes, Francesca Darino
}
