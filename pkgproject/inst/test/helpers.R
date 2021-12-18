weights <-
  c(
    "Date",
    "Measure1",
    "Measure2",
    "Measure3",
    "Measure4",
    "Measure5",
    "Measure6",
    "Request",
    "Operator",
    "Process.Sample",
    "Product.Size",
    "Target.Value",
    "Tare"
  )

fieldsMandatory <-
  c(
    "Measure1",
    "Measure2",
    "Measure3",
    "Measure4",
    "Measure5",
    "Measure6",
    "Request",
    "Target.Value",
    "Process.Sample",
    "Tare"
  )


outputDirWeight <- "responses"

dataLoaded <- loadData()

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <-
    sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDirWeight, fileName),
    row.names = TRUE,
    quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDirWeight, full.names = TRUE)
  data1 <- lapply(files, read.csv)
  # Concatenate all data together into one data.frame
  data1 <- do.call(rbind, data1)
  data1 <- as.tibble(data1)
  return(data1)
}

packages <- c(
  "here",
  "readxl",
  # for the project's organization
  "tidyverse",
  "lubridate",
  # for wrangling
  "knitr",
  "kableExtra",
  "bookdown",
  "rmarkdown",
  "DT",
  # for the report
  "summarytools",
  "caret",
  "ggplot2",
  "dplyr",
  "pkgproject"
)

purrr::walk(packages, library, character.only = TRUE)

#store the data into one tibble
data <-
  list.files(path = "inst/test/responses/",
             pattern = "*.csv",
             full.names = TRUE) %>%
  lapply(read_csv) %>%                                            # Store all files in list
  bind_rows                                                        # Combine data sets into one data set


#merge the data base
Measure <- nasty
data_all <- rbind(Measure, data)



request_TS <- function(data, request, prelev) {
  TS <- filter(data, Request == request & Process.Sample == prelev)

  graph <- ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(y = (TS$Measure1 - TS$Tare), x = 1)) +
    ggplot2::geom_point(ggplot2::aes(y = (TS$Measure2 - TS$Tare), x = 2)) +
    ggplot2::geom_point(ggplot2::aes(y = (TS$Measure3 - TS$Tare), x = 3)) +
    ggplot2::geom_point(ggplot2::aes(y = (TS$Measure4 - TS$Tare), x = 4)) +
    ggplot2::geom_point(ggplot2::aes(y = (TS$Measure5 - TS$Tare), x = 5)) +
    ggplot2::geom_point(ggplot2::aes(y = (TS$Measure6 - TS$Tare), x = 6)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = (
      (
        TS$Measure1 - TS$Tare + TS$Measure2 - TS$Tare + TS$Measure3 - TS$Tare + TS$Measure4 -
          TS$Tare + TS$Measure5 - TS$Tare + TS$Measure6 - TS$Tare
      )
    ) / 6),
    color = "blue",
    linetype = 3) +
    ggplot2::geom_hline(
      yintercept = (TS$Target.Value),
      linetype = "dashed",
      color = "red"
    ) +
    labs(
      x = "Measure",
      y = "measure (Gr)",
      title = paste("Request", TS$Request),
      subtitle = paste("Process Sample", TS$Process.Sample)
    )
  print(graph)
}



summary_table <- function(x1, x2, x3, x4, x5, x6, t) {
  x <- c(x1, x2, x3, x4, x5, x6)
  x <- x - t
  mean <- mean(x)
  sd <- sd(x)
  median <- median(x)
  quantile1 <- quantile(x, probs = seq(0.25))
  quantile3 <- quantile(x, probs = seq(0.75))
  t <- tibble(mean, sd, median, quantile1, quantile3)
  t
}
