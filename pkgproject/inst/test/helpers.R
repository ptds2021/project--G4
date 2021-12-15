measures <-
  c("Date",
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
    "Tare")

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

outputDirmeasure <- "responses"

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDirWeight, fileName),
    row.names = TRUE, quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDirmeasure, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data <- as.matrix(data)
  data
}

packages <- c(
  "here", "readxl", # for the project's organization
  "tidyverse", "lubridate", # for wrangling
  "knitr", "kableExtra", "bookdown", "rmarkdown", "DT", # for the report
  "summarytools","caret","ggplot2",
  "dplyr", "pkgproject")

purrr::walk(packages, library, character.only = TRUE)

#store the data into one tibble
data <- list.files(path = "test/responses/", pattern="*.csv",  full.names = TRUE) %>%
  lapply(read_csv) %>%                                            # Store all files in list
  bind_rows                                                       # Combine data sets into one data set
data


#merge the data base
Measure <- nasty
data_all <- rbind(Measure, data)



 request_TS <- function(request, prelev) {
   TS <-  nasty %>%
     filter(Request == request & Process.Sample == prelev)

   graph <- ggplot2::ggplot() +
     ggplot2::geom_point(ggplot2::aes(y = (TS$measure[1]-TS$Tare), x = 1)) +
     ggplot2::geom_point(ggplot2::aes(y = (TS$measure[2]-TS$Tare), x = 2)) +
     ggplot2::geom_point(ggplot2::aes(y = (TS$measure[3]-TS$Tare), x = 3)) +
     ggplot2::geom_point(ggplot2::aes(y = (TS$measure[4]-TS$Tare), x = 4)) +
     ggplot2::geom_point(ggplot2::aes(y = (TS$measure[5]-TS$Tare), x = 5)) +
     ggplot2::geom_point(ggplot2::aes(y = (TS$measure[6]-TS$Tare), x = 6)) +
     ggplot2::geom_hline(ggplot2::aes(
       yintercept = (
         (TS$measure[1]-TS$Tare + TS$measure[2]-TS$Tare + TS$measure[3]-TS$Tare + TS$measure[4]-TS$Tare + TS$measure[5]-TS$Tare + TS$measure[6]-TS$Tare)
       )/6
     ),
     color = "blue",
     linetype = 3) +
     ggplot2::geom_hline(yintercept = (TS$Target.Value),
                         linetype = "dashed",
                         color = "red") +
     labs(x = "Measure", y = "measure (Gr)",
          title = paste("Request",TS$Request),
          subtitle = paste("Process Sample",TS$Process.Sample))
   print(graph)
 }



 summary_TS <- function(request, prelev) {
   TS <-  Measure %>%
     filter(Request == request & Process.Sample == prelev)

   t(as.matrix(summary(TS$measure-TS$Tare)))
 }
