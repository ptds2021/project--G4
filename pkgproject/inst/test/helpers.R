weights <-
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
    "Taget.Value",
    "Tare")

fieldsMandatory <- c("Measure1", "Measure2", "Measure3", "Measure4", "Measure5", "Measure6", "Request", "Taget.Value", "Process.Sample","Tare")

outputDirWeight <- "responses"



loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDirWeight, full.names = TRUE)
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
Measure<- nasty
Measure <- Measure %>%
  mutate(Date = as.numeric(Date))
data_all <- rbind(Measure, data)



 request_TS <- function(request, prelev) {
   TS <-  Measure %>%
     filter(Request == request & Prelevement == prelev)

   graph <- ggplot2::ggplot() +
     ggplot2::geom_point(ggplot2::aes(y = (TS$weight[1]-TS$Tare), x = 1)) +
     ggplot2::geom_point(ggplot2::aes(y = (TS$weight[2]-TS$Tare), x = 2)) +
     ggplot2::geom_point(ggplot2::aes(y = (TS$weight[3]-TS$Tare), x = 3)) +
     ggplot2::geom_point(ggplot2::aes(y = (TS$weight[4]-TS$Tare), x = 4)) +
     ggplot2::geom_point(ggplot2::aes(y = (TS$weight[5]-TS$Tare), x = 5)) +
     ggplot2::geom_point(ggplot2::aes(y = (TS$weight[6]-TS$Tare), x = 6)) +
     ggplot2::geom_hline(ggplot2::aes(
       yintercept = (
         (TS$weight[1]-TS$Tare + TS$weight[2]-TS$Tare + TS$weight[3]-TS$Tare + TS$weight[4]-TS$Tare + TS$weight[5]-TS$Tare + TS$weight[6]-TS$Tare)
       )/6
     ),
     color = "blue",
     linetype = 3) +
     ggplot2::geom_hline(yintercept = (TS$Taget.Value),
                         linetype = "dashed",
                         color = "red") +
     labs(x = "Measure", y = "Weight (Gr)",
          title = paste("Request",TS$Request),
          subtitle = paste("Process Sample",TS$Process.Sample))
   print(graph)
 }



 summary_TS <- function(request, prelev) {
   TS <-  Measure %>%
     filter(Request == request & Prelevement == prelev)

   t(as.matrix(summary(TS$weight-TS$Tare)))
 }
