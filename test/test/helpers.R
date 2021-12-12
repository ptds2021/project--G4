weights <-
  c("Date",
    "Heure",
    "Poids1",
    "Poids2",
    "Poids3",
    "Poids4",
    "Poids5",
    "Poids6",
    "Request",
    "Operator",
    "Prelevement",
    "Batch_pod_scellé",
    "Size",
    "Specification",
    "Cible",
    "Tare")

outputDirWeight <- "responses"

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDirWeight, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

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
  "dplyr")

purrr::walk(packages, library, character.only = TRUE)


poids <- readxl::read_xlsx("~/NESTLE/PodSPC - Documents/Data/VenusLab - QMS poids.xlsx") %>% select(-"...16")
 
#poids <- readxl::read_xlsx("C:/Users/sophi//NESTLE/PodSPC - Documents/Data/VenusLab - QMS poids.xlsx") %>% select(-"...16")

#store the data into one tibble
data <- list.files(path = "test/responses/", pattern="*.csv",  full.names = TRUE) %>%
  lapply(read_csv) %>%                                            # Store all files in list
  bind_rows                                                       # Combine data sets into one data set
data


poids$Poids1 <- as.double(poids$Poids1)
poids$Poids2 <- as.double(poids$Poids2)

#merge the data base
poids <- poids %>%
  mutate(Date = as.numeric(Date))
data_all <- rbind(poids, data)
# data_all<-poids

df <- poids %>%
   pivot_longer(
     cols = starts_with("Poids"),
     names_to = "Poids",
     names_prefix = "Poids",
    values_to = "weight",
    values_drop_na = TRUE
   ) #tidy data set

 df <- df[!is.na(df$Prelevement),]
 df <- df[!is.na(df$Cible),]
 
 

 