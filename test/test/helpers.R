weights <-
  c("Date",
    "Heure",
    "Prelevement",
    "Operator",
    "Request",
    "Batch_pod_scellé",
    "Batch_pod_bottom",
    "Poids1",
    "Poids2",
    "Poids3",
    "Poids4",
    "Poids5",
    "Poids6",
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

#Joaos' Theme
theme_qcc <- function(base_size = 12, base_family = "") {
  ggplot2::theme_grey(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      # Specify axis options
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = base_size*0.8, color = "black", lineheight = 0.9),
      axis.text.y = ggplot2::element_text(size = base_size*0.8, color = "black", lineheight = 0.9),
      axis.ticks = ggplot2::element_line(color = "black", size  =  0.2),
      axis.title.x = ggplot2::element_text(size = base_size, color = "black", margin = ggplot2::margin(0, 10, 0, 0)),
      axis.title.y = ggplot2::element_text(size = base_size, color = "black", angle = 90, margin = ggplot2::margin(0, 10, 0, 0)),
      axis.ticks.length = ggplot2::unit(0.3, "lines"),
      # Specify legend options
      legend.background = ggplot2::element_rect(color = NA, fill = "white"),
      legend.key = ggplot2::element_rect(color = "grey80",  fill = "#ccffff"),
      legend.key.size = ggplot2::unit(1.2, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = ggplot2::element_text(size = base_size*0.8, color = "black"),
      legend.title = ggplot2::element_text(size = base_size*0.8, face = "bold", hjust = 1, color = "black"),
      legend.position = "right",
      legend.text.align = NULL,
      legend.title.align = NULL,
      legend.direction = "vertical",
      legend.box = NULL,
      # Specify panel options
      panel.background = ggplot2::element_rect(fill = "white", color  =  NA),
      panel.border = ggplot2::element_rect(fill = NA, color = "grey50"),
      panel.grid.major = ggplot2::element_line(color = NA),
      panel.grid.minor = ggplot2::element_line(color = NA),
      panel.margin = ggplot2::unit(0.5, "lines"),
      # Specify facetting options
      strip.background = ggplot2::element_rect(fill = "grey30", color = "grey10"),
      strip.text.x = ggplot2::element_text(size = base_size*0.8, color = "black"),
      strip.text.y = ggplot2::element_text(size = base_size*0.8, color = "black",angle = -90),
      # Specify plot options
      plot.background = ggplot2::element_rect(color = NA, fill = "grey90"),
      plot.title = ggplot2::element_text(size = base_size*1.2, color = "black", hjust = 0),
      # plot.margin = ggplot2::unit(rep(1, 4), "lines")
      
    )
}

#poids <- readxl::read_xlsx("~/NESTLE/PodSPC - Documents/Data/VenusLab - QMS poids.xlsx") %>% select(-"...16")
 
poids <- readxl::read_xlsx("C:/Users/sophi//NESTLE/PodSPC - Documents/Data/VenusLab - QMS poids.xlsx") %>% select(-"...16")

#store the data into one tibble
data <- list.files(path = "test/responses/", pattern="*.csv",  full.names = TRUE) %>%
  lapply(read_csv) %>%                                            # Store all files in list
  bind_rows                                                       # Combine data sets into one data set
data


poids$Poids1 <- as.double(poids$Poids1)
poids$Poids2 <- as.double(poids$Poids2)

#merge the data base
poids<-poids%>% mutate(Date= as.numeric(Date))
data_all<- rbind(poids, data)
# data_all<-poids
# df <- poids %>%
#   pivot_longer(
#     cols = starts_with("Poids"),
#     names_to = "Poids",
#     names_prefix = "Poids",
#     values_to = "weight",
#     values_drop_na = TRUE
#   ) #tidy data set

# df <- df[!is.na(df$Prelevement),]
# df <- df[!is.na(df$Cible),]