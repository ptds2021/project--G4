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

poids <- readxl::read_xlsx("/Users/ROUGE/Desktop/VenusLab - QMS poids.xlsx")

poids$Poids1 <- as.double(poids$Poids1)
poids$Poids2 <- as.double(poids$Poids2)

a <- poids %>%
  pivot_longer(
    cols = starts_with("Poids"),
    names_to = "Poids",
    names_prefix = "Poids",
    values_to = "weight",
    values_drop_na = TRUE
) #tidy data set

b <- a[!is.na(a$Prelevement),] %>% 
  group_by(Request, Prelevement, Cible) %>%
  summarise(median = mean(weight),
            sd = sd(weight),
            range = max(weight) - min(weight)) %>%
  filter(Request == 929) #data test



Rbar = mean(b$range)
A2 = 0.483 #because each sample is composed of 6 observations
UCL = mean(b$median) + A2*Rbar
LCL = mean(b$median) - A2*Rbar 

####### X_bar chart : n = 6
b %>%
  ggplot2::ggplot() +
  ggplot2::geom_point(aes(x = Prelevement, y = median)) +
  ggplot2::geom_hline(yintercept = mean(b$median),
                      linetype = "dashed",
                      color = "blue") +
  ggplot2::geom_hline(ggplot2::aes(yintercept = UCL),
                      color = "blue",
                      linetype = 3) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = LCL),
                      color = "blue",
                      linetype = 3) +
  ggplot2::geom_hline(yintercept = (b$Cible),
                      linetype = "dashed",
                      color = "red") +
  theme_qcc()


####### distribution : n = 6
#Cpk analysis
b %>%
    ggplot2::ggplot() +
    ggplot2::geom_histogram(
      ggplot2::aes(x = b$median),
      fill = "grey80",
      color = "grey20") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = UCL), color = "blue", linetype = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = LCL), color = "blue", linetype = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = b$Cible), color = "red", linetype = 2) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mean(b$median), sd = median(b$sd))) +
  ylab("") +
  scale_y_continuous(breaks = NULL) + theme_qcc()
