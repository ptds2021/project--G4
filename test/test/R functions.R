poids <- readxl::read_xlsx("/Users/ROUGE/Desktop/VenusLab - QMS poids.xlsx")
poids <- poids[!is.na(poids$Prelevement),]
poids <- poids[!is.na(poids$Cible),]
poids$Poids1 <- as.double(poids$Poids1)
poids$Poids2 <- as.double(poids$Poids2)
poids <- poids[,-16]
poids <- poids %>%
  pivot_longer(
    cols = starts_with("Poids"),
    names_to = "Poids",
    names_prefix = "Poids",
    values_to = "weight",
    values_drop_na = TRUE
  )
poids %>% filter(Request == "1008.1")

poids_SPC <- poids %>% 
  group_by(Request, Prelevement, Cible) %>%
  summarise(real_weight = weight - Tare,
            median = mean(real_weight),
            sd = sd(real_weight),
            range = max(real_weight) - min(real_weight))

request_CL <- function(request, A2 = 0.483) {
  request_SPC <- poids_SPC %>% filter(Request == request)
  Rbar = mean(request_SPC$range)
  UCL = mean(request_SPC$median) + A2*Rbar
  LCL = mean(request_SPC$median) - A2*Rbar
  
  USL = request_SPC$Cible[1] + 0.05 #Ask to Joao : which value around the cible
  LSL = request_SPC$Cible[1] - 0.05 #Ask to Joao : which value around the cible
  
  Cpu <- (USL - request_SPC$Cible[1])/3*Rbar
  Cpl <- (request_SPC$Cible[1] - LSL)/3*Rbar
  
  CPK <- matrix(c(Rbar, UCL, LCL, USL, LSL, Cpu, Cpl))
  
  request_SPC %>%
    ggplot2::ggplot() +
    ggplot2::geom_histogram(
      ggplot2::aes(x = request_SPC$median),
      fill = "grey80",
      color = "grey20", bins = 70) + 
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(request_SPC$median)), color = "blue", linetype = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = UCL), color = "blue", linetype = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = LCL), color = "blue", linetype = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = request_SPC$Cible), color = "red") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = USL), color = "red") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = LSL), color = "red") +
    stat_function(fun = dnorm, n = 101, args = list(mean = mean(request_SPC$median), sd = median(request_SPC$sd))) +
    ylab("")

}

#Add some basic stats, legend, and a text that analyze automatically


