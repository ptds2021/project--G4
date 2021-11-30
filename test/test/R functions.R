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

request_CL <- function(request, A2 = 0.483, d2 = 5.534) {
  request_SPC <- poids_SPC %>% filter(Request == request)
  Rbar = mean(request_SPC$range)
  UCL = mean(request_SPC$median) + A2*Rbar
  LCL = mean(request_SPC$median) - A2*Rbar
  
  USL = request_SPC$Cible[1] + 0.05 #Ask to Joao : which value around the cible
  LSL = request_SPC$Cible[1] - 0.05 #Ask to Joao : which value around the cible
  Cp = (USL - LSL)/(6*Rbar/d2)
  Pp = (USL - LSL)/(6*median(request_SPC$sd))
  Cpu <- (USL - request_SPC$Cible[1])/3*Rbar
  Ppu <- (USL - request_SPC$Cible[1])/3*median(request_SPC$sd)
  Cpl <- (request_SPC$Cible[1] - LSL)/3*Rbar
  Ppl <- (request_SPC$Cible[1] - LSL)/3*median(request_SPC$sd)
  Cpk <- min(Cpu, Cpl)
  Ppk <- min(Ppu, Ppl)
  
  graph <- request_SPC %>%
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
    stat_function(fun = dnorm, n = 101,
                  args = list(mean = mean(request_SPC$median),
                                                    sd = median(request_SPC$sd))) +
    ylab("") + xlab("")

if (Ppk < 1) {
  caption1 <- "Process variation is not equal to the specs"
}
  
if (Ppk >= 1 && Ppk < 1.3) {
  caption1 <- "Process variation is about equal to the specs"
}
  
if (Ppk >= 1.6) {
  caption1 <- "Process variation is equal to the specs"
  }
  
if (Ppk < Pp) {
  caption2 <- "Process is off center"
}
  
if (Cpk < Ppk && Cpk < Cp) {
  caption3 <- "Process Out of Control and Off-Center"
}
  
if (Ppk == Cpk || Ppk == (Cpk + 0.05) | Ppk == (Cpk - 0.05) ) {
  caption3 <- "Process is fairly in control"
}
if (Ppk < Cpk) {
    caption3 <- "Process is in control"
}
if (Ppk < Cpk) {
    caption3 <- "Process is out of control"
  }
  print(graph + labs(caption = paste(caption1,"\n", caption2, "\n", caption3)))
}


#Add some basic stats, legend, and a text that analyze automatically

request_CL(929)


