poids <- readxl::read_xlsx("~/NESTLE/PodSPC - Documents/Data/VenusLab - QMS poids.xlsx")
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

poids_SPC <- poids %>% 
  group_by(Request, Prelevement, Cible) %>%
  summarise(real_weight = weight - Tare,
            median = median(real_weight),
            sd = sd(real_weight),
            range = max(real_weight) - min(real_weight))

poids_SPC <- poids_SPC[,-4] %>%
  distinct()

request_CL <- function(request, A2 = 0.483, d2 = 5.534) {
  request_SPC <- poids_SPC %>%
    filter(Request == request)
  Rbar = mean(request_SPC$range)
  UCL = median(request_SPC$median) + A2*Rbar
  LCL = median(request_SPC$median) - A2*Rbar
  
  
  z <- request_SPC$median > UCL | request_SPC$median < LCL
  out_control_perc <- sum(z)/length(request_SPC)
  
  
  USL = request_SPC$Cible[1] + request_SPC$Cible[1]*0.015 #Ask to Joao : What value should be set for the gap around the target
  LSL = request_SPC$Cible[1] - request_SPC$Cible[1]*0.015 #Ask to Joao : What value should be set for the gap around the target
  Cp = (USL - LSL)/(6*Rbar/d2)
  Cpu <- (USL - mean(request_SPC$median))/(3*Rbar/d2)
  Cpl <- (mean(request_SPC$median) - LSL)/(3*Rbar/d2)
  Cpk <- min(Cpu, Cpl)
  

  graph <- request_SPC %>%
    ggplot2::ggplot() +
    ggplot2::geom_histogram(aes(x = request_SPC$median, fill = request_SPC$median > UCL | request_SPC$median < LCL), bins = 100) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(request_SPC$median)), color = "black", linetype = 3) +
    geom_text(aes(x = mean(request_SPC$median), label="Process Median", y=-0.5), colour="black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = UCL), color = "black", linetype = 3) +
    geom_text(aes(x = UCL, label = "UCL", y = -0.5), colour = "black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = LCL), color = "black", linetype = 3) +
    geom_text(aes(x = LCL, label = "LCL", y = -0.5), colour = "black") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = request_SPC$Cible), color = "blue") +
    geom_text(aes(x = request_SPC$Cible, label="Cible", y = 23), colour="blue") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = USL), color = "blue") +
    geom_text(aes(x = USL, label = "USL", y = 23), colour = "blue") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = LSL), color = "blue") +
    geom_text(aes(x = LSL, label = "LSL", y = 23), colour = "blue") +
    stat_function(fun = dnorm, n = 101,
                  args = list(mean = mean(request_SPC$median),
                                                    sd = median(request_SPC$sd))) +
    ylab("") +
    xlab("") + theme(legend.position = "none") + 
    scale_fill_manual(values = c("black", "red")) 

if (Cpk < 1) {
  caption1 <- "Process variation is not equal to the specs"
}
  
if (Cpk >= 1 && Cpk < 1.3) {
  caption1 <- "Process variation is about equal to the specs"
}
  
if (Cpk >= 1.3) {
  caption1 <- "Process variation is equal to the specs"
  }
  
if (Cpk < Cp) {
  caption2 <- "Process is off center"
}


print(graph + labs(caption = paste(
        round(out_control_perc, 3),"% of observations are considered as outliers and are represented by the red stripes." , "\n",
        "Cp = ", Cp,"\t",
        "Cpk = ", Cpk,"\t", "\n",
        caption1,"\n", caption2, "\n")) +
    theme(plot.caption = element_text(hjust = 0, face = "italic"),
          plot.title.position = "plot", 
          plot.caption.position =  "plot"))

}

request_CL(929)
request_CL(797)
request_CL(820)

# next : 

# add basic stats
# understand how implement SL -> if we manage to do it : cpk analyis + text
# improve presentation


