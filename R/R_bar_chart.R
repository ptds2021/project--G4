#' @title Xbar-R bar chart
#' @author Özgür Aydemir, Sophie La Gennusa, Louis del Perugia, Daniel Szenes, Francesca Darino
#' @description This function will produce a Shewhart X-bar/R control chart. The Xbar-R control chart is the most commonly used.
#' Xbar/R chart:
#' This function allows you to plots each sample mean to see between-sample variation
#' Useful for identifying special cause changes to the process mean (X), for detecting shifts/trends.
#' Control limits, related to +/- 3 standard deviations, are calculated using Rbar.
#' Points within the control limits are in accordance with the process variation. Those outside the limits are special causes to be analysed
#' @return A plot displaying the X_bar/R control chart
#' @param \code{data.frame} data The data set of the analysis
#' @param \code{numeric} request A particular product, with specific production parameters (a value in the data.frame)
#' @param A2 \code{numeric} Constant determined by a normal distribution as a function of the sample size useful for defining the control limits. We consider by default that the number of points in the sample is equal to 6, which gives a constant equal to 0.483.
#' @example R_bar_chart(nasty, 929)
#' @import tidyverse
#' @import ggplot2
#' @import shiny
#' @import ggrepel
#' @export



R_bar_chart <- function(data, request, A2 = 0.483) {
  request <- data %>%
    filter(Request == request)

  data_long <- request %>%
    pivot_longer(
      cols = starts_with("Measure"),
      names_to = "Inputs",
      names_prefix = "Inputs",
      values_to = "weight",
      values_drop_na = TRUE
    )

  df <- data_long %>%
    group_by(Request, Process.Sample, Target.Value) %>%
    mutate(real_weight = weight - Tare) %>%
    summarise(
      median = median(real_weight),
      sd = sd(real_weight),
      range = max(real_weight) - min(real_weight)
    )


  Rbar = mean(df$range)
  UCL = median(df$median) + A2 * Rbar
  LCL = median(df$median) - A2 * Rbar

  z <- df$median > UCL | df$median < LCL
  out_control_point <- sum(z)

  d <- df %>% select(median, Process.Sample)
  a <- tibble()
  for (i in 1:nrow(d)) {
    if (d$median[i] > UCL | d$median[i] < LCL) {
      a <- rbind(a, d[i, ])
    }
  }

  Rchart <- df %>%
    ggplot() +
    geom_point(aes(
      x = Process.Sample,
      y = median,
      colour = df$median > UCL | df$median < LCL
    )) +
    geom_hline(
      yintercept = mean(df$median),
      linetype = "dashed",
      color = "black"
    ) +
    geom_text(aes(
      x = 2,
      label = "Process \n Median",
      y = mean(df$median) + 0.007
    ), colour = "black") +
    geom_hline(aes(yintercept = UCL),
               color = "black",
               linetype = 3) +
    geom_text(aes(
      x = 0,
      label = "UCL",
      y = UCL + 0.005
    ), colour = "black") +
    geom_hline(aes(yintercept = LCL),
               color = "black",
               linetype = 3) +
    geom_text(aes(
      x = 0,
      label = "LCL",
      y = LCL + 0.005
    ), colour = "black") +
    geom_hline(yintercept = (df$Target.Value),
               color = "blue") +
    geom_text(aes(
      x = -5,
      label = "Target.Value",
      y = (df$Target.Value) + 0.005
    ), colour = "blue") +
    xlab("Process Sample") +
    ylab("Median of each sample in grams") + theme(legend.position = "none") +
    scale_color_manual(values = c("black", "red")) +
    labs(
      title = paste("Request", df$Request, "R chart"),
      subtitle = paste(
        "The",
        out_control_point,
        "red dots are outside the control limits. Process variation cannot explain these extreme values, Process must be analysed"
      )
    ) +
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())


  if (nrow(a) > 0) {
    print(Rchart + ggrepel::geom_label_repel(
      data = a,
      aes(
        x = a$Process.Sample,
        y = a$median,
        label = a$Process.Sample,
        fill = "red"
      ),
      colour = "white",
      size = 3.5
    ))
  } else {
    print(Rchart)
  }
}
