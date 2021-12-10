n929_1 <- as.matrix(rnorm(125, mean = 12.0565, sd = 0.443 ))
n929_2 <- as.matrix(rnorm(125, mean = 12.0565, sd = 0.443 ))
n929_3 <- as.matrix(rnorm(125, mean = 12.0565, sd = 0.443 ))
n929_4 <- as.matrix(rnorm(125, mean = 12.0565, sd = 0.443 ))
n929_5 <- as.matrix(rnorm(125, mean = 12.0565, sd = 0.443 ))
n929_6 <- as.matrix(rnorm(125, mean = 12.0565, sd = 0.443 ))

name_929 <- sample(c("Marc-Olivier", "Valérie", "Iegor", "Samuel", "Sophie", "Ozgür"), 125, replace = TRUE)
cible_929 <- rep(10, 125)
tare_929 <- rep(0.89, 125)
test_929 <- seq(1,125)
size_929 <- rep("M", 125)

batch929 <- cbind(
  name_929,
  test_929,
  size_929,
  n929_1,
  n929_2,
  n929_3,
  n929_4,
  n929_5,
  n929_6,
  cible_929,
  tare_929
)







n1003_1 <- as.matrix(rnorm(103, mean = 8.89298, sd = 0.34157 ))
n1003_2 <- as.matrix(rnorm(103, mean = 8.89298, sd = 0.34157 ))
n1003_3 <- as.matrix(rnorm(103, mean = 8.89298, sd = 0.34157 ))
n1003_4 <- as.matrix(rnorm(103, mean = 8.89298, sd = 0.34157 ))
n1003_5 <- as.matrix(rnorm(103, mean = 8.89298, sd = 0.34157 ))
n1003_6 <- as.matrix(rnorm(103, mean = 8.89298, sd = 0.34157 ))

name_1003 <- sample(c("Marc-Olivier", "Valérie", "Iegor", "Samuel", "Sophie", "Ozgür"), 103, replace = TRUE)
cible_1003 <- rep(5.5, 103)
tare_1003 <- rep(0.93, 103)
test_1003 <- seq(1, 103)
size_1003 <- rep("S", 103)

batch1003 <- cbind(
  name_1003,
  test_1003,
  size_1003,
  n1003_1,
  n1003_2,
  n1003_3,
  n1003_4,
  n1003_5,
  n1003_6,
  cible_1003,
  tare_1003
)

n351_1 <- as.matrix(rnorm(89, mean = 14.877, sd = 4.5 ))
n351_2 <- as.matrix(rnorm(89, mean = 14.877, sd = 4.5 ))
n351_3 <- as.matrix(rnorm(89, mean = 14.877, sd = 4.5 ))
n351_4 <- as.matrix(rnorm(89, mean = 14.877, sd = 4.5 ))
n351_5 <- as.matrix(rnorm(89, mean = 14.877, sd = 4.5 ))
n351_6 <- as.matrix(rnorm(89, mean = 14.877, sd = 4.5 ))

name_351 <- sample(c("Marc-Olivier", "Valérie", "Iegor", "Samuel", "Sophie", "Ozgür"), 89, replace = TRUE)
cible_351 <- rep(5.5, 89)
tare_351 <- rep(0.93, 89)
test_351 <- seq(1, 89)
size_351 <- rep("L", 89)

batch351 <- cbind(
  name_351,
  test_351,
  size_351,
  n351_1,
  n351_2,
  n351_3,
  n351_4,
  n351_5,
  n351_6,
  cible_351,
  tare_351
)




df <- rbind(batch929, batch1003, batch351)
