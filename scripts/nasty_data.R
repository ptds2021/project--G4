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
request_929 <- rep(929, 125)

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
  tare_929,
  request_929
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
request_1003 <- rep(1003, 125)

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
  tare_1003,
  request_1003
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
request_351 <- rep(351, 125)

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
  tare_351,
  request_351
)




n4_1 <- as.matrix(rnorm(364, mean = 18.84, sd = 0.72 ))
n4_2 <- as.matrix(rnorm(364, mean = 18.84, sd = 0.72 ))
n4_3 <- as.matrix(rnorm(364, mean = 18.84, sd = 0.72 ))
n4_4 <- as.matrix(rnorm(364, mean = 18.84, sd = 0.72 ))
n4_5 <- as.matrix(rnorm(364, mean = 18.84, sd = 0.72 ))
n4_6 <- as.matrix(rnorm(364, mean = 18.84, sd = 0.72 ))

cible_4 <- rep(19, 364)
name_4 <- sample(c("Marc-Olivier", "Valérie", "Iegor", "Samuel", "Sophie", "Ozgür"), 364, replace = TRUE)
tare_4 <- rep(0.2, 364)
test_4 <- seq(1, 364)
size_4 <- rep("XL", 364)
request_4 <- rep(4, 125)



batch4 <- cbind(
  name_4,
  test_4,
  size_4,
  n4_1,
  n4_2,
  n4_3,
  n4_4,
  n4_5,
  n4_6,
  cible_4,
  tare_4,
  request_4
)




n254_1 <- as.matrix(rnorm(289, mean = 9.53, sd = 2.4 ))
n254_2 <- as.matrix(rnorm(289, mean = 9.53, sd = 2.4 ))
n254_3 <- as.matrix(rnorm(289, mean = 9.53, sd = 2.4 ))
n254_4 <- as.matrix(rnorm(289, mean = 9.53, sd = 2.4 ))
n254_5 <- as.matrix(rnorm(289, mean = 9.53, sd = 2.4 ))
n254_6 <- as.matrix(rnorm(289, mean = 9.53, sd = 2.4 ))

cible_254 <- rep(15, 289)
name_254 <- sample(c("Marc-Olivier", "Valérie", "Iegor", "Samuel", "Sophie", "Ozgür"), 289, replace = TRUE)
tare_254 <- rep(1.5, 289)
test_254 <- seq(1, 289)
size_254 <- rep("XL", 289)
request_254 <- rep(254, 125)


batch254 <- cbind(
  name_254,
  test_254,
  size_254,
  n254_1,
  n254_2,
  n254_3,
  n254_4,
  n254_5,
  n254_6,
  cible_254,
  tare_254,
  request_254
)


date_df <- sample(c("2021-11-08", "2021-11-01", "2021-10-05", "2021-09-07", "2021-07-19", "2021-07-15"), 970, replace = TRUE)
nasty <- rbind(batch929, batch1003, batch351, batch4, batch254)

nasty <- cbind(date_df, nasty)
colnames(nasty) <- c("Date", "Operator", "Process Sample", "Product Size", "Measure1", "Measure2", "Measure3", "Measure4", "Measure5", "Measure6", "Target Value", "Tare", "Request")



xlsx::write.xlsx2(nasty, file = "nasty.xlsx")
