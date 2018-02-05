# Read library
library(ggplot2)
library(DAAG)


# Set working directory
setwd("D://UW-Madison//17-18 Spring//Stat 628//Module 1")


# Read data
bodyfat <- read.csv("BodyFat.csv")


# Variance display
body_cir <- bodyfat[, c(8:17)]
body_cirvar <- data.frame("Type" = as.factor(colnames(body_cir)),
                          "Var" = rep(0, length(c(8:17))))
var_m <- cov(body_cir)
for (i in 1:ncol(var_m)) {
  body_cirvar[i, 2] <- var_m[i, i]
}
ggplot(data = body_cirvar) + geom_point(aes(x = Type, y = Var, size = Var)) + 
  labs(title = "Variance of each circumference measurements")


# Scale circumference data w.r.t wrist(least variance)
bodyfat_m <- bodyfat
target <- c(8:17)
for (i in target) {
  for (j in 1:nrow(bodyfat)) {
    bodyfat_m[j, i] <- bodyfat_m[j, i]/bodyfat_m[j, 17]
  }
}


# Simple preprocess
# Distribution & Normality check
for (i in 1:ncol(bodyfat_m)) {
  # Histogram
  col_hist <- ggplot(data = bodyfat_m, aes(x = bodyfat_m[, i])) + 
    geom_histogram(bins = 30, color = "black", fill = "grey") +
    labs(x = colnames(bodyfat_m)[i])
  # QQ Plot
  y <- quantile(x = bodyfat_m[, i], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1] - slope*x[1]
  col_qq <- ggplot(data = bodyfat_m, aes(sample = bodyfat_m[, i])) + 
    geom_qq() + geom_abline(slope = slope, intercept = int) +
    labs(x = colnames(bodyfat_m)[i])
  # Print out
  print(col_hist)
  print(col_qq)
}



# Eliminate error data
# Delete 0 bodyfat, unreasonable records
bodyfat_m <- bodyfat_m[-c(39, 182),]


# Examining linear model
# Extract concerned parameters
bodyfat_n <- bodyfat_m[, -c(1, 3, 5, 6, 17)]
# Create naive empty and full models
body_null <- lm(BODYFAT ~ 1, data = bodyfat_n)
body_full <- lm(BODYFAT ~ ., data = bodyfat_n)
# Simple forward selection 
step(body_null, scope = list(lower = body_null, upper = body_full), direction = "forward")

# Forward results
body_f <- lm(BODYFAT ~ ABDOMEN + HIP + ADIPOSITY + CHEST + AGE + 
               NECK + FOREARM, data = bodyfat_n)
# Ideal model based on prior information & reasonable derivation
body_t <- lm(BODYFAT ~ ABDOMEN + ADIPOSITY + AGE, data = bodyfat_n)


# Model diagnosis
par(mfrow = c(2, 2))
# Naive diagnosis
plot(body_f)
plot(body_t)
layout(1)
# Residuals intervals
summary(body_t$residuals)
# Cross validation
# cv.lm(data = bodyfat_n, form.lm = body_t, m = 5)


# Robustness validation
body_jit <- bodyfat_m[, c(10, 7, 4)]
body_jit[, 1] <- jitter(body_jit[, 1], amount = 0.5)
body_jit[, 2] <- jitter(body_jit[,2], amount = 0.2)



