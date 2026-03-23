
# ============================================================
# Project 2: Modelling daily cycling demand in Edinburgh
# Authors: Alice Miller, Saba Sitter, Jake Saunders
# ============================================================

#
# Place the code needed in the Report_project02.Rmd, including documentation.
#
# 0. Packages
library(ggplot2); library(dplyr); library(tidyr)
library(lubridate); library(knitr); library(kableExtra)
library(patchwork); library(broom)

# 1. Load data
load('cycle_daily_df.Rdata')

# 2. Data Wrangling 

cycle_daily_df <- cycle_daily_df %>%
  mutate(
    # Task 1: month as ordered factor for plots/inference
    month = factor(month,
                    levels = 1:12,    
                    labels = month.abb, 
                    ordered = TRUE),
    
    # Task 2: dow with explicit levels 
    dow = factor(dow,
                levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), 
                ordered = TRUE),
    
    # Task 3: trend as integer days since 2020-01-01
    trend = as.numeric(date - date[1], units = "days")
    ) 

#plots
plot(cycle_daily_df$date,cycle_daily_df$count)
boxplot(count ~ month , data = cycle_daily_df)
boxplot(count ~ dow , data = cycle_daily_df)



# 3. Model Fitting

#set the seed, defined test and training data, 
n = 2192
set.seed(1234)
idx <- sample(1:n, size = round(0.7*n))
train <- cycle_daily_df[idx, ]
test  <- cycle_daily_df[-idx, ]

# switch variable temporarily
train$month_num <- as.numeric(train$month)
test$month_num  <- as.numeric(test$month)


# Note: Use factor(month) in formulas for M1-M3
m0 <- lm(count ~ temp_mean + weekend + month, data = train)
# m1 <- ...
# m2 <- ...
# m3 <- ...

# 4. Cross-Validation Functions

calc_scores <- function(y, mu, sigma, alpha = 0.05) {
  # y     : vector of observed values
  # mu    : vector of predictive means (from predict(fit, newdata=test)$fit)
  # sigma : vector of predictive SDs. Combine residual error and mean uncertainty:
  #         sigma = sqrt(summary(fit)$sigma^2 + predict(fit, newdata=test, se.fit=TRUE)$se.fit^2)
  # Returns a named list with RMSE, MAE, DS, IS
  
  # Implement RMSE, MAE, DS, and IS here
  
  #RMSE
  RMSE <- sqrt(mean((y - mu)^2))
  
  #MAE
  MAE <- mean(abs(y - mu))
  
  #DS
  DS <- mean(log(sigma^2) + ((y - mu)^2 / sigma^2))
  
  #IS
  lower <- mu - (qnorm(1-alpha/2) * sigma)
  upper <-mu + (qnorm(1-alpha/2) * sigma)
  IS <-  mean((upper - lower) + 
    (2/alpha)* (lower - y) + 
    (2/alpha)*(y - upper))

  #make a table with the results
  return (list(RMSE = RMSE, MAE = MAE, DS = DS, IS = IS))
}

#calc scores for m0
y = test$count
mu0 = predict(m0, newdata=test)
pred_obj <- predict(m0, newdata = test, se.fit = TRUE)
sigma0 = sqrt(summary(m0)$sigma^2 + pred_obj$se.fit^2)

calc_scores(y = y, mu = mu0, sigma = sigma0, alpha = 0.05)

# 5. Leave-One-Year-Out CV Loop 
# 6. CV by Month 
# ...
