
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
library(patchwork); library(broom); library(gridExtra)

# 1. Load data
load('cycle_daily_df.Rdata')

# 2.1 Data Wrangling 

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

# 2.2 exploratory plots

# Summary table for count, temp_mean, temp_min, temp_max
summary_stats <- cycle_daily_df %>%
  summarise(across(c(count, temp_mean, temp_min, temp_max), 
                   list(Mean = ~mean(.x), 
                        SD = ~sd(.x), 
                        Min = ~min(.x), 
                        Max = ~max(.x)),
                   .names = "{.col}.{.fn}")) %>% # use dots in variable names instead of underscore
  # Reshape for a vertical table
  pivot_longer(everything(), 
               names_to = c("Variable", "Statistic"), 
               names_sep = "\\.") %>% # Split at the dot instead of the underscore
  pivot_wider(names_from = Statistic, values_from = value)


# Time series of daily counts 2020-2025 with a smoother
plot_timeseries <- ggplot(cycle_daily_df, aes(x = date, y = count)) +
  geom_line(alpha = 0.4, color = "gray") + 
  geom_smooth(method = "gam", color = "blue", se = TRUE) + # The smoother line
  scale_x_date(date_breaks = "12 months", date_labels = "%b %Y") + 
  labs(
    title = "Daily Cycling Count in Edinburgh (2020-2025)",
    x = "Date",
    y = "Total Daily Cyclist Count"
  ) +
  theme_minimal()

# Boxplots of count by month and by day of week.

# Boxplot by Month 
plot_month <- ggplot(cycle_daily_df, aes(x = month, y = count)) + # Use fill = month to add colour
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Cycling Demand by Month", x = "Month", y = "Count") +
  theme_minimal()

# Boxplot by Day of Week
plot_dow <- ggplot(cycle_daily_df, aes(x = dow, y = count)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Cycling Demand by Day of Week", x = "Day", y = "Count") +
  theme_minimal()

# Scatter plot of count vs mean temperature
plot_temp_scatter <- ggplot(cycle_daily_df, aes(x = temp_mean, y = count)) +
  geom_point(alpha = 0.3) + # alpha changes the opaqueness of the points
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Cycling Count vs. Mean Temperature",
    x = "Mean Daily Temperature (°C)",
    y = "Total Daily Cyclist Count"
  ) +
  theme_minimal()

##non trivial plot 
plot_weekend_seas <- ggplot(cycle_daily_df, aes(x = month, y = count, fill = factor(weekend))) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) + # Clean boxes for the "average"
  theme_minimal() +
  scale_fill_manual(values = c("0" = "#56B4E9", "1" = "#E69F00"), 
                    labels = c("Weekday", "Weekend"), name = "Day Type") +
  labs(title = "Distribution of Daily Cycle Counts by Month and Day Type",
       x = "Month", 
       y = "Number of Cyclists")


# 3. Model Fitting

# set the seed, defined test and training data, 
n = 2192
set.seed(1234) # for reproducibility 
idx <- sample(1:n, size = round(0.7*n))
train <- cycle_daily_df[idx, ]
test  <- cycle_daily_df[-idx, ]

# Note: Use factor(month) in formulas for M1-M3
m0 <- lm(count ~ temp_mean + as.numeric(weekend) + as.numeric(month), data = train)
## m0 + factor(month) unsure what double parameters - choose factor version to avoid collinearity
m1 <- lm(count ~ temp_mean + as.numeric(weekend) + trend + 
           factor(month) + factor(dow), data = train)
m2 <- lm(count ~ I(temp_mean^2) + as.numeric(weekend) + trend + 
           factor(month) + factor(dow), data = train )
#m3 <- lm()



#diagnostic plots for first 3 models  
# 
#  Residuals vs fitted and normal QQ diagnostic plots, side by side.
#  Brief assessment: what does M0 get right and wrong? 
#  What motivates M1?

#m0

par(mfrow = c(2, 2))
plot(m0)

#m1 
plot(m1)

#m2
plot(m2)





# 4. Cross-Validation Functions

calc_scores <- function(y, mu, sigma, alpha = 0.05) {
  # y     : vector of observed values
  # mu    : vector of predictive means (from predict(fit, newdata=test)$fit)
  # sigma : vector of predictive SDs. Combine residual error and mean uncertainty:
  #         sigma = sqrt(summary(fit)$sigma^2 +
  # predict(fit, newdata=test, se.fit=TRUE)$se.fit^2)
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
    (2/alpha)* pmax(0,(lower - y)) + 
    (2/alpha)*pmax(0,(y - upper)))

  # make a table with the results
  return (list(RMSE = RMSE, MAE = MAE, DS = DS, IS = IS))
}

# calc scores for m0
y = test$count
mu0 = predict(m0, newdata=test)
pred_obj <- predict(m0, newdata = test, se.fit = TRUE)
sigma0 = sqrt(summary(m0)$sigma^2 + pred_obj$se.fit^2)

calc_scores(y = y, mu = mu0, sigma = sigma0, alpha = 0.05)

y = test$count
mu0 = predict(m1, newdata=test)
pred_obj <- predict(m1, newdata = test, se.fit = TRUE)
sigma0 = sqrt(summary(m1)$sigma^2 + pred_obj$se.fit^2)

calc_scores(y = y, mu = mu0, sigma = sigma0, alpha = 0.05)








# 5. Leave-One-Year-Out CV Loop 
# 6. CV by Month 
# ...
