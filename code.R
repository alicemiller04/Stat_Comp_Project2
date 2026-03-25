
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
# Note: Use factor(month) in formulas for M1-M3
m0 <- lm(count ~ temp_mean + as.numeric(weekend) + as.numeric(month), 
         data = cycle_daily_df)

## m0 + factor(month) unsure what double parameters - choose factor version to avoid collinearity
m1_literal<- lm(count ~ temp_mean + as.numeric(weekend) + as.numeric(month) + 
           trend + factor(month) + factor(dow), 
         data = cycle_daily_df)
##m1_literal has collinear terms so we define the factor versions of month and 
#dow and omit the weekend.

m1 <- lm(count ~ temp_mean + trend + factor(month) + factor(dow), 
         data = cycle_daily_df)

m2 <- lm(count ~ I(temp_mean^2) + as.numeric(weekend) + trend + 
           factor(month) + factor(dow), data = cycle_daily_df )


#m3 <- lm()



#diagnostic plots for first 3 models  
# 
#  Residuals vs fitted and normal QQ diagnostic plots, side by side.
#  Brief assessment: what does M0 get right and wrong? 
#  What motivates M1?

#m0

# Add residuals and fitted values to the dataframe.
m0_diag <- augment(m0)

# Plot Residuals vs Fitted for m0
p1_m0 <- ggplot(m0_diag, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Plot Normal Q-Q for m0
p2_m0 <- ggplot(m0_diag, aes(sample = .resid)) +
  stat_qq(alpha = 0.5) +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# m1 
# Add residuals and fitted values to the dataframe.
m1_diag <- augment(m1)

# Residuals vs Fitted for M1
p1_m1 <- ggplot(m1_diag, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "M1: Residuals vs Fitted",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Normal Q-Q Plot for M1
p2_m1 <- ggplot(m1_diag, aes(sample = .resid)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line(color = "red") +
  labs(title = "M1: Normal Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()


#m2
# Add residuals and fitted values to the dataframe.
m2_diag <- augment(m2)

# Residuals vs Fitted for M2
p1_m2 <- ggplot(m2_diag, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "M2: Residuals vs Fitted",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Normal Q-Q Plot for M2
p2_m2 <- ggplot(m2_diag, aes(sample = .resid)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line(color = "red") +
  labs(title = "M2: Normal Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()





# 4. Cross-Validation Functions

# 4. Cross-Validation Functions

calc_scores <- function(y, mu, sigma, alpha = 0.05, model_name = "Model") {
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
  return (data.frame(Model = model_name, RMSE = RMSE, MAE = MAE, DS = DS, IS = IS))
}





# 4. Cross-Validation Functions

calc_scores <- function(y, mu, sigma, alpha = 0.05, model_name = "Model") {
  # y     : vector of observed values
  # mu    : vector of predictive means (from predict(fit, newdata=test)$fit)
  # sigma : vector of predictive SDs. Combine residual error and mean uncertainty:
  #         sigma = sqrt(summary(fit)$sigma^2 +
  # predict(fit, newdata=test, se.fit=TRUE)$se.fit^2)
  # Returns a named list with RMSE, MAE, DS, IS
  
  # Implement RMSE, MAE, DS, and IS here
  
  #RMSE
  RMSE <- sqrt(mean((y - mu)^2, na.rm = TRUE))
  
  #MAE
  MAE <- mean(abs(y - mu))
  
  #DS
  DS <- mean(log(sigma^2) + ((y - mu)^2 / sigma^2), na.rm = TRUE)
  
  #IS
  lower <- mu - (qnorm(1-alpha/2) * sigma)
  upper <-mu + (qnorm(1-alpha/2) * sigma)
  IS <-  mean((upper - lower) + 
                (2/alpha)* pmax(0,(lower - y)) + 
                (2/alpha)*pmax(0,(y - upper)))
  
  # make a table with the results
  return (data.frame(Model = model_name, RMSE = RMSE, MAE = MAE, DS = DS, IS = IS))
}



# 5. Leave-One-Year-Out CV Loop 
# Define models in a list ###change formulas!!!
models_list <- list(
  m0 = count ~temp_mean + as.numeric(weekend) + as.numeric(month),
  m1 = count~ temp_mean + as.numeric(weekend) + as.numeric(month), 
  m2 = count~ temp_mean + as.numeric(weekend) + as.numeric(month), 
  m3 = count~ temp_mean + as.numeric(weekend) + as.numeric(month))

cycle_daily_df$date <- as.Date(cycle_daily_df$date)
cycle_daily_df$year <- year(cycle_daily_df$date)
years <- sort(unique(cycle_daily_df$year))
table1_results <- list()

for (y in years) {
  # Split: Extract one year for testing
  train_data <- subset(cycle_daily_df, year != y)
  test_data  <- subset(cycle_daily_df, year == y)
  
  for (mod_name in names(models_list)) {
    # Fit the model on the remaining years
    fit <- lm(models_list[[mod_name]], data = train_data)
    
    # Get predictive means (mu) and standard errors (se.fit)
    pred_obj <- predict(fit, newdata = test_data, se.fit = TRUE)
    mu <- pred_obj$fit
    
    # Calculate total predictive SD (sigma)
    # Combining residual variance and uncertainty in the mean
    res_std_error <- summary(fit)$sigma
    sigma <- sqrt(res_std_error^2 + pred_obj$se.fit^2)
    
    # Run  calc_scores 
    scores <- calc_scores(y = test_data$count, mu = mu, sigma = sigma, model_name = mod_name)
    
    # Store result with identifiers
    table1_results[[paste(y, mod_name, sep="_")]] <- scores %>% mutate(Split_Year = y)
    
  }
}

#finding the average scores for each model and displaying
all_cv_results <- bind_rows(table1_results)
table1_final <- all_cv_results %>%
  group_by(Model) %>%
  summarise(across(c(RMSE, MAE, DS, IS), mean))
table1_final


#Table 2, CV RMSE and DS by month for final model 
mbest = models_list$m3 

months_list <- month.name
table2_results <- list()

for (m in months_list){
  
  #extract one month to test 
  train_data <- subset(cycle_daily_df, month(date, label = TRUE, abbr = FALSE) != m)
  test_data  <- subset(cycle_daily_df, month(date, label = TRUE, abbr = FALSE) == m)
  
  # Fit the model on the remaining months
  fit <- lm(mbest, data = train_data)
  
  # predict on the other month
  pred_obj <- predict(fit, newdata = test_data, se.fit = TRUE)
  
  # Calculate total predictive SD (sigma)
  # Combining residual variance and uncertainty in the mean
  res_std_error <- summary(fit)$sigma
  sigma <- sqrt(res_std_error^2 + pred_obj$se.fit^2)
  
  # Run calc_scores
  scores <- calc_scores(y = test_data$count, mu = pred_obj$fit, sigma = sigma, model_name = mod_name)
  
  # Store result with identifiers
  table2_results[[m]] <- data.frame(Month = m, RMSE = scores$RMSE, DS = scores$DS)
}

table2_final <- bind_rows(table2_results) %>%
  mutate(
    RMSE = round(RMSE, 1),
    DS = round(DS, 2)
  )

table2_final

#table 3 - trend coefficients from M1 - M3
# 
# #fitting full models
# m1_full <- lm(models_list$m1, data = cycle_daily_df)
# m2_full <- lm(models_list$m2, data = cycle_daily_df)
# m3_full <- lm(models_list$m3, data = cycle_daily_df)
# 
# #getting the trend from each model
# tr1 <- coef(m1_full)[["trend"]]
# tr2 <- coef(m2_full)[["trend"]]
# tr3 <- coef(m3_full)[["trend"]]
# 
# #building table 3
# table3_final <- data.frame(
#   #defining the rows as the 3 models
#   Model = c("M1", "M2", "M3"),
#   `Daily change (cyclists/day)` = c(tr1, tr2, tr3), #defining cyclists per day
#   `Annual change (cyclists/year)` = c(tr1 * 365, tr2 * 365, tr3 * 365), #defining cyclists per year
#   check.names = FALSE
# ) %>%
#   mutate(
#     `Daily change (cyclists/day)` = round(`Daily change (cyclists/day)`, 3), #rounding to 3
#     `Annual change (cyclists/year)` = round(`Annual change (cyclists/year)`, 0) #rounding to 0
#   )
# 
# print(table3_final)

# 6. CV by Month 
# ...
