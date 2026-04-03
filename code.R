
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
    m_num = month(date),
    # Task 2: Days of week
    dow = factor(dow, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), ordered = TRUE),
    # Task 3: Trend (Hard-coded date is safer than date[1])
    trend = as.numeric(date - as.Date("2020-01-01")),
    is_covid = ifelse(date >= "2020-03-23" & date <= "2021-03-31", 1, 0),
    is_holiday = ifelse(m_num == 12 & day(date) >= 24, 1, 0),
    # Task 1: Month labels
    month = factor(m_num, levels = 1:12, labels = month.abb, ordered = TRUE)
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
  scale_x_date(
    date_breaks = "1 year",      
    date_labels = "%b %Y",     
    expand = c(0, 0), 
    limits = c(as.Date("2020-01-01"), as.Date("2025-12-31"))
  ) +  
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

#Boxplot by temp mean
plot_temp_scatter <- ggplot(cycle_daily_df, aes(x = temp_mean, y = count)) +
  geom_point(alpha = 0.3) + 
  geom_smooth(aes(color = "Linear"), method = "lm", se = TRUE) +
  geom_smooth(aes(color = "Quadratic"), method = "lm", formula = y ~ x + I(x^2)) +
  
  scale_color_manual(name = "Different", 
                     values = c("Linear" = "red", "Quadratic" = "blue")) +
  labs(
    title = "Cycling Count vs. Mean Temperature",
    x = "Mean Daily Temperature (°C)",
    y = "Total Daily Cyclist Count"
  ) +
  theme_minimal()

# Holiday boxplots
plot_holiday <- ggplot(cycle_daily_df, aes(x = factor(is_holiday), y = count)) +
  geom_boxplot() +
  labs(title = "Impact of Festive Period on Daily Cycle Count",
       x = "Is Holiday (0=No, 1=Yes)", y = "Cycle Count") +
  theme_minimal()

# COVID variable
plot_covid <- ggplot(cycle_daily_df, aes(x = date, y = count, color = factor(is_covid))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(title = "Cycling Trends: Non Covid vs. Covid Era") +
  theme_minimal()

# Non trivial plot - festive period
# Filter for December and calculate the average count for each day across all years
december_trends <- cycle_daily_df %>%
  filter(month(date) == 12) %>%
  mutate(day = day(date)) %>%
  group_by(day) %>%
  summarise(avg_count = mean(count, na.rm = TRUE))

# Create the plot
plot_december <-ggplot(december_trends, aes(x = day, y = avg_count)) +
  geom_line(linewidth = 1) +
  labs(title = "Average Cycling Demand in December",
       x = "Day in December",
       y = "Average Daily Cyclists") +
  theme_minimal()

# 3. Model Fitting
# Note: Use factor(month) in formulas for M1-M3
m0 <- lm(count ~ temp_mean + as.numeric(weekend) + as.numeric(month), 
         data = cycle_daily_df)

#m1_literal has collinear terms so we define the factor versions of month and 
#dow and omit the weekend.

m1 <- lm(count ~ temp_mean + trend + factor(month) + factor(dow), 
         data = cycle_daily_df)

m2 <- lm(count ~ temp_mean + I(temp_mean^2)  + trend + 
           factor(month) + factor(dow), data = cycle_daily_df )

m3 <- lm(log(count+1) ~ temp_mean + I(temp_mean^2) + trend + 
           factor(month) + factor(dow)  + is_covid + is_holiday, data = cycle_daily_df)

# Define named list once
models_list <- list(
  m0 = formula(m0),
  m1 = formula(m1),
  m2 = formula(m2),
  m3 = formula(m3)
)

# ggplots for models:
m0_fort <- fortify(m0)
m1_fort <- fortify(m1)
m2_fort <- fortify(m2)
m3_fort <- fortify(m3)

# Helper function to mimic Base R's plot(lm)
gg_diagnostic <- function(model_fort, type = 1, mod_name = "") {
  if (type == 1) { # Residuals vs Fitted
    ggplot(model_fort, aes(.fitted, .resid)) +
      geom_point(alpha = 0.5) +
      geom_smooth(se = FALSE, color = "red", method = "loess") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = paste(mod_name, "Residuals vs Fitted"), x = "Fitted values", y = "Residuals") +
      theme_bw()
  } else if (type == 2) { # Normal Q-Q
    ggplot(model_fort, aes(sample = .stdresid)) +
      stat_qq() + stat_qq_line(color = "red") +
      labs(title = paste(mod_name, "Normal Q-Q"), x = "Theoretical Quantiles", y = "Standardized Residuals") +
      theme_bw()
  } else if (type == 3) { # Scale-Location
    ggplot(model_fort, aes(.fitted, sqrt(abs(.stdresid)))) +
      geom_point(alpha = 0.5) +
      geom_smooth(se = FALSE, color = "red", method = "loess") +
      labs(title = paste(mod_name, "Scale-Location"), x = "Fitted values", y = expression(sqrt("|Standardized residuals|"))) +
      theme_bw()
  } else if (type == 4) { # Residuals vs Leverage
    ggplot(model_fort, aes(.hat, .stdresid)) +
      geom_point(alpha = 0.5) +
      geom_smooth(se = FALSE, color = "red", method = "loess") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = paste(mod_name, "Residuals vs Leverage"), x = "Leverage", y = "Standardized Residuals") 
  } + theme_bw()
}

p_m0_1 <- gg_diagnostic(m0_fort, 1, "M0")
p_m1_1 <- gg_diagnostic(m1_fort, 1, "M1")
p_m2_1 <- gg_diagnostic(m2_fort, 1, "M2")
p_m3_1 <- gg_diagnostic(m3_fort, 1, "M3")

p_m0_2 <- gg_diagnostic(m0_fort, 2, "M0")
p_m1_2 <- gg_diagnostic(m1_fort, 2, "M1")
p_m2_2 <- gg_diagnostic(m2_fort, 2, "M2")
p_m3_2 <- gg_diagnostic(m3_fort, 2, "M3")

p_m0_3 <- gg_diagnostic(m0_fort, 3, "M0")
p_m1_3 <- gg_diagnostic(m1_fort, 3, "M1")
p_m2_3 <- gg_diagnostic(m2_fort, 3, "M2")
p_m3_3<- gg_diagnostic(m3_fort, 3, "M3")

p_m0_4<- gg_diagnostic(m0_fort, 4, "M0")
p_m1_4 <- gg_diagnostic(m1_fort, 4, "M1")
p_m2_4 <- gg_diagnostic(m2_fort, 4, "M2")
p_m3_4<- gg_diagnostic(m3_fort, 4, "M3")


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
  
  # Synthesise table
  return (data.frame(Model = model_name, RMSE = RMSE, MAE = MAE, DS = DS, IS = IS))
}
calc_scores <- function(y, mu, sigma, alpha = 0.05, model_name = "Model") {
  # RMSE
  RMSE <- sqrt(mean((y - mu)^2, na.rm = TRUE))
  
  # MAE
  MAE <- mean(abs(y - mu), na.rm = TRUE)
  
  # DS
  DS <- mean(log(sigma^2) + ((y - mu)^2 / sigma^2), na.rm = TRUE)
  
  # IS
  lower <- mu - qnorm(1 - alpha/2) * sigma
  upper <- mu + qnorm(1 - alpha/2) * sigma
  
  IS <- mean((upper - lower) + 
               (2/alpha) * pmax(0, lower - y, na.rm = TRUE) + 
               (2/alpha) * pmax(0, y - upper, na.rm = TRUE), na.rm = TRUE)
  
  # Return as a clean data frame for bind_rows() later
  return(data.frame(
    Model = model_name, 
    RMSE = RMSE, 
    MAE = MAE, 
    DS = DS, 
    IS = IS
  ))
}

# 5. Leave-One-Year-Out CV Loop 
years <- sort(unique(cycle_daily_df$year))
table1_results <- list()

for (y in years) {
  # Split data
  train_data <- subset(cycle_daily_df, year != y)
  test_data  <- subset(cycle_daily_df, year == y)
  
  for (mod_name in names(models_list)) {
    # Fit model
    fit <- lm(models_list[[mod_name]], data = train_data)
    
    # Get predictions and standard errors
    pred_obj <- predict(fit, newdata = test_data, se.fit = TRUE)
    mu_raw <- pred_obj$fit
    
    # log-model (m3) changes
    if (mod_name == "m3") {
      # Total variance on the log scale
      total_log_var <- summary(fit)$sigma^2 + pred_obj$se.fit^2
      # Back-transform to the original scale 
      mu <- exp(mu_raw + (total_log_var / 2)) - 1
      sigma <- sqrt((exp(total_log_var) - 1) * exp(2 * mu_raw + total_log_var))
      
    } else {
      #Defined as expected
      mu <- mu_raw
      sigma <- sqrt(summary(fit)$sigma^2 + pred_obj$se.fit^2)
    }
    # Calculate scores 
    scores <- calc_scores(y = test_data$count, mu = mu, sigma = sigma, model_name = mod_name)
    
    # Store 
    table1_results[[paste(y, mod_name, sep="_")]] <- scores %>% mutate(Split_Year = y)
  }
}
# Collect results
table1_final <- bind_rows(table1_results) %>%
  group_by(Model) %>%
  summarise(across(c(RMSE, MAE, DS, IS), mean, na.rm = TRUE))

print(table1_final)

m1_full <- lm(models_list$m1, data = cycle_daily_df)
m2_full <- lm(models_list$m2, data = cycle_daily_df)
m3_full <- lm(models_list$m3, data = cycle_daily_df)

mbest <- m2_full 

months_list <- month.abb
table2_results <- list()

# Get the residual variance once from the full model
res_var <- summary(mbest)$sigma^2

for (m in months_list) {
  
  # Extract data for the specific month
  test_data <- subset(cycle_daily_df, month == m)
  
  # Skip if no data for that month
  if(nrow(test_data) == 0) next 
  
  # Predict on the month using the best model
  # We use se.fit = TRUE to get the uncertainty of the model fit
  pred_obj <- predict(mbest, newdata = test_data, se.fit = TRUE)
  
  # Since m2 is linear
  mu_final <- pred_obj$fit
  
  # Calculate predictive sigma
  sigma_final <- sqrt(res_var + pred_obj$se.fit^2)
  
  # Run calc_scores 
  scores <- calc_scores(y = test_data$count, 
                        mu = mu_final, 
                        sigma = sigma_final, 
                        model_name = "m2")
  
  # Store result
  table2_results[[m]] <- data.frame(
    Month = m, 
    RMSE = scores$RMSE, 
    DS = scores$DS
  )
}

# Round and collect
table2_final <- bind_rows(table2_results) %>%
  mutate(
    RMSE = round(RMSE, 1),
    DS = round(DS, 2)
  )

print(table2_final)

# 5.1 
# test and train
n = nrow(cycle_daily_df)
set.seed(1234)

idx <- sample(1:n, size = round(0.7 * n))

# Define the two datasets
train <- cycle_daily_df[idx, ] 
test  <- cycle_daily_df[-idx, ] 

M2_Tmean <-lm(count ~ temp_mean + I(temp_mean^2) + trend + 
                
                factor(month) + factor(dow), data = train )

M2_Tmin <- lm(count ~ temp_min + I(temp_min^2) + trend + 
                
                factor(month) + factor(dow), data = train )

M2_Tmax <- lm(count ~ temp_max + I(temp_max^2) + trend + 
                
                factor(month) + factor(dow), data = train )


m2_scores <- function(model, data = test) {
  mu <- predict(model, data = test)
  se <- predict(model,data = test, se.fit = TRUE)$se.fit
  sigma <- sqrt(summary(model)$sigma^2 + se^2)
  calc_scores(y = test$count, mu = mu, sigma = sigma)
}

# Table
m2_comparison <- bind_rows(
  "Temp Mean" = m2_scores(M2_Tmean, test),
  "Temp Min"  = m2_scores(M2_Tmin, test),
  "Temp Max"  = m2_scores(M2_Tmax, test),
  .id = "Model")

# Estimated marginal effect with temp_min
# b1, linear slope and  b2 is the curvature coefficient
b1 <- coef(M2_Tmin)["temp_min"]
b2 <- coef(M2_Tmin)["I(temp_min^2)"]

# Calculate the Slope at 5C and 15C
# Formula: Slope = b1 + (2 * b2 * Temperature)
effect_at_5  = b1 + (2 * b2 * 5)
effect_at_15 = b1 + (2 * b2 * 15)


# 5.3
#table 3 - trend coefficients from M1 - M3

# Retrieve trend from each model
tr1 <- coef(m1_full)[["trend"]]
tr2 <- coef(m2_full)[["trend"]]
tr3_log <- coef(m3_full)[["trend"]]

#calculating average 
avg_daily_count <- mean(cycle_daily_df$count, na.rm = TRUE)

#Calculating the annual and daily changes for M3 (average count * percent change over a year) then divide by 365 for daily
annual_m3 <- avg_daily_count * (exp(tr3_log * 365) - 1)
daily_m3 <- annual_m3 / 365

# Building table 3
table3_final <- data.frame(
  #defining the rows as the 3 models
  Model = c("M1", "M2", "M3"),
  `Daily change (cyclists/day)` = c(tr1, tr2, daily_m3), #defining cyclists per day
  `Annual change (cyclists/year)` = c(tr1 * 365, tr2 * 365, annual_m3), #defining cyclists per year
  check.names = FALSE
) %>%
  mutate(
    `Daily change (cyclists/day)` = round(`Daily change (cyclists/day)`, 3), #rounding to 3
    `Annual change (cyclists/year)` = round(`Annual change (cyclists/year)`, 0) #rounding to 0
  )

table3_final

#Extrapolation for 5.3
#defining the dates for the plot, using month as july as lowest scores and day as wednesday
future_dates <- seq(as.Date("2020-01-01"), as.Date("2035-12-31"), by = "day")
extrapolation_df <- data.frame(date = future_dates, trend= as.numeric(future_dates - as.Date("2020-01-01")),
                 temp_mean = mean(cycle_daily_df$temp_mean, na.rm = TRUE),
                 month = "Jul", levels = levels(cycle_daily_df$month), dow = factor("Wed", levels = levels(cycle_daily_df$dow)))

#predicting using m2
extrapolation_df$predicted_count <- predict(m2_full, newdata = extrapolation_df)

#finding the date of 10k intersection
intersect_10k <- extrapolation_df %>%
  filter(abs(predicted_count - 10000) == min(abs(predicted_count - 10000))) %>%
  pull(date)

# finding the date for the 5k intersection
intersect_5k <- extrapolation_df %>%
  filter(abs(predicted_count - 5000) == min(abs(predicted_count - 5000))) %>%
  pull(date)

#plotting the extrapolation plot 
extrapolation_plot <- ggplot(extrapolation_df, aes(x = date, y = predicted_count)) +
  geom_line() + 
  labs(title = "Projected Cycling Demand to 2035 using Model M2",
                     subtitle = "Holding weather and seasonality constant",
                     x = "Year", y = "Projected Daily Count") +
  theme_minimal() +
  #adding lines to represent the thresholds
  geom_hline(yintercept = 10000, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 5000, color = "blue", linetype = "dashed") + 
  #adding the labels to show the date of intersection
  annotate("text", x = as.Date("2027-01-01"), y = 10400, 
           label = intersect_10k, color = "red", hjust = 0,  vjust = -0.5) +
  
  annotate("text", x = as.Date("2033-01-01"), y = 5400, 
           label = intersect_5k, color = "blue", hjust = 0, vjust = -0.5)



# Plot option 2
cycle_daily_df <- cycle_daily_df %>%
  mutate(
    residuals = count - predict(m2, newdata = .),
    month_numeric = as.numeric(month(date)) + (day(date)-1)/31
  )

plot_residuals_years <- ggplot(cycle_daily_df, 
                                 aes(x = month_numeric, y = residuals, group = year, color = factor(year))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE, linewidth = 1.1) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_discrete(name = "Year") +
  labs(
    title = "Residuals vs Time for each year plot over months",
    x = "Month",
    y = "Residuals"
  ) +
  theme_minimal()

# Isolating the poorly fit period (April 2022)
april_2022_data <- cycle_daily_df %>%
  filter(year == 2022, month == "Apr")

# Calculate residuals using m2
apr_preds <- predict(m2, newdata = april_2022_data)
apr_residuals <- april_2022_data$count - apr_preds

# Mean Residual and RMSE
mean_res_apr22 <- mean(apr_residuals)
rmse_apr22 <- sqrt(mean(apr_residuals^2))

# Interesting diagnostic plot 

# Extract Cook's Distance 
cycle_daily_df$cooks_d <- cooks.distance(mbest)
cycle_daily_df$obs_index <- 1:nrow(cycle_daily_df)

# Faceted plot
faceted_cooks_plot <- ggplot(cycle_daily_df, aes(x = obs_index, y = cooks_d)) +
  geom_segment(aes(xend = obs_index, yend = 0), color = "steelblue", alpha = 0.7) +
  geom_hline(yintercept = 4/nrow(cycle_daily_df), linetype = "dashed", color = "red") +
  facet_wrap(~factor(month, levels = month.abb), scales = "free_x") +
  labs(
    title = "Faceted Cook's Distance by Month",
    subtitle = "Identifying which seasonal outliers exert the most influence on the model",
    x = "Observation Index",
    y = "Cook's Distance"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        strip.background = element_rect(fill = "gray95"))
