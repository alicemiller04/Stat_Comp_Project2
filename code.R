
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
    
    #Season indicator
    m_num = month(date),
    is_spring = ifelse(m_num %in% 3:5, 1, 0),
    is_summer = ifelse(m_num %in% 6:8, 1, 0),
    is_autumn = ifelse(m_num %in% 9:11, 1, 0),
    is_winter = ifelse(m_num %in% c(12, 1, 2), 1, 0),
    
    # Task 2: dow with explicit levels 
    dow = factor(dow,
                 levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), 
                 ordered = TRUE),
    
    # Task 3: trend as integer days since 2020-01-01
    trend = as.numeric(date - date[1], units = "days"),
    
    #Covid indicator
    is_covid = ifelse(date >= "2020-03-23" & date <= "2021-03-31",1,0),
    
    #festive period indicator
    is_holiday = ifelse(m_num == 12 & day(date) >=24 & day(date)<=31, 1,0),
    
    #Daily temp range
    temp_range = temp_max - temp_min,
    
    #cold indicator
    is_freezing = ifelse(temp_min <= 0, 1, 0))%>%
  
  mutate(
    
    # Task 1: month as ordered factor for plots/inference
    month = factor(m_num,
                    levels = 1:12,    
                    labels = month.abb, 
                    ordered = TRUE)
    
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
  geom_smooth(method = "lm", color = "red", se = TRUE)+
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = "blue") +
  labs(
    title = "Cycling Count vs. Mean Temperature",
    x = "Mean Daily Temperature (°C)",
    y = "Total Daily Cyclist Count"
  ) +
  theme_minimal()

# non trivial plot - festive period
# Filter for December and calculate the average count for each day across all years
december_trends <- cycle_daily_df %>%
  filter(month(date) == 12) %>%
  mutate(day = day(date)) %>%
  group_by(day) %>%
  summarise(avg_count = mean(count, na.rm = TRUE),
            sd_count = sd(count, na.rm = TRUE))

# Create the plot
plot_decemeber <-ggplot(december_trends, aes(x = day, y = avg_count)) +
  geom_line(color = "darkblue", size = 1) +
  geom_ribbon(aes(ymin = avg_count - sd_count, ymax = avg_count + sd_count), 
              fill = "lightblue", alpha = 0.3) +
  geom_vline(xintercept = 25, linetype = "dashed", color = "red") +
  annotate("text", x = 26, y = max(december_trends$avg_count), 
           label = "Christmas Day", color = "red", hjust = 0) +
  labs(title = "The 'Festive Dip': Average Cycling Demand in December",
       subtitle = "Aggregated daily counts across 2020-2024",
       x = "Day of December",
       y = "Average Daily Cyclists") +
  theme_minimal()


# 3. Model Fitting
# Note: Use factor(month) in formulas for M1-M3
m0 <- lm(count ~ temp_mean + as.numeric(weekend) + as.numeric(month), 
         data = cycle_daily_df)

##m1_literal has collinear terms so we define the factor versions of month and 
#dow and omit the weekend.

m1 <- lm(count ~ temp_mean + trend + factor(month) + factor(dow), 
         data = cycle_daily_df)

m2 <- lm(count ~ temp_mean + I(temp_mean^2)  + trend + 
           factor(month) + factor(dow), data = cycle_daily_df )

m3 <- lm(log(count+1) ~ temp_mean + I(temp_mean^2) + trend + 
               factor(month) + factor(dow)  + is_covid + is_holiday, data = cycle_daily_df)

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

#m3
# Add residuals and fitted values to the dataframe.
m3_diag <- augment(m3)

# Residuals vs Fitted for M3
p1_m3 <- ggplot(m3_diag, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "M3: Residuals vs Fitted",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()
p1_m3
# Normal Q-Q Plot for M3
p2_m3 <- ggplot(m3_diag, aes(sample = .resid)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line(color = "red") +
  labs(title = "M3: Normal Q-Q Plot",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()
p2_m3




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
models_list <- list(m0 = count ~temp_mean + as.numeric(weekend) +
                      as.numeric(month),
                    m1 = count~ temp_mean + I(temp_mean^2)  + trend + 
                      factor(month) + factor(dow), 
                    m2 = count ~ temp_mean + I(temp_mean^2) + 
                      as.numeric(weekend) + trend + factor(month) + factor(dow), 
                    m3 = log(count+1) ~ temp_mean + I(temp_mean^2) + 
                      trend + factor(month) + factor(dow)  + is_covid  + is_holiday)
 
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
    
    if (mod_name == "m3") { #transforming the log back
      mu_3 <- exp(mu)- 1
      sigma_3 <- mu_3 * res_std_error
      mu <- mu_3
      sigma<- sigma_3
    }

    
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

#fitting full models
m1_full <- lm(models_list$m1, data = cycle_daily_df)
m2_full <- lm(models_list$m2, data = cycle_daily_df)
m3_full <- lm(models_list$m3, data = cycle_daily_df)

mbest = m3_full

months_list <- month.abb
table2_results <- list()

for (m in months_list){
  
  #extract one month to test 
  test_data <- subset(cycle_daily_df, month == m)
  
  # predict on the other month
  pred_obj <- predict(mbest, newdata = test_data, se.fit = TRUE)
  
  # Calculate total predictive SD (sigma)
  # Combining residual variance and uncertainty in the mean
  res_std_error <- summary(fit)$sigma
  
  #transform back
  mu_back <- exp(pred_obj$fit) - 1
  sigma_back <- mu_back * res_std_error
  
  # Run calc_scores
  scores <- calc_scores(y = test_data$count, mu = mu_back, sigma = sigma_back, model_name = "m3")
  
  # Store result with identifiers
  table2_results[[m]] <- data.frame(Month = m, RMSE = scores$RMSE, DS = scores$DS)
}

table2_final <- bind_rows(table2_results) %>%
  mutate(
    RMSE = round(RMSE, 1),
    DS = round(DS, 2)
  )


#table 3 - trend coefficients from M1 - M3

# 
# #getting the trend from each model
tr1 <- coef(m1_full)[["trend"]]
tr2 <- coef(m2_full)[["trend"]]
tr3 <- coef(m3_full)[["trend"]]
# 
#building table 3
table3_final <- data.frame(
  #defining the rows as the 3 models
  Model = c("M1", "M2", "M3"),
  `Daily change (cyclists/day)` = c(tr1, tr2, tr3), #defining cyclists per day
  `Annual change (cyclists/year)` = c(tr1 * 365, tr2 * 365, tr3 * 365), #defining cyclists per year
  check.names = FALSE
) %>%
  mutate(
    `Daily change (cyclists/day)` = round(`Daily change (cyclists/day)`, 3), #rounding to 3
    `Annual change (cyclists/year)` = round(`Annual change (cyclists/year)`, 0) #rounding to 0
  )


# 5.1 

# test and train
n = 2192 
set.seed(1234)

idx <- sample(1:n, size = round(0.7 * n))

# Define the two datasets
train <- cycle_daily_df[idx, ] 
test  <- cycle_daily_df[-idx, ] 

M2_Tmean <-lm(count ~ temp_mean + I(temp_mean^2) + trend + 
                
                factor(month) + factor(dow), data = train )

M2_Tmin <- lm(count ~ temp_mean + I(temp_min^2) + trend + 
                
                factor(month) + factor(dow), data = train )

M2_Tmax <- lm(count ~ temp_mean + I(temp_max^2) + trend + 
                
                factor(month) + factor(dow), data = train )

m2_scores <- function(model, data = test) {
  mu <- predict(model, data = test)
  se <- predict(model,data = test, se.fit = TRUE)$se.fit
  sigma <- sqrt(summary(model)$sigma^2 + se^2)
  calc_scores(y = test$count, mu = mu, sigma = sigma)
}

# table
m2_comparison <- bind_rows(
  "Temp Mean" = m2_scores(M2_Tmean, test),
  "Temp Min"  = m2_scores(M2_Tmin, test),
  "Temp Max"  = m2_scores(M2_Tmax, test),
  .id = "Model")

# estimated marginal effect
# Extract the coefficients
# b1 is the linear slope, b2 is the curvature coefficient
b1 <- coef(M2_Tmax)["temp_mean"]
b2 <- coef(M2_Tmax)["I(temp_max^2)"]

# Calculate the Slope at 5C and 15C
# Formula: Slope = b1 + (2 * b2 * Temperature)
increase_5  <- b1 + (2 * b2 * 5)
increase_15 <- b1 + (2 * b2 * 15)

# 3. Round these for easy reading in your sentence
val_5  <- round(increase_5, 0)
val_15 <- round(increase_15, 0)

# Fit M2 with seasonal interactions
M2_interaction <- lm(count ~ temp_mean + (temp_mean : is_spring) + (temp_mean:is_winter) + 
                       I(temp_max^2) + trend + factor(dow), data = train)

# Extract Coefficients
b_base   <- coef(M2_interaction)["temp_mean"]
b_spring <- coef(M2_interaction)["temp_mean:is_spring"]
b_winter <- coef(M2_interaction)["temp_mean:is_winter"]
b_quad   <- coef(M2_interaction)["I(temp_max^2)"]

# Calculate Marginal Effect at 5 degrees and 15 degress for both seasons
# Formula: (Base + Season_Bonus) + (2 * Curvature * Temp)
effect_spring_5 <- (b_base + b_spring) + (2 * b_quad * 5)
effect_spring_15 <- (b_base + b_spring) + (2 * b_quad * 15)
effect_winter_5 <- (b_base + b_winter) + (2 * b_quad * 5)
effect_winter_15 <- (b_base + b_winter) + (2 * b_quad * 15)

# 6. CV by Month 
# ...
