'install.packages('PerformanceAnalytics')'

library(ggplot2)
library(dplyr)
library(corrplot)


df <- read_excel("STRESS.xlsx")

str(df)

head(df)


dim(df)

sum(is.na(df))

colnames(df)

summary(df)


variables <- c("AGE", "COHES", "ESTEEM", "GRADES", "SATTACH", "STRESS")


# Set up the plot layout to 2 rows and 3 columns
par(mfrow = c(2, 3))

# Loop over the variables and plot histograms
for (var in variables) {
  # Subset the data for the current variable
  data_to_plot <- df[[var]]
  
  # Determine the range for breaks
  min_val <- floor(min(data_to_plot, na.rm = TRUE))
  max_val <- ceiling(max(data_to_plot, na.rm = TRUE))
  
  # Create a histogram with whole numbers on the x-axis
  hist(data_to_plot, col = "steelblue", 
       breaks = seq(from = min_val, to = max_val, by = 1),
       main = paste("Histogram of", var),
       xlab = var,
       xaxt = 'n') # Turn off default x-axis to customize
  
  # Add custom x-axis with whole numbers
  axis(1, at = seq(min_val, max_val, by = 1))
}

# Reset the plotting layout to default
par(mfrow = c(1, 1))


# Boxplots


# Set up the plot layout to 2 rows and 3 columns
par(mfrow = c(2, 3))

# Loop over the variables and plot boxplots
for (var in variables) {
  # Subset the data for the current variable
  data_to_plot <- df[[var]]
  
  # Create a boxplot for each variable
  boxplot(data_to_plot, col = "lightblue",
          main = paste("Boxplot of", var),
          ylab = var)
  
  # Optional: Add a horizontal line at the median
  abline(h = median(data_to_plot, na.rm = TRUE), col = "red", lty = 2)
}

# Reset the plotting layout to default
par(mfrow = c(1, 1))

### Relationships

pairs(df[, variables])
corr_matrix <- cor(df[, sapply(df, is.numeric)], use = "complete.obs")
corrplot(corr_matrix, method = "circle", type = "full", addCoef.col = "black")


#################################################
# Task 1

library(ggplot2)
library(knitr)


# Create the histogram
p <- ggplot(df, aes(x=STRESS)) +
  geom_histogram(binwidth=1, fill='steelblue') + # Ensure binwidth is appropriate for your data
  scale_x_continuous(breaks=seq(floor(min(df$STRESS)), ceiling(max(df$STRESS)), by=1)) +
  labs(title='Histogram of STRESS', x='STRESS', y='Frequency')

# Print the plot
print(p)



# Calculating summary statistics
summary_stats <- summary(df$STRESS)
additional_stats <- c(
  'Standard Deviation' = sd(df$STRESS)
)

# Combine the summary statistics
all_stats <- c(summary_stats, additional_stats)

# Print summary statistics using kable
kable(as.data.frame(t(all_stats)), caption = "Summary Statistics of STRESS")



# Assuming df is your dataframe and STRESS is your variable of interest
# Create the Q-Q plot
qqnorm(df$STRESS, main = "Normal Q-Q Plot for STRESS")
qqline(df$STRESS, col = "steelblue", lwd = 2)

# Alternatively, if you want to create a Q-Q plot using ggplot2:
ggplot(df, aes(sample = STRESS)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Normal Q-Q Plot for STRESS")





#################################################################
# Task 2


# Assuming 'df' is your dataframe containing the variables
# Fitting the OLS regression model
model <- lm(STRESS ~ COHES + ESTEEM + GRADES + SATTACH, data = df)

# Summarizing the model
summary(model)

# Diagnostic plots
# 1. Residuals vs Fitted to check for homoscedasticity and linearity
plot(model, which = 1)

# 2. Normal Q-Q plot to check for the normality of residuals
plot(model, which = 2)

# 3. Scale-Location plot to check for homoscedasticity
plot(model, which = 3)

# 4. Residuals vs Leverage to check for influential observations
plot(model, which = 5)

# Additionally, you can use the 'car' package for more diagnostic plots
# Install if necessary: install.packages('car')
library(car)
# Influence Plot
influencePlot(model)

# Cook's distance plot to identify influential points
cutoff <- 4/(nrow(df)-length(model$coefficients)-2) # Rule of thumb for influential points
plot(model, which = 4, cook.levels = cutoff)


#########################################
# Create a dataframe with actual and predicted values
results <- data.frame(Actual = df$STRESS, Predicted = predicted_values)

# Round the actual and predicted values to whole numbers
results$Actual <- round(results$Actual)
results$Predicted <- round(results$Predicted)

# Calculate the maximum frequency in the histograms
max_freq <- max(hist(results$Actual, plot = FALSE)$counts,
                hist(results$Predicted, plot = FALSE)$counts)

# Set the desired y-axis limit
y_axis_limit <- 400

# Create a histogram of actual values with transparent color
hist_results_actual <- hist(results$Actual, 
                            breaks = seq(min(results$Actual), max(results$Actual) + 1, by = 1),
                            plot = FALSE)

hist(results$Actual, 
     breaks = seq(min(results$Actual), max(results$Actual) + 1, by = 1),
     main = "Actual vs. Predicted STRESS Values",
     xlab = "STRESS Values", ylab = "Frequency", col = rgb(0, 0, 1, alpha = 0.5), 
     xlim = c(0, max(results$Actual) + 1), ylim = c(0, y_axis_limit))

# Add a histogram of predicted values with transparent color
hist_results_predicted <- hist(results$Predicted, 
                               breaks = seq(min(results$Actual), max(results$Actual) + 1, by = 1),
                               plot = FALSE)

hist(results$Predicted, 
     breaks = seq(min(results$Actual), max(results$Actual) + 1, by = 1),
     col = rgb(1, 0, 0, alpha = 0.5), add = TRUE)

# Add custom x-axis labels at 0, 2, 4, 6, and 8
axis(1, at = c(0, 2, 4, 6, 8), labels = c(0, 2, 4, 6, 8))

# Add data labels to the histogram bars for both actual and predicted values
text(hist_results_actual$mids, hist_results_actual$counts, labels = hist_results_actual$counts, 
     col = "blue", pos = 3, offset = 1)
text(hist_results_predicted$mids, hist_results_predicted$counts, labels = hist_results_predicted$counts, 
     col = "red", pos = 3, offset = 1)

# Add a legend
legend("topright", legend = c("Actual", "Predicted"), fill = c(rgb(0, 0, 1, alpha = 0.5), rgb(1, 0, 0, alpha = 0.5)))




#################################################################
# Task 3


# Add 1 to STRESS values before taking the natural logarithm
df$ln_STRESS <- log(df$STRESS + 1)

# Check for missing or infinite values in ln_STRESS
missing_values <- sum(is.na(df$ln_STRESS))
infinite_values <- sum(!is.finite(df$ln_STRESS))

if (missing_values > 0 || infinite_values > 0) {
  # Handle missing or infinite values
  # For example, you can remove rows with missing or infinite values
  df <- df[is.finite(df$ln_STRESS), ]
}

# Fit the OLS regression model for ln(STRESS) after handling missing/infinite values
model <- lm(ln_STRESS ~ COHES + ESTEEM + GRADES + SATTACH, data = df)

# Summarize the model
summary(model)

# Predict values and subtract 1 to get back to the original scale
predicted_values <- exp(predict(model)) - 1

# Diagnostic plots
# 1. Residuals vs Fitted to check for homoscedasticity and linearity
plot(model, which = 1)

# 2. Normal Q-Q plot to check for the normality of residuals
plot(model, which = 2)

# 3. Scale-Location plot to check for homoscedasticity
plot(model, which = 3)

# 4. Residuals vs Leverage to check for influential observations
plot(model, which = 5)

# Additionally, you can use the 'car' package for more diagnostic plots
# Install if necessary: install.packages('car')
library(car)

# Influence Plot
influencePlot(model)

# Cook's distance plot to identify influential points
cutoff <- 4/(nrow(df)-length(model$coefficients)-2) # Rule of thumb for influential points
plot(model, which = 4, cook.levels = cutoff)

#########################################
# Create a dataframe with actual and predicted values
results <- data.frame(Actual = df$STRESS, Predicted = predicted_values)

# Round the actual and predicted values to whole numbers
results$Actual <- round(results$Actual)
results$Predicted <- round(results$Predicted)

# Calculate the maximum frequency in the histograms
max_freq <- max(hist(results$Actual, plot = FALSE)$counts,
                hist(results$Predicted, plot = FALSE)$counts)

# Set the desired y-axis limit (higher than the maximum frequency)
y_axis_limit <- max(max_freq, max(results$Predicted)) + 10  # Add some margin for the y-axis limit

# Create a histogram of actual values with transparent color
hist_results_actual <- hist(results$Actual, 
                            breaks = seq(min(results$Actual), max(results$Actual) + 1, by = 1),
                            plot = FALSE)

hist(results$Actual, 
     breaks = seq(min(results$Actual), max(results$Actual) + 1, by = 1),
     main = "Actual vs. Predicted STRESS Values",
     xlab = "STRESS (prediction after taking ln)", ylab = "Frequency", col = rgb(0, 0, 1, alpha = 0.5), 
     xlim = c(0, max(results$Actual) + 1), ylim = c(0, y_axis_limit), xaxt = "n")  # Suppress x-axis labels

# Add a histogram of predicted values with transparent color
hist_results_predicted <- hist(results$Predicted, 
                               breaks = seq(min(results$Actual), max(results$Actual) + 1, by = 1),
                               plot = FALSE)

hist(results$Predicted, 
     breaks = seq(min(results$Actual), max(results$Actual) + 1, by = 1),
     col = rgb(1, 0, 0, alpha = 0.5), add = TRUE)

# Add custom x-axis labels at 0, 2, 4, 6, and 8
axis(1, at = c(0, 2, 4, 6, 8), labels = c(0, 2, 4, 6, 8))

# Add data labels to the histogram bars for both actual and predicted values
text(hist_results_actual$mids, hist_results_actual$counts, labels = hist_results_actual$counts, 
     col = "blue", pos = 3, offset = 1)
text(hist_results_predicted$mids, hist_results_predicted$counts, labels = hist_results_predicted$counts, 
     col = "red", pos = 3, offset = 1)

# Add a legend
legend("topright", legend = c("Actual", "Predicted"), fill = c(rgb(0, 0, 1, alpha = 0.5), rgb(1, 0, 0, alpha = 0.5)))


###############################################################
# Task 4

# Load the necessary library
library(stats)

# Assuming the dataframe is named 'df' and has the variables: STRESS, COHES, ESTEEM, GRADES, SATTACH

# Fit a Poisson regression model
model <- glm(STRESS ~ COHES + ESTEEM + GRADES + SATTACH, family = poisson, data = df)

# Summary of the model
summary(model)


# Load the necessary library
library(MASS)

# Assuming the dataframe is named 'df' and contains the variables: STRESS, COHES, ESTEEM, GRADES, SATTACH

# Fit an over-dispersed Poisson regression model (Negative Binomial model)
model_nb <- glm.nb(STRESS ~ COHES + ESTEEM + GRADES + SATTACH, data = df)

# Summary of the model
summary(model_nb)

###############################################################
# Task 5




# Calculate mean and standard deviation of COHES
mean_cohes <- mean(df$COHES, na.rm = TRUE)
sd_cohes <- sd(df$COHES, na.rm = TRUE)

# Define the Cohesion Groups
low_cohes <- mean_cohes - sd_cohes
high_cohes <- mean_cohes + sd_cohes

# Extract coefficients from the model
intercept <- coef(model)["(Intercept)"]
beta_cohes <- coef(model)["COHES"]

# Calculate predicted counts for low, middle, and high groups
predicted_low <- exp(intercept + beta_cohes * low_cohes)
predicted_middle <- exp(intercept + beta_cohes * mean_cohes)
predicted_high <- exp(intercept + beta_cohes * high_cohes)

# Calculate expected percent difference between high and low groups
percent_difference <- ((predicted_high - predicted_low) / predicted_low) * 100

# Output the results
list(
  predicted_low = predicted_low,
  predicted_middle = predicted_middle,
  predicted_high = predicted_high,
  percent_difference = percent_difference
)

# Print the predicted counts and percent difference, rounded to two decimal places
print(paste("Predicted count of STRESS for low COHES group:", round(predicted_low, 2)))
print(paste("Predicted count of STRESS for middle COHES group:", round(predicted_middle, 2)))
print(paste("Predicted count of STRESS for high COHES group:", round(predicted_high, 2)))
print(paste("Expected percent difference between high and low COHES groups:", round(percent_difference, 2)))



#####################################################
# Task 6



# Compute AIC and BIC for the Poisson model
aic_poisson <- AIC(model)
bic_poisson <- BIC(model)

# Compute AIC and BIC for the Negative Binomial model
aic_nb <- AIC(model_nb)
bic_nb <- BIC(model_nb)

# Output the results
list(
  aic_poisson = aic_poisson,
  bic_poisson = bic_poisson,
  aic_nb = aic_nb,
  bic_nb = bic_nb
)
# Print AIC and BIC for the Poisson model, rounded to three decimal places
print(paste("AIC for Poisson model:", round(aic_poisson, 3)))
print(paste("BIC for Poisson model:", round(bic_poisson, 3)))

# Print AIC and BIC for the Negative Binomial model, rounded to three decimal places
print(paste("AIC for Negative Binomial model:", round(aic_nb, 3)))
print(paste("BIC for Negative Binomial model:", round(bic_nb, 3)))


############################################################
# Task 8

# Create the new indicator variable Y_IND
df$Y_IND <- ifelse(df$STRESS == 0, 0, 1)

# Fit a logistic regression model
logistic_model <- glm(Y_IND ~ COHES + ESTEEM + GRADES + SATTACH, family = binomial, data = df)

# Summary of the logistic regression model
summary(logistic_model)

#############################################################
# Task 9

# Fit the logistic regression model to estimate the probability of STRESS being greater than zero
logistic_model <- glm(Y_IND ~ COHES + ESTEEM + GRADES + SATTACH, family = binomial(link = "logit"), data = df)

# Fit the Poisson regression model to estimate the expected count of STRESS, given that STRESS is greater than zero
# Create a subset of df where STRESS is greater than zero for the Poisson part
df_positive <- df[df$STRESS > 0, ]

# Fit the Poisson regression model on the subset
poisson_model <- glm(STRESS ~ COHES + ESTEEM + GRADES + SATTACH, family = poisson(link = "log"), data = df_positive)

# Output the summary of both models
summary(logistic_model)
summary(poisson_model)

# Use the logistic regression model to predict the probability of stress being present
logistic_predictions <- predict(logistic_model, type = "response")

# Use the Poisson regression model to predict the counts for the subset with stress present
subset_with_stress <- df[df$Y_IND == 1, ]  # Subset of data with stress present
poisson_predictions <- predict(poisson_model, newdata = subset_with_stress, type = "response")

# Initialize a vector to store the combined predictions
combined_predictions <- rep(0, nrow(df))

# For individuals where the logistic model predicts stress presence (Y_IND = 1),
# replace the zero with the predicted count from the Poisson model
combined_predictions[df$Y_IND == 1] <- poisson_predictions

# Add the combined predictions to the original data frame 'df'
df$Combined_Predictions <- round(combined_predictions)

# If you need to print the summaries
print(logistic_summary)
print(poisson_summary)

# Compute residuals between "STRESS" and "Combined_Predictions" columns
df$Residuals <- df$STRESS - df$Combined_Predictions

# Print the top 20 rows of STRESS, Combined_Predictions, and Residuals
top_20_rows <- head(df[c("STRESS", "Combined_Predictions", "Residuals")], 20)
print(top_20_rows)


# Calculate MAE (Mean Absolute Error)
mae <- mean(abs(df$STRESS - df$Combined_Predictions))
cat("Mean Absolute Error (MAE):", mae, "\n")

# Calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((df$STRESS - df$Combined_Predictions)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")






# Calculate the summary statistics for df$Residuals
residuals_mean <- mean(df$Residuals)
residuals_median <- median(df$Residuals)
residuals_sd <- sd(df$Residuals)
residuals_min <- min(df$Residuals)
residuals_max <- max(df$Residuals)

# Create a data frame to store the summary statistics
residuals_summary_df <- data.frame(
  Statistic = c("Mean", "Median", "Standard Deviation", "Min", "Max"),
  Value = c(residuals_mean, residuals_median, residuals_sd, residuals_min, residuals_max)
)

# Print the table using kable
library(knitr)
kable(residuals_summary_df, format = "html")

# Save the HTML table to a file
writeLines(kable(residuals_summary_df, format = "html"), "residuals_summary.html")

# Open the HTML file in a web browser
browseURL("residuals_summary.html")




# Create a histogram for the residuals
hist(df$Residuals, main = "Histogram of Residuals", xlab = "Residuals", ylab = "Frequency", col = "steelblue")
########################################################


###########################
# Plot histogram
# Create a dataframe with actual and predicted values
results <- data.frame(Actual = df$STRESS, Predicted = df$Combined_Predictions)

# Round the actual and predicted values to whole numbers
results$Actual <- round(results$Actual)
results$Predicted <- round(results$Predicted)

# Calculate the maximum frequency in the histograms
max_freq <- max(hist(results$Actual, plot = FALSE)$counts,
                hist(results$Predicted, plot = FALSE)$counts)

# Set the desired y-axis limit (higher than the maximum frequency)
y_axis_limit <- max(max_freq, max(results$Predicted)) + 10  # Add some margin for the y-axis limit

# Create a histogram of actual values with transparent color
hist_results_actual <- hist(results$Actual, 
                            breaks = seq(min(results$Actual), max(results$Actual) + 1, by = 1),
                            plot = FALSE)

hist(results$Actual, 
     breaks = seq(min(results$Actual), max(results$Actual) + 1, by = 1),
     main = "Actual vs. Predicted STRESS Values",
     xlab = "STRESS", ylab = "Frequency", col = rgb(0, 0, 1, alpha = 0.5), 
     xlim = c(0, max(results$Actual) + 1), ylim = c(0, y_axis_limit), xaxt = "n")  # Suppress x-axis labels

# Add a histogram of predicted values with transparent color
hist_results_predicted <- hist(results$Predicted, 
                               breaks = seq(min(results$Actual), max(results$Actual) + 1, by = 1),
                               plot = FALSE)

hist(results$Predicted, 
     breaks = seq(min(results$Actual), max(results$Actual) + 1, by = 1),
     col = rgb(1, 0, 0, alpha = 0.5), add = TRUE)

# Add custom x-axis labels at 0, 2, 4, 6, and 8
axis(1, at = c(0, 2, 4, 6, 8), labels = c(0, 2, 4, 6, 8))

# Add data labels to the histogram bars for both actual and predicted values
text(hist_results_actual$mids, hist_results_actual$counts, labels = hist_results_actual$counts, 
     col = "blue", pos = 3, offset = 1)
text(hist_results_predicted$mids, hist_results_predicted$counts, labels = hist_results_predicted$counts, 
     col = "red", pos = 3, offset = 1)

# Add a legend
legend("topright", legend = c("Actual", "Predicted"), fill = c(rgb(0, 0, 1, alpha = 0.5), rgb(1, 0, 0, alpha = 0.5)))

##############################


# Assuming df$Residuals is the column containing residuals
# df$STRESS - df$Combined_Predictions computes the residuals

# Create an index or row number as the independent variable
index <- seq_along(df$Residuals)

# Create a residual plot
plot(index, df$Residuals, main = "Residual Plot", xlab = "Index", ylab = "Residuals", pch = 19)
abline(h = 0, col = "red")  # Add a horizontal line at y = 0 (residuals should be centered around this line)




####################################################################
# Task 10

'install.packages("pscl", dependencies = TRUE)'
library(pscl)


# Fit the Zero-Inflated Poisson (ZIP) model to predict 'STRESS'
zip_model <- zeroinfl(STRESS ~ AGE + COHES + ESTEEM + GRADES + SATTACH, data = df, dist = "poisson")

# Extract count model coefficients
count_coef <- coef(zip_model, "count")

# Extract zero-inflation model coefficients
zero_infl_coef <- coef(zip_model, "zero")

# Print the coefficients
print(count_coef)
print(zero_infl_coef)

# Display the summary of the model
summary(zip_model)


# AIC and BIC for the original model
aic_value_original <- AIC(zip_model)
bic_value_original <- BIC(zip_model)

# Print AIC and BIC values for the original model
print(paste("AIC for original ZIP model:", aic_value_original))
print(paste("BIC for original ZIP model:", bic_value_original))

# Likelihood Ratio Test for the original model
poisson_model_original <- glm(STRESS ~ AGE + COHES + ESTEEM + GRADES + SATTACH, data = df, family = "poisson")
lr_test_original <- lrtest(zip_model, poisson_model_original)

# Print the results of the Likelihood Ratio Test for the original model
print(lr_test_original)

# AIC and BIC for the simplified model
aic_value_sig <- AIC(zip_model_sig)
bic_value_sig <- BIC(zip_model_sig)

# Print AIC and BIC values for the simplified model
print(paste("AIC for simplified ZIP model:", aic_value_sig))
print(paste("BIC for simplified ZIP model:", bic_value_sig))

# Likelihood Ratio Test for the simplified model
poisson_model_sig <- glm(STRESS ~ COHES + ESTEEM, data = df, family = "poisson")
lr_test_sig <- lrtest(zip_model_sig, poisson_model_sig)

# Print the results of the Likelihood Ratio Test for the simplified model
print(lr_test_sig)

# Get the Pearson residuals from the ZIP model
residuals_zip <- residuals(zip_model, type = "pearson")
# Plot the residuals against the fitted values or an index
plot(residuals_zip, main = "Residuals of ZIP model for STRESS", xlab = "Index", ylab = "Pearson Residuals", pch = 20)
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0 for reference

#####################################



# Fit the Zero-Inflated Poisson (ZIP) model to predict 'STRESS' using only the significant variables for the count part
# and only intercept for the zero-inflation part as no other variables were significant there
zip_model_sig <- zeroinfl(STRESS ~ COHES + ESTEEM | 1, data = df, dist = "poisson")

# Display the summary of the model
summary(zip_model_sig)

# Print the coefficients for the significant model
count_coef_sig <- coef(zip_model_sig, "count")
zero_infl_coef_sig <- coef(zip_model_sig, "zero")

print(count_coef_sig)
print(zero_infl_coef_sig)

###############################################

'install.packages("lmtest", dependencies = TRUE)'
library(lmtest)

# AIC and BIC for the model
aic_value <- AIC(zip_model_sig)
bic_value <- BIC(zip_model_sig)

# Print AIC and BIC values
print(paste("AIC:", aic_value))
print(paste("BIC:", bic_value))

# Likelihood Ratio Test compared to a simpler model
# For example, comparing to a standard Poisson model
poisson_model <- glm(STRESS ~ COHES + ESTEEM, data = df, family = "poisson")
lr_test <- lrtest(zip_model_sig, poisson_model)

# Print the results of the Likelihood Ratio Test
print(lr_test)



# Get the Pearson residuals from the ZIP model
residuals_zip <- residuals(zip_model_sig, type = "pearson")
# Plot the residuals against the fitted values or an index
plot(residuals_zip, main = "Residuals of ZIP model for STRESS", xlab = "Index", ylab = "Pearson Residuals", pch = 20)
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0 for reference



