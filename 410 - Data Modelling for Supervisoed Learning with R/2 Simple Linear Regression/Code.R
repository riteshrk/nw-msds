# Section 0

#install.packages("ggplot2")
library(readxl)
library(ggplot2)
library(knitr)
library(dplyr)
library(tidyr)

data <- read_excel("USStates.xlsx")

str(data)

colnames(data)

# Loop through each column in the dataset
for (col_name in names(data)) {
  cat("Summary for", col_name, ":\n")
  print(summary(data[[col_name]]))
  cat("\n")  # Add an extra newline for separation
}



# Section2
# Section3


# 1. Calculate Summary Statistics for the specified columns
# Necessary libraries
library(knitr)
library(dplyr)
library(tidyr)

# Excluding character variables
numeric_data <- data %>% select_if(is.numeric)

# Calculate summary statistics
summary_stats <- numeric_data %>%
  summarise_all(list(
    n = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  )) %>%
  gather(variable, value, everything()) %>%
  separate(variable, into = c("Variable", "Statistic")) %>%
  pivot_wider(names_from = Statistic, values_from = value)

# Print the summary using kable
kable(summary_stats)



par(mar=c(4, 4, 2, 2))

# Scatterplots of non-demographic explanatory variables vs. HouseholdIncome
non_demographic_vars <- c("HighSchool", "College", "Smokers", "PhysicalActivity", "Obese", "NonWhite", "HeavyDrinkers", "TwoParents", "Insured")

# Setting up the plotting layout
par(mfrow=c(3,3))  # 3x3 grid of plots

# Plotting
for(var in non_demographic_vars) {
  plot(data[[var]], data$HouseholdIncome, main=paste(var, "vs. HouseholdIncome"), xlab=var, ylab="HouseholdIncome")
}

# Resetting the plotting parameter to defaults
par(mfrow=c(1,1))


# Section 4

# List of non-demographic variables
non_demographic_vars <- c("HighSchool", "College", "Smokers", "PhysicalActivity", "Obese", "NonWhite", "HeavyDrinkers", "TwoParents", "Insured")

# Calculate correlations with HouseholdIncome
correlations <- sapply(non_demographic_vars, function(var) {
  cor(data[[var]], data$HouseholdIncome, method="pearson", use="complete.obs")
})

# Present correlations in a table
correlation_table <- data.frame(
  Variable = non_demographic_vars,
  Correlation_with_HouseholdIncome = correlations
)
print(correlation_table)

# Section 5

# Fitting the simple linear regression model
model1 <- lm(HouseholdIncome ~ College, data=data)

# Display the summary of the model
summary(model1)


# Section 6
anova(model1)

# Section 7

# Predicted values using Model 1
Y_hat <- predict(model1, data)
Y_hat

# Residuals
residuals <- data$HouseholdIncome - Y_hat
residuals

# Sum of Squared Errors
sse <- sum(residuals^2)
sse

# Sum of Squared Totals
Y_bar <- mean(data$HouseholdIncome)   # Mean of Y
Y_bar
sst <- sum((data$HouseholdIncome - Y_bar)^2)
sst

# Sum of Squares due to Regression
ssr <- sum((Y_hat - Y_bar)^2)
ssr

# Compute Desired Statistic
statistic <- ssr / sst

# Predicted values and Residuals
Y_hat <- predict(model1, data)
residuals <- data$HouseholdIncome - Y_hat

# Sum of Squared Residuals (SSE)
sse <- sum(residuals^2)

# Sum of Squares Total (SST)
Y_bar <- mean(data$HouseholdIncome)
sst <- sum((data$HouseholdIncome - Y_bar)^2)

# Sum of Squares due to Regression (SSR)
ssr <- sum((Y_hat - Y_bar)^2)

# Calculate the statistic
statistic <- ssr / sst

# Print out the results
cat("SSE:", sse, "\n")
cat("SST:", sst, "\n")
cat("SSR:", ssr, "\n")
cat("Statistic:", statistic, "\n")

# Section 8

# Standardize the Residuals
std_residuals <- residuals / sd(residuals)

# Plot the Standardized Residuals using a Histogram:
hist(std_residuals, main="Histogram of Standardized Residuals", xlab="Standardized Residuals", col="skyblue", border="black")

# Scatter plot of Standardized Residuals vs. Predicted Values:
plot(Y_hat, std_residuals, main="Scatterplot of Standardized Residuals vs. Predicted Values", xlab="Predicted Values", ylab="Standardized Residuals", pch=19, col="blue")
abline(h=0, col="red") # horizontal line at y=0

# Section 9

# Linear Regression Model with "Smokers" is the variable for Model 2
model2 <- lm(HouseholdIncome ~ Smokers, data=data)
summary(model2)

# Section 10

# Model 3: Predicting "HouseholdIncome" using the "Insured" variable.

model3 <- lm(HouseholdIncome ~ Insured, data=data)
summary(model3)






