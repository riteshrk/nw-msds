'install.packages("readxl")
install.packages("waterfalls")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("e1071")
install.packages("knitr")'

library(e1071)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(waterfalls)
library(caret)
library(glmnet)
library(knitr)
library(gridExtra)

### Assignment 1

## Section 1

# Read the datafile
mydata <- read_excel("ames_housing_data.xlsx")

# Print the structure of the file
str(mydata) # The datafile has 82 columns and 2930 rows

# Print the fist 6 rows of the dataset
head(mydata)

# Print the names of the columns
names(mydata)

# Get the names of the columns with missing values
columns_with_missing <- names(mydata)[colSums(is.na(mydata)) > 0]
print(columns_with_missing)

# Get the number of values missing in each of these columns
missing_counts <- colSums(is.na(mydata))
missing_counts <- missing_counts[missing_counts > 0]
print(missing_counts)

# Identify column types
continuous_cols <- names(mydata)[sapply(mydata, function(col) is.numeric(col) && length(unique(col)) > 10)]
discrete_cols <- names(mydata)[sapply(mydata, function(col) is.numeric(col) && length(unique(col)) <= 10)]
nominal_cols <- names(mydata)[sapply(mydata, function(col) is.factor(col) && !is.ordered(col))]
ordinal_cols <- names(mydata)[sapply(mydata, is.ordered)]
binary_cols <- names(mydata)[sapply(mydata, function(col) is.numeric(col) && all(col %in% c(0, 1)))]


print(paste("Continuous columns:", paste(continuous_cols, collapse = ", ")))
print(paste("Discrete columns:", paste(discrete_cols, collapse = ", ")))
print(paste("Nominal columns:", paste(nominal_cols, collapse = ", ")))
print(paste("Ordinal columns:", paste(ordinal_cols, collapse = ", ")))
print(paste("Binary columns:", paste(binary_cols, collapse = ", ")))




## Section 2

# Boxplot for SalePrice

ggplot(mydata, aes(y = SalePrice)) + 
  geom_boxplot(fill = "lightblue") +
  labs(title="Boxplot of SalePrice", y="SalePrice") +
  scale_y_continuous(labels = comma)

# graphics.off()
par(mar = c(5.1, 4.1, 4.1, 2.1))

# Compute Q1 and Q3
Q1 <- quantile(mydata$SalePrice, 0.25)
Q3 <- quantile(mydata$SalePrice, 0.75)

# Calculate IQR
IQR_val <- Q3 - Q1

# Define bounds
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

# Identify outliers below and above bounds
outliers_below <- mydata$SalePrice[which(mydata$SalePrice < lower_bound)]
outliers_above <- mydata$SalePrice[which(mydata$SalePrice > upper_bound)]

# Print results
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")
cat("Number of outliers below lower bound:", length(outliers_below), "\n")
cat("Number of outliers above upper bound:", length(outliers_above), "\n")


# Generate the boxplot using ggplot2
ggplot(mydata, aes(y = GrLivArea)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Boxplot of GrLivArea", y = "GrLivArea (Above Grade Living Area SqFt)") +
  theme_minimal()
par(mar = c(5.1, 4.1, 4.1, 2.1))

# Compute Q1 and Q3 for GrLivArea
Q1_GrLivArea <- quantile(mydata$GrLivArea, 0.25)
Q3_GrLivArea <- quantile(mydata$GrLivArea, 0.75)

# Calculate IQR for GrLivArea
IQR_GrLivArea <- Q3_GrLivArea - Q1_GrLivArea

# Define bounds for GrLivArea
lower_bound_GrLivArea <- Q1_GrLivArea - 1.5 * IQR_GrLivArea
upper_bound_GrLivArea <- Q3_GrLivArea + 1.5 * IQR_GrLivArea

# Identify outliers below and above bounds for GrLivArea
outliers_below_GrLivArea <- mydata$GrLivArea[which(mydata$GrLivArea < lower_bound_GrLivArea)]
outliers_above_GrLivArea <- mydata$GrLivArea[which(mydata$GrLivArea > upper_bound_GrLivArea)]

# Print results for GrLivArea
cat("Lower Bound for GrLivArea:", lower_bound_GrLivArea, "\n")
cat("Upper Bound for GrLivArea:", upper_bound_GrLivArea, "\n")
cat("Number of outliers below lower bound for GrLivArea:", length(outliers_below_GrLivArea), "\n")
cat("Number of outliers above upper bound for GrLivArea:", length(outliers_above_GrLivArea), "\n")



# Generate the scatter plot using ggplot2
ggplot(mydata, aes(x = BldgType, y = SalePrice)) +
  geom_point(aes(color = BldgType), size = 3) +
  labs(title = "Scatter plot of SalePrice by BldgType",
       x = "Building Type",
       y = "Sale Price") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(legend.position = "none")


# Scatter plot of SalePrice by SaleCondition
ggplot(mydata, aes(x = SaleCondition, y = SalePrice)) +
  geom_point(aes(color = SaleCondition), size = 3) +
  labs(title = "Scatter plot of SalePrice by SaleCondition",
       x = "Sale Condition",
       y = "Sale Price") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position = "none")


# Scatter plot of SalePrice by Functional
ggplot(mydata, aes(x = Functional, y = SalePrice)) +
  geom_point(aes(color = Functional), size = 3) +
  labs(title = "Scatter plot of SalePrice by Functional",
       x = "Home Functionality",
       y = "Sale Price") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position = "none")


# Scatter plot of SalePrice by Zoning
ggplot(mydata, aes(x = Zoning, y = SalePrice)) +
  geom_point(aes(color = Zoning), size = 3) +
  labs(title = "Scatter plot of SalePrice by Zoning",
       x = "Zoning Classification",
       y = "Sale Price") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position = "none")
# graphics.off()
par(mar = c(5.1, 4.1, 4.1, 2.1))
# Plot the Waterfall
start_count <- nrow(mydata)
# Open a graphics device
plot.new()


# Apply filters 
condition1 <- mydata[mydata$Zoning %in% c("RH", "RL", "RM"), ]
condition2 <- condition1[condition1$SaleCondition == "Normal", ]
condition3 <- condition2[condition2$BldgType == "1Fam", ]
condition4 <- condition3[condition3$Functional == "Typ", ]
condition5 <- condition4[condition4$SalePrice <= 339500, ]
condition6 <- condition5[condition5$GrLivArea <= 2667, ]

# Calculate the number of rows dropped at each step
drops <- c(
  start_count,
  nrow(condition1) - nrow(mydata),
  nrow(condition2) - nrow(condition1),
  nrow(condition3) - nrow(condition2),
  nrow(condition4) - nrow(condition3),
  nrow(condition5) - nrow(condition4),
  nrow(condition6) - nrow(condition5)
)

# Create labels for each condition
labels <- c(
  "Initial Count", "After Zoning Filter", "After Sale Condition Filter",
  "After Building Type Filter", "After Functionality Filter",
  "After Price Constraint", "After Living Area Constraint"
)

# Plot the waterfall chart
waterfall(values = drops, labels = labels)

# Add axis labels
axis(side = 1, at = 1:length(labels), labels = labels)


# Create a dataframe with the filtered data
# Apply filters
typical_homes <- subset(mydata, 
                        Zoning %in% c("RH", "RL", "RM") & 
                          SaleCondition == "Normal" & 
                          BldgType == "1Fam" & 
                          Functional == "Typ" & 
                          SalePrice <= 339500 & 
                          GrLivArea <= 2667)

# Display the size of the filtered dataframe
size <- dim(typical_homes)
print(paste("The filtered dataframe has", size[1], "rows and", size[2], "columns."))



## Section 3

# Get the names of the columns with missing values
columns_with_missing <- names(typical_homes)[colSums(is.na(typical_homes)) > 0]
print(columns_with_missing)

# Get the number of values missing in each of these columns
missing_counts <- colSums(is.na(typical_homes))
missing_counts <- missing_counts[missing_counts > 0]
print(missing_counts)



# Counting duplicate rows in typical_homes dataframe
duplicate_count <- sum(duplicated(typical_homes))
print(paste("Number of duplicate rows:", duplicate_count))

# Removing duplicate rows
typical_homes_unique <- unique(typical_homes)

# Checking YearBuilt and YearRemodel consistency
typical_homes <- typical_homes[!(typical_homes$YearBuilt > typical_homes$YearRemodel),]

# Filtering rows with negative SalePrice
negative_saleprice_rows <- typical_homes[typical_homes$SalePrice < 0, ]
# Viewing the rows with negative SalePrice values
print(negative_saleprice_rows)


# Dropping the columns
typical_homes$LotFrontage <- NULL
typical_homes$GarageYrBlt <- NULL

# Dropping rows where MasVnrType has missing values
typical_homes <- typical_homes[!is.na(typical_homes$MasVnrType), ]
# Dropping rows where BsmtExposure has missing values
typical_homes <- typical_homes[!is.na(typical_homes$BsmtExposure), ]
# Dropping rows where BsmtFinType2 has missing values
typical_homes <- typical_homes[!is.na(typical_homes$BsmtFinType2), ]
# Dropping rows where Electrical has missing values
typical_homes <- typical_homes[!is.na(typical_homes$Electrical), ]




# Get the number of values missing in each of these columns
missing_counts <- colSums(is.na(typical_homes))
missing_counts <- missing_counts[missing_counts > 0]
print(missing_counts)


# Display the size of the updated dataframe
size <- dim(typical_homes)
print(paste("The filtered dataframe has", size[1], "rows and", size[2], "columns."))



# Histogram for SalePrice
hist(typical_homes$SalePrice, main="Histogram for SalePrice", xlab="SalePrice")

# Scatter plot for SalePrice vs. GrLivArea
plot(typical_homes$GrLivArea, typical_homes$SalePrice, main="Scatterplot", xlab="Living Area", ylab="SalePrice")
#graphics.off()
par(mar = c(5.1, 4.1, 4.1, 2.1))






# Identify continuous variables based on data type
continuous_vars <- names(typical_homes)[sapply(typical_homes, is.numeric)]

# Remove 'SalePrice' from the list to avoid correlation of 1
continuous_vars <- setdiff(continuous_vars, "SalePrice")

# Compute correlations of continuous variables with SalePrice
correlations <- sapply(continuous_vars, function(var) {
  cor(typical_homes[[var]], typical_homes$SalePrice, use="complete.obs")
})

# Remove NA values if there are any
correlations <- correlations[!is.na(correlations)]

# Sort the correlations in descending order
sorted_correlations <- sort(correlations, decreasing=TRUE)

# Create a data frame for kable
correlation_df <- data.frame(
  Variable = names(sorted_correlations),
  Correlation = sorted_correlations
)

dim(correlation_df)

# Use kable to print the correlations
kable(correlation_df, row.names = FALSE)

# Select variables with correltion > 0.5

selected_vars <- subset(correlation_df, Correlation > 0.5)

kable(selected_vars, row.names = FALSE)

# Creating a dataframe with variables whose correlation with SalePrice > 0.5

selected_columns <- c(selected_vars$Variable, "SalePrice")
new_df <- typical_homes[, selected_columns]

dim(new_df)

# Creating a dataframe with variables whose correlation with SalePrice > 0.5
selected_columns <- c(selected_vars$Variable, "SalePrice")
new_df <- typical_homes[, selected_columns]

dim(new_df)



# Variables of interest based on the given correlation table
vars_of_interest <- c("OverallQual", "GrLivArea", "GarageCars", "FullBath", "GarageArea", 
                      "YearBuilt", "TotRmsAbvGrd", "YearRemodel")

# Initialize a vector to store counts of outliers
outliers_count <- numeric(length(vars_of_interest))
names(outliers_count) <- vars_of_interest

# Loop through each variable to count outliers
for (var in vars_of_interest) {
  Q1 <- quantile(new_df[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(new_df[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  # Define boundaries for outliers
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Count outliers
  outliers_count[var] <- sum(new_df[[var]] < lower_bound | new_df[[var]] > upper_bound, na.rm = TRUE)
}

# Filter out variables with zero outliers
vars_with_outliers <- outliers_count[outliers_count > 0]

# Print the variables that have outliers
cat("Variables with outliers:\n")
print(names(vars_with_outliers))
cat("\n")

# Print the number of outliers for each variable
cat("Number of outliers in each variable:\n")
for (var in names(vars_with_outliers)) {
  cat(var, ":", vars_with_outliers[var], "\n")
}

# Drop rows with outliers in specified variables from the dataframe `new_df`
vars_to_check_for_outliers <- c("OverallQual", "GrLivArea", "GarageCars", "GarageArea", "YearBuilt", "TotRmsAbvGrd")

for (var in vars_to_check_for_outliers) {
  Q1 <- quantile(new_df[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(new_df[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  # Define boundaries for outliers
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Drop rows with outliers for the current variable
  new_df <- new_df[!(new_df[[var]] < lower_bound | new_df[[var]] > upper_bound), ]
}

# Drop the columns "FirstFlrSF" and "TotalBsmtSF"
new_df <- new_df[, !(names(new_df) %in% c("FirstFlrSF", "TotalBsmtSF"))]

# Check the resulting dataframe dimensions to confirm the columns have been dropped
dim(new_df)


# Print the columns 
column_names <- names(new_df)
print(column_names)


# Calculate the required statistics for each column
summary_stats <- data.frame(
  Variable = names(new_df),
  Mean = sapply(new_df, mean, na.rm = TRUE),
  Median = sapply(new_df, median, na.rm = TRUE),
  Std_Dev = sapply(new_df, sd, na.rm = TRUE),
  Max = sapply(new_df, max, na.rm = TRUE),
  Min = sapply(new_df, min, na.rm = TRUE)
)

# Print the summary table using kable
kable(summary_stats, row.names = FALSE)


variables <- setdiff(names(new_df), "SalePrice")

# Function to produce a combined scatterplot and histogram for a given variable
plot_var <- function(var) {
  # Scatter plot of variable vs. SalePrice
  scatter <- ggplot(new_df, aes_string(x = var, y = "SalePrice")) + 
    geom_point(aes(color = SalePrice), alpha = 0.6) +
    labs(title = paste("Scatter plot of", var, "vs SalePrice"), x = var, y = "SalePrice") +
    theme_minimal() +
    scale_color_gradient(low = "blue", high = "red")
  
  # Histogram of the variable. We'll use the default bin setting for simplicity, but you can adjust it.
  histogram <- ggplot(new_df, aes_string(var)) + 
    geom_histogram(aes(y = after_stat(density)), fill = "blue", color = "black", alpha = 0.7) +
    geom_density(color = "red") +
    labs(title = paste("Histogram of", var), x = var, y = "Density") +
    theme_minimal()
  
  # Combine the two plots
  combined <- grid.arrange(scatter, histogram, ncol = 2)
  
  return(combined)
}

# Loop through the variables and plot
for (var in variables) {
  plot_var(var)
}


### Question 1

# Fit the model
Model_1 <- lm(SalePrice ~ GrLivArea, data = new_df)

# Display the summary
summary(Model_1)

# Coefficient table
coef_table <- summary(Model_1)$coefficients
print(coef_table)

# ANOVA table
anova_table <- anova(Model_1)
print(anova_table)



# Scatterplot with regression line
ggplot(new_df, aes(x = GrLivArea, y = SalePrice)) + 
  geom_point(aes(color = GrLivArea), alpha = 0.6) + # scatterplot
  geom_smooth(method = "lm", se = FALSE, color = "red") + # regression line
  labs(title = "Scatterplot of SalePrice vs GrLivArea with Regression Line",
       x = "GrLivArea (Above ground living area in sq ft)",
       y = "SalePrice ($)") +
  theme_minimal()


# Compute standardized residuals
standardized_residuals <- rstandard(Model_1)

# Compute predicted values
predicted_values <- fitted(Model_1)

ggplot(data = NULL, aes(x = standardized_residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "red") +
  labs(title = "Histogram of Standardized Residuals",
       x = "Standardized Residuals",
       y = "Density")

ggplot(data = NULL, aes(x = predicted_values, y = standardized_residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Scatterplot of Standardized Residuals vs. Predicted Values",
       x = "Predicted Values",
       y = "Standardized Residuals")

# Leverage
hatvalues <- hatvalues(Model_1)
plot(hatvalues, main = "Hat Values (Leverage)")
abline(h = 2*(2+1)/length(new_df$SalePrice), col = "red")

# DFFITS
dffits_values <- dffits(Model_1)
plot(dffits_values, main = "DFFITS")
abline(h = 2*sqrt((2+1)/length(new_df$SalePrice)), col = "red")

# Cook's Distance
cooks_d <- cooks.distance(Model_1)
plot(cooks_d, main = "Cook's Distance")
abline(h = 1, col = "red")



### Question 2
# Fit the model
Model_2 <- lm(SalePrice ~ GrLivArea+OverallQual, data = new_df)

# Display the summary
summary(Model_2)


# ANOVA table
anova_table <- anova(Model_2)
print(anova_table)


# Standardized residuals
std_res <- rstandard(Model_2)

# Histogram
hist(std_res, main="Histogram of Standardized Residuals", xlab="Standardized Residuals")

# Scatterplot
plot(predict(Model_2), std_res, xlab="Predicted Values", ylab="Standardized Residuals", main="Residuals vs Fitted Values")
abline(h=0, col="red")

# Load necessary libraries
install.packages("car")
library(car)

# 1. DFFITS:
dffits_values <- dffits(Model_2)
plot(dffits_values, main="DFFITS Values", ylab="DFFITS", xlab="Index")
abline(h=c(-2/sqrt(nrow(new_df)), 2/sqrt(nrow(new_df))), col="red", lty=2)

# 2. Cook's Distance:
cooks_d <- cooks.distance(Model_2)
plot(cooks_d, main="Cook's Distance", ylab="Cook's Distance", xlab="Index")
abline(h=4/length(cooks_d), col="red", lty=2)

# 3. Leverage:
hat_values <- hatvalues(Model_2)
plot(hat_values, main="Hat (Leverage) Values", ylab="Hat Values", xlab="Index")
abline(h=2*length(coef(Model_2))/nrow(new_df), col="red", lty=2)

# 4. Influence Plot:
influencePlot(Model_2, main="Influence Plot")


# 5. QQ-Plot for residuals:
qqPlot(residuals(Model_2), main="QQ-Plot for Residuals")




### Question 3
# Fit the model
Model_3 <- lm(SalePrice ~ GrLivArea+OverallQual+GarageArea, data = new_df)

# Display the summary
summary(Model_3)

# ANOVA table
anova_table <- anova(Model_3)
print(anova_table)


# Standardized residuals
std_res <- rstandard(Model_3)


# Histogram
hist(std_res, main="Histogram of Standardized Residuals", xlab="Standardized Residuals")

# Scatterplot
plot(predict(Model_3), std_res, xlab="Predicted Values", ylab="Standardized Residuals", main="Residuals vs Fitted Values")
abline(h=0, col="red")


# 1. DFFITS:
dffits_values <- dffits(Model_3)
plot(dffits_values, main="DFFITS Values", ylab="DFFITS", xlab="Index")
abline(h=c(-2/sqrt(nrow(new_df)), 2/sqrt(nrow(new_df))), col="red", lty=2)

# 2. Cook's Distance:
cooks_d <- cooks.distance(Model_3)
plot(cooks_d, main="Cook's Distance", ylab="Cook's Distance", xlab="Index")
abline(h=4/length(cooks_d), col="red", lty=2)

# 3. Leverage:
hat_values <- hatvalues(Model_3)
plot(hat_values, main="Hat (Leverage) Values", ylab="Hat Values", xlab="Index")
abline(h=2*length(coef(Model_2))/nrow(new_df), col="red", lty=2)


### Question 4


# Transform SalePrice using Natural Logarithm
new_df$log_SalePrice <- log(new_df$SalePrice)

# Fit Model_4
Model_4 <- lm(log_SalePrice ~ GrLivArea + OverallQual + GarageArea, data = new_df)

# Display the summary
summary(Model_4)


# ANOVA table
anova_table <- anova(Model_4)
print(anova_table)

# Extract Adjusted R-squared for Both Models
adj_r2_model3 <- summary(Model_3)$adj.r.squared
adj_r2_model4 <- summary(Model_4)$adj.r.squared

# Extract Residual Standard Error for Both Models
resid_se_model3 <- summary(Model_3)$sigma
resid_se_model4 <- summary(Model_4)$sigma

# Compare Models Using Akaike Information Criterion (AIC)
aic_model3 <- AIC(Model_3)
aic_model4 <- AIC(Model_4)

# Print Comparison Results
cat("Adjusted R-squared for Model 3:", adj_r2_model3, "\n")
cat("Adjusted R-squared for Model 4:", adj_r2_model4, "\n")
cat("Residual Standard Error for Model 3:", resid_se_model3, "\n")
cat("Residual Standard Error for Model 4:", resid_se_model4, "\n")
cat("AIC for Model 3:", aic_model3, "\n")
cat("AIC for Model 4:", aic_model4, "\n")


### Question 5


# 1. Calculate DFFITS, Cook's Distance, and Hat values
dffits_values <- dffits(Model_3)
cooks_d <- cooks.distance(Model_3)
hat_values <- hatvalues(Model_3)

# 2. Set thresholds
threshold_dffits <- 2/sqrt(nrow(new_df))
threshold_cooks <- 4/(nrow(new_df) - length(coef(Model_3)) - 1)
threshold_hat <- 2*length(coef(Model_3))/nrow(new_df)

# 3. Identify the influential data points for each threshold
influential_dffits <- which(dffits_values > threshold_dffits)
influential_cooks <- which(cooks_d > threshold_cooks)
influential_hat <- which(hat_values > threshold_hat)

# Print the number of influential data points by each threshold
cat("Number of influential data points by DFFITS:", length(influential_dffits), "\n")
cat("Number of influential data points by Cook's Distance:", length(influential_cooks), "\n")
cat("Number of influential data points by Hat values:", length(influential_hat), "\n")

# 4. Combine the influential points from all three thresholds
all_influential_points <- unique(c(influential_dffits, influential_cooks, influential_hat))

# Get the total number of unique influential points
length(all_influential_points)

# 5. Remove those points from the dataset
new_df_cleaned <- new_df[-all_influential_points, ]
dim(new_df_cleaned)

# Print the number of observations left after removal
cat("Number of observations after removal of influential points:", nrow(new_df_cleaned), "\n")

# 6. Refit the model with the modified dataset
Model_3_cleaned <- lm(SalePrice ~ GrLivArea + OverallQual + GarageArea, data = new_df_cleaned)

# Display the summary of the refitted model
summary(Model_3_cleaned)


# ANOVA table
anova_table <- anova(Model_3_cleaned)
print(anova_table)

# Compute AIC for the original model
aic_original <- AIC(Model_3)

# Compute AIC for the refitted model (after removal of influential points)
aic_refitted <- AIC(Model_3_cleaned)

# Print the AIC values
cat("AIC for the original model:", aic_original, "\n")
cat("AIC for the refitted model:", aic_refitted, "\n")



### Question 6


# Load necessary library
library(stats)

# Initial full model with all predictors
full_model <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + FullBath + GarageArea + YearBuilt + FirstFlrSF + TotRmsAbvGrd + TotalBsmtSF + YearRemodel, data=new_df)

# Initial null model with no predictors
null_model <- lm(SalePrice ~ 1, data=new_df)

# Perform mixed stepwise regression
stepwise_model <- step(null_model, 
                       scope=list(lower=null_model, upper=full_model), 
                       direction="both", 
                       trace=1, # to show steps
                       k=log(nrow(new_df)) # AIC adjustment; for BIC, k=log(nrow(new_df))
)

# Display the final model summary
summary(stepwise_model)

# ANOVA table
anova_table <- anova(stepwise_model)
print(anova_table)

all_predictors <- c("OverallQual", "GrLivArea", "GarageCars", "FullBath", 
                    "GarageArea", "YearBuilt", "FirstFlrSF", "TotRmsAbvGrd", 
                    "TotalBsmtSF", "YearRemodel")

included_predictors <- names(coef(stepwise_model))[-1] # -1 to exclude the intercept

excluded_predictors <- setdiff(all_predictors, included_predictors)

print(excluded_predictors)

# Plot Residuals
plot(stepwise_model$fitted.values, residuals(stepwise_model), main="Residuals vs Fitted Values", xlab="Fitted Values", ylab="Residuals")

# Indepdence of Residuals
install.packages("lmtest")
library(lmtest)
dwtest(stepwise_model)

# No  Multicollinearity:
vif(stepwise_model)


# No Auto Correlation

acf(residuals(stepwise_model))
