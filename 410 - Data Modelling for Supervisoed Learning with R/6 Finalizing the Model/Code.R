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
library(reshape2)
library(MASS)
library(car)

### Assignment 6

## Preparatory Work

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




##########################################
### Create the typical_homes dataset###


# Load necessary libraries (ensure they are installed)
# library(your_required_library_for_waterfall)

# Initial count for the waterfall chart
start_count <- nrow(mydata)

# Apply initial filters
condition1 <- mydata[mydata$Zoning %in% c("RH", "RL", "RM"), ]
condition2 <- condition1[condition1$SaleCondition == "Normal", ]
condition3 <- condition2[condition2$BldgType == "1Fam", ]
condition4 <- condition3[condition3$Functional == "Typ", ]
condition5 <- condition4[condition4$SalePrice <= 339500, ]
condition6 <- condition5[condition5$GrLivArea <= 2667, ]

# Create the typical_homes dataframe
typical_homes <- condition6

# Count duplicate rows in typical_homes dataframe
duplicate_count <- sum(duplicated(typical_homes))
print(paste("Number of duplicate rows:", duplicate_count))

# Count number of rows before removing duplicates
before_removal <- nrow(typical_homes)

# Removing duplicate rows
typical_homes_unique <- unique(typical_homes)

# Count number of rows after removing duplicates
after_removal <- nrow(typical_homes_unique)

# Calculate number of duplicate rows
num_duplicates <- before_removal - after_removal

print(paste("Number of duplicate rows: ", num_duplicates))

# Checking YearBuilt and YearRemodel consistency
typical_homes <- typical_homes[!(typical_homes$YearBuilt > typical_homes$YearRemodel),]

# Filtering rows with negative SalePrice
negative_saleprice_rows <- typical_homes[typical_homes$SalePrice < 0, ]
print(negative_saleprice_rows)  # Print rows with negative SalePrice

# Dropping specific columns
typical_homes$LotFrontage <- NULL
typical_homes$GarageYrBlt <- NULL

# Drop rows with missing values in specific columns
typical_homes <- typical_homes[!is.na(typical_homes$MasVnrType), ]
typical_homes <- typical_homes[!is.na(typical_homes$BsmtExposure), ]
typical_homes <- typical_homes[!is.na(typical_homes$BsmtFinType2), ]
typical_homes <- typical_homes[!is.na(typical_homes$Electrical), ]

# Calculate the number of rows dropped at each filtering step for the waterfall chart
drops <- c(
  start_count,
  nrow(condition1) - nrow(mydata),
  nrow(condition2) - nrow(condition1),
  nrow(condition3) - nrow(condition2),
  nrow(condition4) - nrow(condition3),
  nrow(condition5) - nrow(condition4),
  nrow(condition6) - nrow(condition5),
  num_duplicates, # Duplicates removed
  before_removal - after_removal, # YearBuilt vs YearRemodel inconsistency
  length(which(typical_homes$SalePrice < 0))  # Negative SalePrice rows
)

# Open a graphics device
plot.new()

# Create labels for each condition
labels <- c(
  "Initial Count", "After Zoning Filter", "After Sale Condition Filter",
  "After Building Type Filter", "After Functionality Filter",
  "After Price Constraint", "After Living Area Constraint",
  "Duplicates Removed", "Year Consistency", "Negative SalePrice"
)

# Plot the waterfall chart
# (Ensure that you've loaded the necessary library for the waterfall function)
waterfall(values = drops, labels = labels)

# Calculate positions for labels
pos <- 1:length(labels)

# Add rotated labels at the specified positions
text(x = pos, y = par("usr")[3] - (max(drops) * 0.05), 
     labels = labels, srt = 45, adj = 1, xpd = TRUE, cex=0.8)

# Display counts of missing values in columns
missing_counts <- colSums(is.na(typical_homes))
missing_counts <- missing_counts[missing_counts > 0]
print(missing_counts)



##########################################



# Display the size of the updated dataframe
size <- dim(typical_homes)
print(paste("The filtered dataframe has", size[1], "rows and", size[2], "columns."))


# Histogram for SalePrice 
hist(typical_homes$SalePrice, main="Histogram for SalePrice", xlab="SalePrice", col="lightblue")

# Create the boxplot without the y-axis
boxplot(typical_homes$SalePrice, main="Boxplot of SalePrice", xlab="SalePrice", yaxt="n", col="lightblue", border="black")

# Add a custom y-axis with non-scientific notation
axis(2, at=seq(min(typical_homes$SalePrice, na.rm = TRUE), max(typical_homes$SalePrice, na.rm = TRUE), by=50000), 
     labels=format(seq(min(typical_homes$SalePrice, na.rm = TRUE), max(typical_homes$SalePrice, na.rm = TRUE), by=50000), scientific=FALSE))



# Extract numeric columns
numeric_cols <- names(typical_homes)[sapply(typical_homes, is.numeric)]

# Identify discrete columns based on a threshold of unique values
# (e.g., if a column has 15 or fewer unique values, consider it discrete)
threshold <- 15
discrete_cols <- numeric_cols[sapply(typical_homes[, numeric_cols], function(col) {
  length(unique(col)) <= threshold
})]

# Continuous columns are the numeric columns that are not discrete
continuous_cols <- setdiff(numeric_cols, discrete_cols)
continuous_cols <- setdiff(continuous_cols, "SID")
continuous_cols <- setdiff(continuous_cols, "PID")


# Print results
print("Continuous columns:")
print(continuous_cols)

print("Discrete columns:")
print(discrete_cols)

# Prevent using scientific notation
options(scipen = 999)


# Compute summary statistics for continuous columns
summary_stats <- sapply(typical_homes[, continuous_cols], function(col) {
  c(
    n = length(col[!is.na(col)]), 
    missing = sum(is.na(col)),  # Number of missing values
    mean = mean(col, na.rm = TRUE), 
    median = median(col, na.rm = TRUE), 
    sd = sd(col, na.rm = TRUE), 
    min = min(col, na.rm = TRUE), 
    max = max(col, na.rm = TRUE)
  )
})

# Print the summary using kable
kable(t(summary_stats), caption = "Statistical Summary of Continuous Variables with Missing Values")


# Function to compute missing, levels, and counts for a column
get_discrete_stats <- function(col) {
  missing <- sum(is.na(col))
  levels_col <- unique(col[!is.na(col)])
  counts <- table(col, useNA = "no")
  list(
    missing = missing,
    levels = paste(levels_col, collapse = ", "),
    counts = paste(counts, collapse = ", ")
  )
}

# Compute statistics for all discrete columns
discrete_stats <- lapply(typical_homes[, discrete_cols], get_discrete_stats)

# Convert the list to a data.frame
discrete_stats_df <- do.call(rbind, discrete_stats)
rownames(discrete_stats_df) <- discrete_cols

# Print using kable
kable(discrete_stats_df, caption = "Statistics for Discrete Columns")


# Melt the data for ggplot2
melted_data <- melt(typical_homes, id.vars = "SalePrice", measure.vars = continuous_cols)

# Using a color gradient based on SalePrice values
p <- ggplot(melted_data, aes(x = value, y = SalePrice, color = SalePrice)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ variable, scales = "free_x") +
  scale_color_gradient(low = "blue", high = "red") + # Gradient from blue to red
  theme_light() +
  labs(title = "Scatter Plots of Continuous Columns vs. SalePrice",
       x = "Value of Continuous Column", y = "SalePrice")

print(p)



# First, we'll melt the dataframe to long format, focusing on the discrete columns and SalePrice
melted_data <- melt(typical_homes, id.vars = "SalePrice", measure.vars = discrete_cols)

# Plot
p <- ggplot(melted_data, aes(x = as.factor(value), y = SalePrice, color = as.factor(value))) +
  geom_point(alpha = 0.5, position = position_jitter(width = 0.4)) + # Add jitter for better visualization of overlapping points
  facet_wrap(~ variable, scales = "free_x") +
  scale_color_brewer(palette = "Set1") + # Using a color palette
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for clarity
  labs(title = "Scatter Plots of Discrete Columns vs. SalePrice",
       x = "Value of Discrete Column", y = "SalePrice")

print(p)



## Print the Summary Statistics of Discrete Variables
# Iterate over discrete columns
for(col in discrete_cols) {
  
  # Use tapply to calculate summary statistics for each level
  stats <- tapply(typical_homes$SalePrice, typical_homes[[col]], function(x) {
    c(n = length(x),
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  })
  
  # Convert the list to a data frame for better printing
  df_stats <- as.data.frame(do.call(rbind, stats))
  
  # Print the column name
  cat(paste0("\nSummary statistics for SalePrice based on ", col, ":\n"))
  
  # Display the statistics using kable
  print(kable(df_stats, row.names = TRUE, digits = 2))
}


## Drop all the rows in all discrete_cols, where for any level, n<6

# Loop over discrete columns to filter out rows
for(col in discrete_cols) {
  # Calculate frequency for each level
  freqs <- table(typical_homes[[col]])
  
  # Identify levels with count less than 6
  insufficient_levels <- names(freqs[freqs < 6])
  
  if(length(insufficient_levels) > 0) {
    # Drop rows with those levels
    typical_homes <- typical_homes %>% 
      filter(!(.[[col]] %in% insufficient_levels))
  }
}

# Display the size of the updated dataframe
size <- dim(typical_homes)
print(paste("The filtered dataframe has", size[1], "rows and", size[2], "columns."))



# Calculate the number of levels for each discrete column
num_levels <- sapply(discrete_cols, function(col) {
  length(unique(typical_homes[[col]]))
})

# Convert to data frame for kable
df_num_levels <- data.frame(Variable = names(num_levels), Levels = num_levels)

# Sort the data frame by the Levels column in descending order
df_num_levels_sorted <- df_num_levels[order(-df_num_levels$Levels), ]

# Print using kable
kable(df_num_levels_sorted, row.names = FALSE)



# Variables with more than 1 and less than 4 levels
selected_vars <- c("Fireplaces", "BsmtFullBath", "BsmtHalfBath", "HalfBath")

# Compute statistics for all discrete columns
discrete_stats <- lapply(typical_homes[, selected_vars], get_discrete_stats)

# Convert the list to a data.frame
discrete_stats_df <- do.call(rbind, discrete_stats)
rownames(discrete_stats_df) <- selected_vars

# Print using kable
kable(discrete_stats_df, caption = "Statistics for Discrete Columns")


## Print the Stsatistical Summary of SalePrice for Discrete Variables with
## more than 1 levels and less than 4 levels

## Print the Summary Statistics of Selected Discrete Variables
for(col in selected_vars) {
  
  # Use tapply to calculate summary statistics for each level
  stats <- tapply(typical_homes$SalePrice, typical_homes[[col]], function(x) {
    c(n = length(x),
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  })
  
  # Convert the list to a data frame for better visualization
  df_stats <- as.data.frame(do.call(rbind, stats))
  
  # Add a column for the values (levels) of the discrete variable
  df_stats$Value <- rownames(df_stats)
  
  # Display the column name
  cat(paste0("\nSummary statistics for SalePrice based on ", col, ":\n"))
  
  # Display the statistics using kable without specifying row.names
  print(kable(df_stats, digits = 2))
}



## Given the limitations of computing power, and considering the SalePrice differences within levels, 
## we decide to go choose GarageCars, FullBath, and BsmtFullBath.

# Variables with more than 1 and less than 4 levels
selected_vars <- c("GarageCars", "FullBath", "BsmtFullBath")

# Compute statistics for all discrete columns
discrete_stats <- lapply(typical_homes[, selected_vars], get_discrete_stats)

# Convert the list to a data.frame
discrete_stats_df <- do.call(rbind, discrete_stats)
rownames(discrete_stats_df) <- selected_vars

# Print using kable
kable(discrete_stats_df, caption = "Statistics for Discrete Columns")


## Print the Stsatistical Summary of SalePrice for Discrete Variables with
## more than 1 levels and less than 4 levels

## Print the Summary Statistics of Selected Discrete Variables
for(col in selected_vars) {
  
  # Use tapply to calculate summary statistics for each level
  stats <- tapply(typical_homes$SalePrice, typical_homes[[col]], function(x) {
    c(n = length(x),
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  })
  
  # Convert the list to a data frame for better visualization
  df_stats <- as.data.frame(do.call(rbind, stats))
  
  # Add a column for the values (levels) of the discrete variable
  df_stats$Value <- rownames(df_stats)
  
  # Display the column name
  cat(paste0("\nSummary statistics for SalePrice based on ", col, ":\n"))
  
  # Display the statistics using kable without specifying row.names
  print(kable(df_stats, digits = 2))
}





### Create Dummy Variables


# Create new dummy columns GarageCars_d1, GarageCars_d2, GarageCars_d3, with initial values as 0
typical_homes$GarageCars_d1 <- 0
typical_homes$GarageCars_d2 <- 0
typical_homes$GarageCars_d3 <- 0

# Update the dummy columns based on the value in GarageCars
typical_homes$GarageCars_d1[typical_homes$GarageCars == 1] <- 1
typical_homes$GarageCars_d2[typical_homes$GarageCars == 2] <- 1
typical_homes$GarageCars_d3[typical_homes$GarageCars == 3] <- 1


# Create new dummy columns FullBath_d1, FullBath_d2 with initial values as 0
typical_homes$FullBath_d1 <- 0
typical_homes$FullBath_d2 <- 0


# Update the dummy columns based on the value in FullBath
typical_homes$FullBath_d1[typical_homes$FullBath == 2] <- 1
typical_homes$FullBath_d2[typical_homes$FullBath == 3] <- 1


# Selecting the columns of interest
selected_columns <- c(
  "GarageCars",
  "GarageCars_d1",
  "GarageCars_d2",
  "GarageCars_d3",
  "FullBath",
  "FullBath_d1",
  "FullBath_d2",
  "BsmtHalfBath"
)

# Display the selected columns and their headers
header_and_columns <- typical_homes[selected_columns]
print(header_and_columns)

colnames(typical_homes)


# Variables of interest from the 'continuous_cols' vector
vars <- continuous_cols

# Calculate correlations
correlations <- sapply(vars, function(var) cor(typical_homes[[var]], typical_homes$SalePrice, use="complete.obs"))

# Convert to a dataframe for better printing
cor_df <- data.frame(Variable = names(correlations), Correlation = correlations) %>% 
  arrange(-Correlation)

# Print using kable
kable(cor_df, caption = "Correlations of Variables with SalePrice")



## Create a new dataframe with continuous variables (correlation with SalePrice>0.3), and discrete variables

# Calculate correlations
correlations <- sapply(continuous_cols, function(var) cor(typical_homes[[var]], 
                                                          typical_homes$SalePrice, use="complete.obs"))

# Select variables with correlation > 0.3
selected_continuous_vars <- names(correlations[correlations > 0.3])

selected_continuous_vars

# Discrete variables of interest
discrete_vars <- c("GarageCars_d1", "GarageCars_d2", "GarageCars_d3",
                   "FullBath_d1", "FullBath_d2", "BsmtFullBath")

# Create 'data' dataframe
data <- typical_homes %>%
  dplyr::select(all_of(c(selected_continuous_vars, discrete_vars, "OverallQual", "OverallCond", "BsmtFinSF2", "GrLivArea")))



# Display the first few rows of the new dataframe
head(data)

dim(data)


colnames(data)


data <- data[, c("SalePrice", setdiff(names(data), "SalePrice"))]


#########################################################################################

####### Task 1




# Set the seed on the random number generator for reproducibility
set.seed(123)

# Generate random numbers between 0 and 1 for each row in data
data$u <- runif(n=dim(data)[1], min=0, max=1)

# Define two new variables for later use
data$QualityIndex <- data$OverallQual * data$OverallCond
data$TotalSqftCalc <- data$BsmtFinSF1 + data$BsmtFinSF2 + data$GrLivArea

# Create a train/test split based on the random numbers in the 'u' column
train.df <- subset(data, u < 0.70)
test.df <- subset(data, u >= 0.70)

# Check the data split to ensure everything adds up correctly
print(paste("Total rows in data:", dim(data)[1]))
print(paste("Total rows in train.df:", dim(train.df)[1]))
print(paste("Total rows in test.df:", dim(test.df)[1]))
print(paste("Combined rows from train.df and test.df:", dim(train.df)[1] + dim(test.df)[1]))


# Create a data frame to hold the counts
partition_counts <- data.frame(
  Dataset = c("train.df", "test.df", "Combined", "data"),
  Observations = c(dim(train.df)[1], dim(test.df)[1], dim(train.df)[1] + dim(test.df)[1], dim(data)[1])
)

# Display the table using kable
kable(partition_counts, caption = "Observation Counts for Train/Test Data Partition")







#########################################################################################

####### Task 2

train.df1 <- train.df

# Creating a list of columns that can be dropped from the model
drop.list <- c("GarageArea", "WoodDeckSF", "OpenPorchSF", "BsmtFinSF2", "GrLivArea", "BsmtFullBath")

# Dropping the columns
train.clean <-train.df[,!(names(data) %in% drop.list)];




# Define the upper model as the FULL model using train.clean
upper.lm <- lm(SalePrice ~ ., data=train.clean)

# Define the lower model as the Intercept model using train.clean
lower.lm <- lm(SalePrice ~ 1, data=train.clean)

# Define a simple linear regression model as a starting point for stepwise regression
sqft.lm <- lm(SalePrice ~ TotalSqftCalc, data=train.clean)

# -----------------------------------
# FORWARD SELECTION
# -----------------------------------
# Begin with the intercept model and add predictors to achieve the best model fit
forwardModel <- stepAIC(object=lower.lm, 
                        scope=list(upper=formula(upper.lm), lower=~1),
                        direction="forward")
cat("\n\nFORWARD SELECTION SUMMARY:\n")
summary(forwardModel)

# -----------------------------------
# BACKWARD ELIMINATION
# -----------------------------------
# Start with the full model and remove predictors to achieve the best model fit
backwardModel <- stepAIC(object=upper.lm, 
                         direction="backward")
cat("\n\nBACKWARD ELIMINATION SUMMARY:\n")
summary(backwardModel)

# -----------------------------------
# STEPWISE REGRESSION
# -----------------------------------
# Combine forward and backward approaches
stepwiseModel <- stepAIC(object=sqft.lm, 
                         scope=list(upper=formula(upper.lm), lower=~1),
                         direction="both")
cat("\n\nSTEPWISE REGRESSION SUMMARY:\n")
summary(stepwiseModel)




## Checking the train.df with other columns
junk.lm <- lm(SalePrice ~ OverallQual + OverallCond + QualityIndex + GrLivArea + 
                TotalSqftCalc, data=train.df1)
summary(junk.lm)


# Compute the VIF values
sort(vif(forwardModel),decreasing=TRUE)
sort(vif(backwardModel),decreasing=TRUE)
sort(vif(stepwiseModel),decreasing=TRUE)
sort(vif(junk.lm),decreasing=TRUE)


# Compute the correlation between variables with vif>0.5
# Subset the data to only the variables of interest
subset_data <- data %>%
  dplyr::select(TotalSqftCalc, SecondFlrSF, FirstFlrSF, BsmtFinSF1)

# Compute the correlation matrix
cor_matrix <- cor(subset_data)

# Print the correlation matrix
print(cor_matrix)

# Optional: Visualize the correlation matrix using corrplot package
# install.packages("corrplot")  # Install the package if not already done
library(corrplot)
corrplot(cor_matrix, method = "circle")

### Second Iteration of Model Creation for reducing Multicollinearity

# Creating a list of columns that can be dropped from the model
drop.list2 <- c("TotalSqftCalc")

# Dropping the columns
train.clean2 <- train.clean[, !(names(train.clean) %in% drop.list2)]

# Define the upper model as the FULL model using train.clean2
upper.lm <- lm(SalePrice ~ ., data=train.clean2)

# Define the lower model as the Intercept model using train.clean2
lower.lm <- lm(SalePrice ~ 1, data=train.clean2)

# Define a simple linear regression model as a starting point for stepwise regression
OverallQual.lm <- lm(SalePrice ~ OverallQual, data=train.clean2)

# -----------------------------------
# FORWARD SELECTION
# -----------------------------------
# Begin with the intercept model and add predictors to achieve the best model fit
forwardModel <- stepAIC(object=lower.lm, 
                        scope=list(upper=formula(upper.lm), lower=~1),
                        direction="forward")
cat("\n\nFORWARD SELECTION SUMMARY:\n")
summary(forwardModel)

# -----------------------------------
# BACKWARD ELIMINATION
# -----------------------------------
# Start with the full model and remove predictors to achieve the best model fit
backwardModel <- stepAIC(object=upper.lm, 
                         direction="backward")
cat("\n\nBACKWARD ELIMINATION SUMMARY:\n")
summary(backwardModel)

# -----------------------------------
# STEPWISE REGRESSION
# -----------------------------------
# Combine forward and backward approaches
stepwiseModel <- stepAIC(object=OverallQual.lm, 
                         scope=list(upper=formula(upper.lm), lower=~1),
                         direction="both")
cat("\n\nSTEPWISE REGRESSION SUMMARY:\n")
summary(stepwiseModel)


# Compute the VIF values
sort(vif(forwardModel),decreasing=TRUE)
sort(vif(backwardModel),decreasing=TRUE)
sort(vif(stepwiseModel),decreasing=TRUE)


# Compute the metrics for all the 4 models
# List of models
models <- list(forwardModel, backwardModel, stepwiseModel, junk.lm)
# Model names
model_names <- c("forwardModel", "backwardModel", "stepwiseModel", "junk.lm")
# Initialize a list to store results
results <- list()
# Loop through each model and compute metrics
for (i in 1:length(models)) {
   # Get the model
  model <- models[[i]]
  # Predicted values
  predicted_values <- predict(model, train.df1)
  # Compute metrics
  adj_r_squared <- summary(model)$adj.r.squared
  aic_value <- AIC(model)
  bic_value <- BIC(model)
  mse <- mean((train.clean$SalePrice - predicted_values)^2)
  mae <- mean(abs(train.clean$SalePrice - predicted_values))
  # Store the results
  results[[model_names[i]]] <- list(Adjusted_R2 = adj_r_squared,
                                    AIC = aic_value,
                                    BIC = bic_value,
                                    MSE = mse,
                                    MAE = mae)
}

# Display the results
# Convert the results list to a data frame
results_df <- do.call(rbind, results)
results_df <- data.frame(Model = rownames(results_df), results_df, row.names = NULL)

# Display the table using kable
kable(results_df, digits = 2, align = 'c', caption = "Metrics for the Models")




#########################################################################################

####### Task 3


# Compute the metrics for all the 4 models with test.data


# List of models
models <- list(forwardModel, backwardModel, stepwiseModel, junk.lm)
# Model names
model_names <- c("forwardModel", "backwardModel", "stepwiseModel", "junk.lm")
# Initialize a list to store results
results <- list()
# Loop through each model and compute metrics
for (i in 1:length(models)) {
  # Get the model
  model <- models[[i]]
  # Predicted values
  predicted_values <- predict(model, test.df)
  # Compute metrics
  mse <- mean((test.df$SalePrice - predicted_values)^2)
  mae <- mean(abs(test.df$SalePrice - predicted_values))
  # Store the results
  results[[model_names[i]]] <- list(MSE = mse,
                                    MAE = mae)
}

# Display the results
# Convert the results list to a data frame
results_df <- do.call(rbind, results)
results_df <- data.frame(Model = rownames(results_df), results_df, row.names = NULL)

# Display the table using kable
kable(results_df, digits = 2, align = 'c', caption = "Metrics for the Models")


#########################################################################################
####### Task 4


# Function to assign PredictionGrade
get_grade <- function(predicted, actual){
  error_rate <- abs(predicted - actual) / actual
  if (error_rate <= 0.10) {
    return("Grade 1")
  } else if (error_rate <= 0.15) {
    return("Grade 2")
  } else if (error_rate <= 0.25) {
    return("Grade 3")
  } else {
    return("Grade 4")
  }
}

# List of models
models <- list(
  forwardModel = forwardModel, 
  backwardModel = backwardModel, 
  stepwiseModel = stepwiseModel, 
  junk.lm = junk.lm
)

results <- list()


# Iterate over models to get grades for in-sample and out-of-sample data
for (model_name in names(models)) {
  model <- models[[model_name]]
  
  # Predict and get grades for in-sample data
  in_sample_predicted <- predict(model, newdata = train.df1)
  in_sample_grades <- sapply(1:length(in_sample_predicted), 
                             function(i) get_grade(in_sample_predicted[i], train.df1$SalePrice[i]))
  
  # Predict and get grades for out-of-sample data
  out_of_sample_predicted <- predict(model, newdata = test.df)
  out_of_sample_grades <- sapply(1:length(out_of_sample_predicted), 
                                 function(i) get_grade(out_of_sample_predicted[i], test.df$SalePrice[i]))
  
  results[[model_name]] <- list(in_sample = table(in_sample_grades), 
                                out_of_sample = table(out_of_sample_grades))
}


# Display results using kable
for (model_name in names(results)) {
  
  cat(paste0(model_name, " - In-sample Grades:"))
  print(kable(results[[model_name]]$in_sample, col.names = c("Grade", "Count")))
  cat("\n")
  cat(paste0(model_name, " - Out-of-sample Grades:"))
  print(kable(results[[model_name]]$out_of_sample, col.names = c("Grade", "Count")))
  cat("\n")
}

### Clustered Bar Chart

# Reshape data
data_list <- list()

for (model_name in names(results)) {
  
  # Convert table to data frame
  in_sample_df <- as.data.frame(results[[model_name]]$in_sample)
  names(in_sample_df) <- c("Grade", "Count")
  in_sample_df$Model <- model_name
  in_sample_df$SampleType <- "In-sample"
  
  out_of_sample_df <- as.data.frame(results[[model_name]]$out_of_sample)
  names(out_of_sample_df) <- c("Grade", "Count")
  out_of_sample_df$Model <- model_name
  out_of_sample_df$SampleType <- "Out-of-sample"
  
  data_list[[model_name]] <- bind_rows(in_sample_df, out_of_sample_df)
}

# Combine data frames
data <- bind_rows(data_list)

# Plot clustered bar chart
ggplot(data, aes(x=Model, y=Count, fill=Grade)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~SampleType) +
  labs(title="Prediction Grades for Different Models",
       x="Model",
       y="Count of Houses") +
  theme_minimal()



## Clustered bar chart for percentages

# Given datapoint counts
total_in_sample <- 1156
total_out_of_sample <- 488

# Reshape data and compute percentages
data_list <- list()

for (model_name in names(results)) {
  
  # Convert table to data frame
  in_sample_df <- as.data.frame(results[[model_name]]$in_sample)
  names(in_sample_df) <- c("Grade", "Percentage")
  in_sample_df$Percentage <- (in_sample_df$Percentage / total_in_sample) * 100  # Convert to percentage
  in_sample_df$Model <- model_name
  in_sample_df$SampleType <- "In-sample"
  
  out_of_sample_df <- as.data.frame(results[[model_name]]$out_of_sample)
  names(out_of_sample_df) <- c("Grade", "Percentage")
  out_of_sample_df$Percentage <- (out_of_sample_df$Percentage / total_out_of_sample) * 100  # Convert to percentage
  out_of_sample_df$Model <- model_name
  out_of_sample_df$SampleType <- "Out-of-sample"
  
  data_list[[model_name]] <- bind_rows(in_sample_df, out_of_sample_df)
}

# Combine data frames
data <- bind_rows(data_list)

# Plot clustered bar chart
ggplot(data, aes(x=Model, y=Percentage, fill=Grade)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=sprintf("%.1f", Percentage)), position=position_dodge(width=0.9), vjust=-0.5, size=3) +
  facet_wrap(~SampleType) +
  labs(title="Prediction Grades for Different Models (in Percentages)",
       x="Model",
       y="Percentage of Houses") +
  theme_classic()


#########################################################################################
####### Task 5


# 1. Check for Linearity and Additivity & Homoscedasticity
graphics.off()
plot(predict(stepwiseModel), residuals(stepwiseModel), main="Residuals vs Fitted", xlab="Fitted values", ylab="Residuals")
abline(h = 0, col = "red")

# 2. Check for Normality of Residuals
graphics.off()
hist(residuals(stepwiseModel), breaks=30, main="Histogram of Residuals")
qqnorm(residuals(stepwiseModel), main="Normal QQ-Plot")
qqline(residuals(stepwiseModel))


# 3. Check for Outliers and Influence Points

plot(stepwiseModel, which = 1) # standard residuals vs leverage plot

# For Cook's distance
cutoff <- 4/((length(residuals(stepwiseModel))-length(coef(stepwiseModel))-1))
plot(stepwiseModel, which = 4, cook.levels=cutoff)
abline(h = cutoff, col = "red")



# Original model
original_model <- lm(SalePrice ~ OverallQual + FirstFlrSF + SecondFlrSF + 
                       YearBuilt + BsmtFinSF1 + GarageCars_d3 + OverallCond + LotArea + 
                       YearRemodel + TotalBsmtSF + GarageCars_d2, data = train.clean2)

# Capture the R2 of the original model
original_R2 <- summary(original_model)$r.squared

# List of predictors
predictors <- c("OverallQual", "FirstFlrSF", "SecondFlrSF", "YearBuilt", "BsmtFinSF1", 
                "GarageCars_d3", "OverallCond", "LotArea", "YearRemodel", 
                "TotalBsmtSF", "GarageCars_d2")

# Empty vector to store significant predictors
significant_predictors <- vector()

# Loop through predictors to remove one at a time and check R2
for (var in predictors) {
  reduced_model_formula <- as.formula(
    paste("SalePrice ~", paste(setdiff(predictors, var), collapse = " + "))
  )
  reduced_model <- lm(reduced_model_formula, data = train.clean2)
  reduced_R2 <- summary(reduced_model)$r.squared
  
  # If R2 does not decrease by more than 0.5%, then include the variable as significant
  if ((original_R2 - reduced_R2) > 0.005) {
    significant_predictors <- c(significant_predictors, var)
  }
}

# Print significant predictors
significant_predictors

# Creating the model with significant variables
refined_model <- lm(SalePrice ~ OverallQual + FirstFlrSF + SecondFlrSF + 
                       YearBuilt + BsmtFinSF1 + GarageCars_d3 + OverallCond
                    + LotArea, data = train.clean2)
summary(refined_model)

# Removing LotArea, becuase it's coeeficient < 1
refined_model_1 <- lm(SalePrice ~ OverallQual + FirstFlrSF + SecondFlrSF + 
                      YearBuilt + BsmtFinSF1 + GarageCars_d3 + OverallCond, 
                    data = train.clean2)
summary(refined_model_1)

# Adding the related discrete variables GarageCars_d1 and GarageCars_d2 
refined_model_2 <- lm(SalePrice ~ OverallQual + FirstFlrSF + SecondFlrSF + GarageCars_d1 + 
                      GarageCars_d2 +  YearBuilt + BsmtFinSF1 + GarageCars_d3 + OverallCond, 
                      data = train.clean2)
summary(refined_model_2)

# Creating an ANCOVA model for planes generated by the dummy variables 
# GarageCars_d1, GarageCars_d2, and GarageCars_d3

ancova_model <- lm(SalePrice ~ OverallQual + FirstFlrSF + SecondFlrSF + GarageCars_d1 + 
                     GarageCars_d2 + GarageCars_d3 + YearBuilt + BsmtFinSF1 + OverallCond +
                     OverallQual:GarageCars_d1 + OverallQual:GarageCars_d2 + OverallQual:GarageCars_d3 +
                     FirstFlrSF:GarageCars_d1 + FirstFlrSF:GarageCars_d2 + FirstFlrSF:GarageCars_d3 +
                     SecondFlrSF:GarageCars_d1 + SecondFlrSF:GarageCars_d2 + SecondFlrSF:GarageCars_d3 +
                     YearBuilt:GarageCars_d1 + YearBuilt:GarageCars_d2 + YearBuilt:GarageCars_d3 +
                     BsmtFinSF1:GarageCars_d1 + BsmtFinSF1:GarageCars_d2 + BsmtFinSF1:GarageCars_d3 +
                     OverallCond:GarageCars_d1 + OverallCond:GarageCars_d2 + OverallCond:GarageCars_d3,
                   data = train.clean2)

summary(ancova_model)


# Perform stepwise regression
stepwiseModel <- step(ancova_model, direction = "both")


# Perform backward elimination
backwardModel <- step(ancova_model, direction = "backward")


# Define a null model with only the intercept
null_model <- lm(SalePrice ~ 1, data = train.clean2)

# Perform forward selection
forwardModel <- step(null_model, direction = "forward", scope = list(lower = null_model, upper = ancova_model))

# Print the summary of the selected model
summary(stepwiseModel)

# Print the summary of the selected model using forward selection
summary(forwardModel)

# Print the summary of the selected model using backward elimination
summary(backwardModel)


## Goodness for fit


# 1. Residuals vs Fitted Plot
residuals_vs_fitted <- ggplot(forwardModel, aes(.fitted, .resid)) +
  geom_point(aes(y = .resid), alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(se = FALSE, method = "loess", color = "blue") +
  ggtitle("Residuals vs Fitted") +
  theme_minimal()
print(residuals_vs_fitted)

# 2. Normal Q-Q Plot
qq_plot <- ggplot(forwardModel, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Normal Q-Q Plot") +
  theme_minimal()
print(qq_plot)



# 3. Cook's Distance Plot
cooksd <- cooks.distance(forwardModel)

# Compute the threshold
n <- length(cooksd)
threshold <- 4/n

# Print the threshold to check its value
print(threshold)

# Plot Cook's distance with adjusted y-axis limits
plot(cooksd, pch = "*", cex = 2, main = "Cook's distance plot", ylim = c(0, max(cooksd) * 1.1))

# Add the threshold line
abline(h = threshold, col = "red", lty = 2, lwd = 2)

plot(cooksd, pch = "*", cex = 2, main = "Cook's distance plot")



