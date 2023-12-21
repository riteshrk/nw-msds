'install.packages("readxl")
install.packages("knitr")
install.packages("purrr")
install.packages("tidyr")
install.packages("car")'




library(readxl)
library(dplyr)
library(purrr)
library(knitr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(car)
library(lmtest)



# Read the Excel file
data <- read_excel("NutritionStudy.xls")

# View the first few rows of the data
head(data)

# Get the dimensions of the dataframe
dim(data)

# Load necessary libraries
library(readxl)
library(dplyr)


# Identify continuous and discrete variables
continuous_vars <- names(data)[sapply(data, is.double)]

# Manually move 'PriorSmoke' to discrete variables
continuous_vars <- setdiff(continuous_vars, "PriorSmoke")
continuous_vars <- setdiff(continuous_vars, "ID")
discrete_vars <- c(names(data)[sapply(data, is.character)], "PriorSmoke")

# Print the lists
cat("Continuous Variables:\n")
print(continuous_vars)
cat("\nDiscrete Variables:\n")
print(discrete_vars)

 


# Pivoting the data longer to get variable names as a column
long_data <- data %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "Variable")

# Summarizing
summary_data <- long_data %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(value, na.rm = TRUE),
    Median = median(value, na.rm = TRUE),
    SD = sd(value, na.rm = TRUE),
    Min = min(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE)
  )

kable(summary_data, caption = "Summary for Continuous Variables")

 


# Define the list of categorical variables
categorical_vars <- c("Smoke", "Gender", "VitaminUse", "PriorSmoke")

# Loop through each categorical variable and print its frequency table
for (var in categorical_vars) {
  
  # Generate a frequency table
  freq_table <- as.data.frame(table(data[[var]]))
  colnames(freq_table) <- c("Level", "Frequency")
  
  cat("Summary for", var, ":\n")
  print(freq_table)
  cat("\n")  # Add an empty line for separation
}


# Check the number of missing values for each column
missing_values <- sapply(data, function(x) sum(is.na(x)))

# Print out the number of missing values per column
print(missing_values)


# Create a function to identify outliers for a single variable
find_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

# Apply the function to each continuous variable
outliers_list <- lapply(data[continuous_vars], find_outliers)

# Calculate the number of outliers for each variable
num_outliers <- sapply(outliers_list, sum, na.rm = TRUE)

# Convert to dataframe for proper presentation
outliers_df <- data.frame(Variable = names(num_outliers), NumOutliers = num_outliers)

# Print the table using kable
kable(outliers_df, caption = "Number of Outliers for Each Variable")



# Reshape data from wide to long format
long_data <- data %>%
  select(continuous_vars) %>%
  gather(key = "Variable", value = "Value")

# Plot boxplots with facets
ggplot(long_data, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free") +
  theme(axis.text.x = element_blank()) + 
  labs(title = "Boxplots for Each Continuous Variable", y = "Value", x = NULL)


## Create Dummy Variables

# For "Smoke"
data <- data %>%
  mutate(Smoke_d = ifelse(Smoke == "Yes", 1, 0))

# For "Gender"
data <- data %>%
  mutate(Gender_d = ifelse(Gender == "Male", 1, 0))

# For "VitaminUse"
data <- data %>%
  mutate(VitaminUse_d1 = ifelse(VitaminUse == "No", 1, 0),
         VitaminUse_d2 = ifelse(VitaminUse == "Yes", 1, 0),
         VitaminUse_d3 = ifelse(VitaminUse == "Regular", 1, 0))

# For "PriorSmoke"
data <- data %>%
  mutate(PriorSmoke_d1 = ifelse(PriorSmoke == 1, 1, 0),
         PriorSmoke_d2 = ifelse(PriorSmoke == 2, 1, 0),
         PriorSmoke_d3 = ifelse(PriorSmoke == 3, 1, 0))

head(data[, c("Smoke_d", "Gender_d", "VitaminUse_d1", "VitaminUse_d2", "VitaminUse_d3", "PriorSmoke_d1", "PriorSmoke_d2", "PriorSmoke_d3")])


# Create new categorical variable based on Alcohol
data$Alcohol_cat <- cut(data$Alcohol, breaks = c(-Inf, 0, 10, Inf), labels = c("d1", "d2", "d3"), right = FALSE)

# Create dummy variables
data$Alcohol_d1 <- ifelse(data$Alcohol_cat == "d1", 1, 0)
data$Alcohol_d2 <- ifelse(data$Alcohol_cat == "d2", 1, 0)
data$Alcohol_d3 <- ifelse(data$Alcohol_cat == "d3", 1, 0)

# Remove the temporary Alcohol_cat column
data$Alcohol_cat <- NULL


head(data[, c("Alcohol_d1", "Alcohol_d2", "Alcohol_d3")])


### Task1



# Descriptive statistics for Cholesterol by PriorSmoke
desc_stats <- data %>%
  group_by(PriorSmoke) %>%
  summarise(
    n = n(),
    mean = mean(Cholesterol),
    sd = sd(Cholesterol),
    ci = qt(0.975, df=n-1)*sd/sqrt(n))
  

print(desc_stats)


data$PriorSmoke <- as.factor(data$PriorSmoke)
fit <- aov(Cholesterol ~ PriorSmoke, data=data)
summary(fit)



# Fit a linear model first
fit <- aov(Cholesterol ~ PriorSmoke, data = data)

summary(fit)

ggplot(desc_stats,
       aes(x=PriorSmoke, y=mean, group=1)) +
  geom_point(size=3, color='red') +
  geom_line(linetype='dashed', color='darkgrey') +
  geom_errorbar(aes(ymin = mean-ci,
                    ymax = mean+ci),
                width=.1) +
  theme_bw() +
  labs(x = 'PriorSmoke',
       y = 'Cholesterol',
       title='Mean Plot with 95% Confidence Interval')



## Task 2

# Fit the linear regression model using the dummy variables for PriorSmoke
model_1 <- lm(Cholesterol ~ PriorSmoke_d2 + PriorSmoke_d3, data=data)

# Display the coefficient summary
summary(model_1)$coefficients

# Display the ANOVA table for the regression model
anova(model_1)



## Task 3


# Model 2 (ANCOVA Model)
fit_ancova <- lm(Cholesterol ~ Fat + PriorSmoke_d2 + PriorSmoke_d3, data=data)
summary(fit_ancova)

# Diagnostic plots
par(mfrow = c(2, 2))  # Set up the plotting area
plot(fit_ancova)  # Generate four basic diagnostic plots



# Leverage
hatvalues = hatvalues(fit_ancova)
# Threshold for high leverage points
hatvalues_threshold = 2*length(coef(fit_ancova))/length(fit_ancova$fitted.values)
high_leverage_points = which(hatvalues > hatvalues_threshold)

# Influence
cooksD = cooks.distance(fit_ancova)
# Threshold for influence using rule of thumb: 4/n
cooksD_threshold = 4/length(fit_ancova$fitted.values)
influential_points = which(cooksD > cooksD_threshold)

# Outliers: Studentized residuals
studentized_residuals = rstudent(fit_ancova)
# Outliers at the 5% significance level
outliers = which(abs(studentized_residuals) > qt(0.975, df.residual(fit_ancova)))

# Print results
print("High Leverage Points:")
print(high_leverage_points)
print("Influential Points:")
print(influential_points)
print("Outliers:")
print(outliers)

# Counting the number of high leverage points
num_high_leverage_points <- length(high_leverage_points)

# Counting the number of influential points
num_influential_points <- length(influential_points)

# Counting the number of outliers
num_outliers <- length(outliers)

# Printing the counts
cat("Number of High Leverage Points:", num_high_leverage_points, "\n")
cat("Number of Influential Points:", num_influential_points, "\n")
cat("Number of Outliers:", num_outliers, "\n")



## Task 4

# Predicted values from the ANCOVA model
predicted_values <- predict(fit_ancova)

# Scatterplot
library(ggplot2)
ggplot(data, aes(x=Fat, y=predicted_values, color=factor(PriorSmoke))) + 
  geom_point() +
  labs(title="Predicted Cholesterol vs. Fat", x="Fat", y="Predicted Cholesterol", color="PriorSmoke Group") +
  theme_minimal()



ggplot(data, aes(x=Fat, y=Cholesterol, color=factor(PriorSmoke))) + 
  geom_point() +
  labs(title="Actual Cholesterol vs. Fat", x="Fat", y="Actual Cholesterol", color="PriorSmoke Group") +
  theme_minimal()


## Task 5


# Creating New Product Variables:
data$Interaction1 <- data$PriorSmoke_d2 * data$Fat
data$Interaction2 <- data$PriorSmoke_d3 * data$Fat


# Building the Unequal Slopes Model (Model 3):

Model3 <- lm(Cholesterol ~ Fat + PriorSmoke_d2 + PriorSmoke_d3 + Interaction1 + Interaction2, data=data)
summary(Model3)


# Using base R:
plot(Model3, which=1:4)  # This will plot the first four diagnostic plots

# Influence Plot
influencePlot(Model3, main="Influence Plot", sub="Circle size is proportional to Cook's Distance")

# Outlier Statistics
standardized_residuals <- rstandard(Model3)
# View the standardized residuals
head(standardized_residuals)
# Highlight outliers
outliers <- which(abs(standardized_residuals) > 2)  
print(outliers)



## Task 6


# Obtain predicted values
predicted_cholesterol <- predict(Model3)

# Plotting predicted values against FAT
plot(data$Fat, predicted_cholesterol, xlab = "Fat (X)", ylab = "Predicted CHOLESTEROL (Y)", main = "Predicted Cholestrol vs. Fat")
abline(Model3, col="red") # Add regression line



## Task 7


# Values from the outputs
residual_standard_error_2 <- 93.33
df_2 <- 311
residual_standard_error_3 <- 92.54
df_3 <- 309

# Calculate RSS for both models
RSS_reduced <- residual_standard_error_2^2 * df_2
RSS_full <- residual_standard_error_3^2 * df_3

# Calculate the F-statistic
F_statistic <- ((RSS_reduced - RSS_full) / (df_2 - df_3)) / (RSS_full / df_3)

# Obtain the p-value for the F-statistic
p_value <- 1 - pf(F_statistic, df_2 - df_3, df_3)

F_statistic
p_value





## Task 8

# Formulate the full model with all main effects and interactions

full.model <- lm(Cholesterol ~ Smoke_d*Alcohol_d2*Alcohol_d3*Gender_d*PriorSmoke_d2*PriorSmoke_d3*Fat, data = data)

# Mixed model selection
selected.model <- step(full.model, direction="both")

summary(selected.model)














