'install.packages("ggplot2")
install.packages("gridExtra")
install.packages("dplyr")'


library(readxl)
library(ggplot2)
library(gridExtra)
library(dplyr)

# Explore relationship of STA with continuous variables
# Read and Explore Data

icu <- read_excel("icu.xlsx")

# View the dataset
View(icu)

# Structure of the dataset
cat("Structure of the dataset:\n")
str(icu)

# Dimensions
cat("Dimensions of the dataset: ", dim(icu), "\n")

# Print the structure of the file
str(icu)

# Print the fist 6 rows of the dataset
head(icu)


# Print the names of the columns
names(icu)

# Get the names of the columns with missing values
columns_with_missing <- names(icu)[colSums(is.na(icu)) > 0]
print(columns_with_missing)

# Summary statistics
summary(icu)


# Identify column types
continuous_cols <- names(icu)[sapply(icu, function(col) is.numeric(col) && length(unique(col)) > 10)]
discrete_cols <- names(icu)[sapply(icu, function(col) is.numeric(col) && length(unique(col)) <= 10)]


print(paste("Continuous columns:", paste(continuous_cols, collapse = ", ")))
print(paste("Discrete columns:", paste(discrete_cols, collapse = ", ")))


# Remove ID from the continuous columns
continuous_cols <- setdiff(continuous_cols, "ID")


# Remove STA from the continuous columns
discrete_cols <- setdiff(discrete_cols, "STA")




################################################################################
# Explore relationship of STA with discrete variables


# Function to plot a Contingency Table with Heatmap
plot_contingency_heatmap <- function(var) {
  
  # Create a contingency table
  contingency_table <- table(icu$STA, icu[[var]])
  
  # Convert the contingency table into a data frame for plotting
  plot_data <- as.data.frame(as.table(contingency_table))
  
  # Create the heatmap plot
  p <- ggplot(plot_data, aes_string(x = "Var1", y = "Var2", fill = "Freq")) +
    geom_tile() +
    geom_text(aes(label = sprintf("%d", Freq)), vjust = 1.5) +
    scale_fill_gradient(low = "skyblue", high = "darkblue") +
    labs(title = paste("Contingency Table of", var, "and STA"),
         x = "STA",
         y = var,
         fill = "Count") +
    theme_minimal()
  
  return(p)
}

# List to hold the plots
plots_list <- lapply(discrete_cols, plot_contingency_heatmap)

# Group every 6 plots into separate lists for panels
plot_groups <- split(plots_list, ceiling(seq_along(plots_list)/6))

# Display each panel
for(panel in plot_groups) {
  do.call(grid.arrange, c(panel, ncol = 2))
}




################################################################################
# Explore relationship of STA with continuous variables

# Loop through continuous columns and plot histograms
for(var in continuous_cols){
  p <- ggplot(icu, aes_string(x = var)) + 
    geom_histogram(binwidth=10, fill="skyblue", color="black", alpha=0.7) + # adjust binwidth as needed
    facet_wrap(~STA) + 
    labs(title = paste("Histogram of", var, "grouped by STA"), x = var, y = "Count") +
    theme_minimal()
  print(p)
}



################################################################################

# Count levels for each discrete variable
level_counts <- lapply(discrete_cols, function(var) {
  data_frame(Variable = var, 
             CountOfLevels = length(unique(icu[[var]])))
})

# Bind all data frames together
level_counts_df <- bind_rows(level_counts)

# Print the result
print(level_counts_df)


# Loop through discrete columns and print the frequency of each level
for(col in discrete_cols) {
  cat("Frequency for", col, ":\n")
  print(table(icu[[col]]))
  cat("\n")
}



################################################################################
# Drop records with number of records < 10 for any level of a discrete variable
# Function to check if any level has less than 10 records
drop_rows <- function(data, col) {
  tbl <- table(data[[col]])
  drop_levels <- names(tbl[tbl < 10])
  if (length(drop_levels) > 0) {
    return(!(data[[col]] %in% drop_levels))
  }
  return(rep(TRUE, nrow(data)))
}

# Apply the function for each discrete column
for (col in discrete_cols) {
  icu <- icu[drop_rows(icu, col), ]
}

dim(icu)

################################################################################

## Task 2


# Create the contingency table
table_sex_sta <- as.data.frame(table(icu$SEX, icu$STA))
names(table_sex_sta) <- c("SEX", "STA", "Count")

# Plot the heatmap
p <- ggplot(table_sex_sta, aes(x = SEX, y = STA, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), vjust = 1) +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  labs(title = "Heatmap of SEX vs. STA", x = "Gender", y = "Status") +
  theme_minimal()

print(p)

################################################################################

## Task 3

# Create the contingency table
table_typ_sta <- as.data.frame(table(icu$TYP, icu$STA))
names(table_typ_sta) <- c("TYP", "STA", "Count")

# Plot the heatmap
p <- ggplot(table_typ_sta, aes(x = TYP, y = STA, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count), vjust = 1) +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  labs(title = "Heatmap of TYP vs. STA", x = "Admission Type", y = "Status") +
  theme_minimal()

print(p)

################################################################################

## Task 4a

# Fitting the logistic regression model
model <- glm(STA ~ AGE, data=icu, family=binomial)

# Displaying the summary of the model to see the coefficients and statistics
summary(model)



################################################################################

## Task 4b

# Load the ggplot2 package
library(ggplot2)

# Create a scatterplot
scatter_plot <- ggplot(icu, aes(x = AGE, y = STA)) +
  geom_point(alpha = 0.6) +           # alpha is used for point transparency
  labs(x = "Age", y = "Vital Status (STA)") +
  theme_minimal() +
  scale_y_continuous(labels = c("Lived", "Died"), breaks = c(0, 1))

# Display the plot
print(scatter_plot)


################################################################################

## Task 4c

# Creating AGE_CAT variable
icu <- icu %>%
  mutate(AGE_CAT = case_when(
    AGE >= 15 & AGE <= 24 ~ 1,
    AGE >= 25 & AGE <= 34 ~ 2,
    AGE >= 35 & AGE <= 44 ~ 3,
    AGE >= 45 & AGE <= 54 ~ 4,
    AGE >= 55 & AGE <= 64 ~ 5,
    AGE >= 65 & AGE <= 74 ~ 6,
    AGE >= 75 & AGE <= 84 ~ 7,
    AGE >= 85 & AGE <= 94 ~ 8,
    AGE >= 95         ~ 9
  ))

# Compute STA mean for each age category
age_summary <- icu %>%
  group_by(AGE_CAT) %>%
  summarize(STA_mean = mean(STA, na.rm = TRUE))

# Plotting
ggplot(age_summary, aes(x = as.factor(AGE_CAT), y = STA_mean)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  geom_text(aes(label = round(STA_mean, 2)), vjust = -0.5, size = 3.5) +
  labs(x = "AGE Category", y = "STA Mean") +
  theme_minimal()

################################################################################

## Task 4f

summary(model)
BIC(model)


################################################################################

## Task 4g

# Computing tghe Logit Values
logit_values <- predict(model, type = "link")

# Saving the values in the dataframe
icu$logit_values <- predict(model, type = "link")


# Create a scatterplot
ggplot(icu, aes(x = AGE, y = logit_values)) + 
  geom_point() + 
  labs(x = "AGE", y = "Predicted Logit Values", 
       title = "Scatterplot of Predicted Logits by AGE") + 
  theme_minimal()



################################################################################

## Task 4h


# Compute the probabilities from the logits
icu$probability_death <- exp(icu$logit_values) / (1 + exp(icu$logit_values))

# Scatterplot of the predicted probabilities by AGE
ggplot(icu, aes(x = AGE, y = probability_death)) + 
  geom_point() + 
  labs(x = "AGE", y = "Predicted Probability of Death", 
       title = "Scatterplot of Predicted Probabilities of Death by AGE") + 
  theme_minimal()


# Adding the origianl STA values to the plot

ggplot(icu, aes(x = AGE, y = probability_death)) + 
  # Predicted probability of death
  geom_point(aes(y = probability_death)) +
  
  # Raw data: Jitter is added for the STA values to better visualize overlapping points
  geom_jitter(aes(y = STA), color = "red", position = position_jitter(width = 0, height = 0.05), alpha = 0.5) +
  
  labs(x = "AGE", y = "Predicted Probability of Death", title = "Overlay of Raw Data (STA) and Predicted Probabilities of Death") +
  scale_y_continuous(
    "Predicted Probability of Death",
    sec.axis = sec_axis(~., name = "STA (1: Death, 0: Survival)", breaks = c(0,1))
  ) +
  theme_minimal()


################################################################################

## Task 4i

# Assuming the logistic regression model is named 'logit_model'
# Fit a logistic regression model (if you haven't done so already)
logit_model <- glm(STA ~ AGE, data=icu, family=binomial)

# Predict probability of death for someone 35 years old
new_data <- data.frame(AGE = 35)
predicted_prob <- predict(logit_model, newdata=new_data, type="response")
prob_survival = 1 - predicted_prob

cat("Probablity of Survival:", prob_survival)

