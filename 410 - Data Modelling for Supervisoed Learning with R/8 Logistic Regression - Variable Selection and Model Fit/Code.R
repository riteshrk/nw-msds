'install.packages("knitr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("naniar")
install.packages("corrplot")
install.packages("dplyr")
install.packages("e1071")
install.packages("car")
install.packages("MASS")'



library(e1071)
library(MASS)
library(car)
library(e1071)
library(ggplot2)
library(gridExtra)
library(readxl)
library(knitr)
library(corrplot)
library(naniar)
library(dplyr)
library(patchwork)
library(caret)
library(pROC)



wine <- read_excel("wine.xlsx")
View(wine)

str(wine)

wine <- subset(wine, select = -INDEX)

dim(wine)

wine$Cases <- NULL



# Identify Continuous and Discrete variables
continuous_vars <- c()
discrete_vars <- c()

# Iterate over each column in the dataframe
for (col in names(wine)) {
  # Check if the column is numeric (which includes integer and double types)
  if (is.numeric(wine[[col]])) {
    # If the unique number of values in a numeric column is above a certain threshold,
    # consider it continuous. Otherwise, it's discrete.
    if (length(unique(wine[[col]])) > 5) {  
      continuous_vars <- c(continuous_vars, col)
    } else {
      discrete_vars <- c(discrete_vars, col)
    }
  } else {
    # Non-numeric columns are considered discrete
    discrete_vars <- c(discrete_vars, col)
  }
}


# Print the continuous and discrete variables
print("Continuous Variables:")
print(continuous_vars)

print("Discrete Variables:")
print(discrete_vars)





######### Discrete Variables


# Loop through each discrete variable and print the count of each level
for (var in discrete_vars) {
  cat("Counts for", var, ":\n")
  print(table(wine[[var]]))
  cat("\n\n") # Adding extra space between outputs for different variables
}





# Histograms for discrete variables

# Create a list to store the plots
plot_list <- list()

# Loop through each discrete variable and create a histogram with data labels
for (i in 1:length(discrete_vars)) {
  var <- discrete_vars[i]
  # Define a set of colors
  colors <- c("azure3", "lightsteelblue", "lightblue2", "lightskyblue2")
  
  p <- ggplot(wine, aes(x = !!sym(var), fill = !!sym(var))) +
    geom_bar(stat = "count", color = "black", fill = colors[i]) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
    labs(title = var, x = var, y = "Count") +
    theme_minimal()
  plot_list[[var]] <- p
}

# Arrange the plots in one panel
grid.arrange(grobs = plot_list, ncol = 3, nrow = 1)

discrete_vars <- discrete_vars[discrete_vars != "Purchase"]


######### Continuous Variables

# Assuming groups is defined as before
groups <- split(continuous_vars, ceiling(seq_along(continuous_vars)/4))

for (i in seq_along(groups)) {
  # Extract the subset of the dataframe for the current group
  subset_df <- wine[, groups[[i]], drop = FALSE]
  
  # Calculate the standard deviation, handling NA values
  std_dev <- sapply(subset_df, function(x) sd(x, na.rm = TRUE))
  
  # Get the basic summary
  basic_summary <- summary(subset_df)
  
  # Combine the basic summary with standard deviation
  enhanced_summary <- rbind(basic_summary, Std.Dev = round(std_dev,4))
  
  # Create and print the table
  table_html <- kable(enhanced_summary, caption = paste("Summary of Continuous Variables"))
  print(table_html, viewer = TRUE)
  cat("\n\n")
}



# Plot Histograms


# Function to create histograms for a group of variables
create_histograms <- function(variables, colors) {
  plot_list <- list()
  for (i in seq_along(variables)) {
    var <- variables[i]
    color <- colors[i]
    
    # Dynamically set binwidth based on the range of the variable
    range_var <- max(wine[[var]], na.rm = TRUE) - min(wine[[var]], na.rm = TRUE)
    binwidth_var <- range_var / 30  # Set number of bins to around 30
    
    p <- ggplot(wine, aes_string(x = var)) +
      geom_histogram(binwidth = binwidth_var, fill = color) +
      labs(title = var, x = var, y = "Count") +
      theme_minimal()
    plot_list[[var]] <- p
  }
  return(plot_list)
}


# Loop through each group and create a panel of histograms
for (i in seq_along(groups)) {
  plot_list <- create_histograms(groups[[i]], colors)
  grid.arrange(grobs = plot_list, ncol = 2, nrow = 2)
}





################################################################
# Task 1 c

numeric_wine <- wine[sapply(wine, is.numeric)]
correlation_matrix <- cor(numeric_wine, use = "complete.obs")


corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")


################################################################
# Task 1 d


# Calculate the number of missing values for each column
missing_values <- sapply(wine, function(x) sum(is.na(x)))

# Print the table of missing values
print(missing_values)


# Visualize missing data pattern
vis_miss(wine)


# Replace NA values with median for all columns
wine <- wine %>%
  mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))


# Calculate the number of missing values for each column
missing_values <- sapply(wine, function(x) sum(is.na(x)))

# Print the table of missing values
print(missing_values)




################################################################
# Task 1 e


# Melt the data frame to long format for ggplot
wine_long <- reshape2::melt(wine, measure.vars = continuous_vars)

# Plot the boxplots
ggplot(wine_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(outlier.color = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Variables", y = "Value", fill = "Variable") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal()



# Initialize an empty list to store the outlier counts
outlier_counts <- list()

# Loop through each continuous variable to calculate the number of outliers
for (var in continuous_vars) {
  # Calculate Q1, Q3, and IQR
  Q1 <- quantile(wine[[var]], 0.25, na.rm = TRUE)
  Q3 <- quantile(wine[[var]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  # Define the upper and lower bounds for outliers
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Count the outliers
  outliers <- sum(wine[[var]] < lower_bound | wine[[var]] > upper_bound, na.rm = TRUE)
  
  # Store the count in the list with the variable name as the key
  outlier_counts[[var]] <- outliers
}

# Convert the list to a named vector for easier reading
outlier_counts <- unlist(outlier_counts)

# Print the counts
print(outlier_counts)


################################################################
# Task 1 f


# Assuming continuous_vars is already defined
skewness_values <- sapply(wine[continuous_vars], skewness)

# Print the skewness values
print(skewness_values)

# Corrleation with AcidIndex
cor(wine$AcidIndex, wine$Purchase)


# Transform AcidIndex using log transformation
# Add 1 to avoid log(0) which is undefined
wine$AcidIndex_log = log(wine$AcidIndex + 1)

# Compute the correlation with Purchase
cor(wine$AcidIndex_log, wine$Purchase)

wine <- subset(wine, select = -AcidIndex_log)

# Check the VIF
model <- lm(Purchase ~ ., data = wine)

vif(model)
# Bar Graphs for Categorical Variables with Improved Data Labels
par(mfrow = c(3, 1)) # Adjust layout based on the number of variables

##################################


# Create a list to store ggplot objects
plot_list <- list()

# Loop through each continuous variable and create a ggplot object
for (var in continuous_vars) {
  plot <- ggplot(wine, aes_string(x = var, y = "Purchase")) +
    geom_jitter(alpha = 0.5, color = "blue", size = 0.5) +
    labs(title = paste("Scatter plot of", var, "vs Purchase"),
         x = var,
         y = "Purchase") +
    theme_minimal()
  
  plot_list[[var]] <- plot
}

# Arrange the plots into 5 panels with 3 plots each (the last panel will have only 1 plot)
panels <- list()
panels[[1]] <- grid.arrange(grobs = plot_list[1:3], ncol = 3)
panels[[2]] <- grid.arrange(grobs = plot_list[4:6], ncol = 3)
panels[[3]] <- grid.arrange(grobs = plot_list[7:9], ncol = 3)
panels[[4]] <- grid.arrange(grobs = plot_list[10:12], ncol = 3)
panels[[5]] <- grid.arrange(grobs = plot_list[13], ncol = 1)

# Print the panels
for (panel in panels) {
  print(panel)
}


##################################

skewness_values <- apply(wine[ , continuous_vars], 2, skewness)

# Print the skewness values
print(skewness_values)


##################################





# Your wine dataframe
# wine <- ... # make sure your wine dataframe is loaded

# Ensure AcidIndex has no negative or zero values for certain transformations
if (any(wine$AcidIndex <= 0)) {
  wine$AcidIndex <- wine$AcidIndex - min(wine$AcidIndex) + 1
}

# Initialize variables to store the best transformation results
best_skewness <- Inf
best_transformation <- NA
best_transformed_data <- NA

# List of transformations to try
transformations <- list(
  "original" = function(x) x,
  "log" = log,
  "sqrt" = sqrt,
  "inverse" = function(x) 1 / x,
  "square" = function(x) x^2
)

# Loop through transformations
for (trans_name in names(transformations)) {
  # Apply transformation
  transformed_data <- transformations[[trans_name]](wine$AcidIndex)
  
  # Calculate skewness
  current_skewness <- skewness(transformed_data)
  
  # Check if this is the best (lowest) skewness so far
  if (current_skewness < best_skewness) {
    best_skewness <- current_skewness
    best_transformation <- trans_name
    best_transformed_data <- transformed_data
  }
}

# Print the best transformation result
cat("Best Transformation: ", best_transformation, "\n",
    "Skewness Before: ", skewness(wine$AcidIndex), "\n",
    "Skewness After: ", best_skewness, "\n")

# Add the best transformed data as a new column in the wine dataframe
wine$TransformedAcidIndex <- best_transformed_data


###########################################################

# Contingency Tables for Discrete Variables

library(ggplot2)
library(reshape2)


# Loop through each discrete variable and create a contingency table and heatmap
for (var in discrete_vars) {
  # Create contingency table
  contingency_table <- table(wine[[var]], wine$Purchase)
  
  # Convert the table to a data frame for plotting
  heatmap_data <- as.data.frame(melt(contingency_table))
  
  # Creating the heatmap with data labels
  p <- ggplot(heatmap_data, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = value), vjust = 1.5, color = "red") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = paste("Heatmap of", var, "vs Purchase"), x = var, y = "Purchase") +
    theme_minimal()
  
  # Display the plot
  print(p)
}

################################################################

# Loop through each variable and perform a Chi-squared test
for (var in discrete_vars) {
  # Create contingency table
  contingency_table <- table(wine[[var]], wine$Purchase)
  
  # Perform Chi-squared test
  chi_squared_test <- chisq.test(contingency_table)
  
  # Print the results
  cat("Results for", var, ":\n")
  print(chi_squared_test)
  cat("\n")  # Add a new line for readability
}

#################################################################

# Manually create dummy variables for each level of STARS
wine$Stars_d1 <- ifelse(wine$STARS == 1, 1, 0)
wine$Stars_d2 <- ifelse(wine$STARS == 2, 1, 0)
wine$Stars_d3 <- ifelse(wine$STARS == 3, 1, 0)
wine$Stars_d4 <- ifelse(wine$STARS == 4, 1, 0)

# Print the first 10 rows of the selected columns
head(wine[c("STARS", "Stars_d1", "Stars_d2", "Stars_d3", "Stars_d4")], 10)


##################################################################################
##################################################################################

# Task 2


# Calculate the number of missing values for each column
missing_values <- sapply(wine, function(x) sum(is.na(x)))

# Print the table of missing values
print(missing_values)
graphics.off()



##################################################################################
# LR Model without Transformation

# Fit the initial full model with all predictors
full_model <- glm(Purchase ~ FixedAcidity + VolatileAcidity + CitricAcid + 
                    ResidualSugar + Chlorides + FreeSulfurDioxide + 
                    TotalSulfurDioxide + Density + pH + Sulphates + 
                    Alcohol + AcidIndex + Stars_d1 + Stars_d2 + 
                    Stars_d3 + Stars_d4, data = wine, family = binomial)

# Perform stepwise selection using both directions (forward and backward)
stepwise_model <- stepAIC(full_model, direction = "both", trace = FALSE)


# Print the anova of the stepwise model
anova(stepwise_model)

# Print the summary of the stepwise model
summary(stepwise_model)


pretrans = predict(stepwise_model, type=c("response"))

roccurve = roc(Purchase ~ pretrans, data=wine)

plot(roccurve)

auc(roccurve)

#########################################################################
## LR Model with Transformation

# Fit the initial full model with all predictors
full_model_1 <- glm(Purchase ~ FixedAcidity + VolatileAcidity + CitricAcid + 
                    ResidualSugar + Chlorides + FreeSulfurDioxide + 
                    TotalSulfurDioxide + Density + pH + Sulphates + 
                    Alcohol + TransformedAcidIndex + Stars_d1 + Stars_d2 + 
                    Stars_d3 + Stars_d4, data = wine, family = binomial)

# Perform stepwise selection using both directions (forward and backward)
stepwise_model_1 <- stepAIC(full_model_1, direction = "both", trace = FALSE)


# Print the anova of the stepwise model
anova(stepwise_model_1)

# Print the summary of the stepwise model
summary(stepwise_model_1)


posttrans = predict(stepwise_model_1, type=c("response"))

roccurve = roc(Purchase ~ posttrans, data=wine)

plot(roccurve)

auc(roccurve)



###############################################################

# Comparing the two models

# Comparing the two models

# Calculation of the Chi-square statistic
logLik_model_1 = logLik(stepwise_model_1)
logLik_model = logLik(stepwise_model)
chisquare = -2 * (logLik_model_1 - logLik_model)

# Print the Chi-square statistic
print(chisquare)

# Calculation of the critical Chi-square value
# Assuming a significance level of 0.05 and 2 degrees of freedom
critical_chi = qchisq(0.05, 1, ncp=0, lower.tail=FALSE)

# Print the critical Chi-square value
print(critical_chi)
