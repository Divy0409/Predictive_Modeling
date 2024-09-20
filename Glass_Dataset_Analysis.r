# Load required libraries
library(mlbench)  # For dataset
library(ggplot2)  # For plotting
library(corrplot) # For correlation plots
library(e1071)    # For skewness calculation
library(patchwork) # For combining plots
library(caret)    # For data preprocessing

# Load the Glass dataset
data(Glass)

# Display the structure of the dataset
str(Glass)

# a) Explore the predictor variables

# Create histograms for each predictor variable
par(mfrow = c(3, 3))  # Arrange the plots in a 3x3 grid

# List of predictor variables
predictors <- names(Glass)[1:9]

# Loop through each predictor and plot the histogram with density line
for (var in predictors) {
  hist_data <- hist(Glass[[var]], 
                    main = paste("Histogram of", var), 
                    xlab = var, 
                    col = "lightblue", 
                    border = "black", 
                    probability = TRUE)  # Set probability = TRUE for density line
  
  mean_val <- mean(Glass[[var]], na.rm = TRUE)
  sd_val <- sd(Glass[[var]], na.rm = TRUE)
  
  curve(dnorm(x, mean = mean_val, sd = sd_val), 
        col = "red", 
        lwd = 2, 
        add = TRUE)
}

# Save correlation plot to PDF
pdf("corrplot_glass.pdf", width = 12, height = 10)  
corrplot(cor(dplyr::select_if(Glass, is.numeric), use = "na.or.complete"),
         method = 'number',
         type = 'lower',
         diag = FALSE,
         number.cex = 0.75,
         tl.cex = 0.5,
         col = colorRampPalette(c("#00008B", "#0000FF", "#4a1d01", "#FFFF00", "#FF0000", "#8B0000"))(200),  # Darker color palette
         cl.cex = 0.75)  # Adjust the color legend text size
dev.off()

# Boxplots to identify outliers
boxplots <- lapply(predictors, function(var) {
  ggplot(Glass, aes_string(y = var)) +
    geom_boxplot(fill = 'lightblue', color = 'black') +
    labs(title = paste("Boxplot of", var)) +
    theme_minimal()
})

# Arrange all boxplots in a 3x3 grid using patchwork
boxplot_grid <- wrap_plots(boxplots, nrow = 3, ncol = 3)
print(boxplot_grid)

# b) Check skewness and identify outliers

# Create a data frame to store the skewness values
skewness_values <- sapply(Glass[, 1:9], skewness)
skewness_df <- data.frame(Variable = names(skewness_values), Skewness = skewness_values)
print(skewness_df)

# Apply Box-Cox transformation only to specific variables (RI, Na, Si, and Ca)
variables_to_transform <- c("RI", "Na", "Si", "Ca")
trans_box <- preProcess(Glass[, variables_to_transform], method = c("BoxCox"))
transformed <- predict(trans_box, Glass[, variables_to_transform])

# Combine the transformed variables with the original dataset
Glass_transformed <- Glass  # Create a copy of the original data
Glass_transformed[, variables_to_transform] <- transformed  # Replace only the transformed variables

# Apply log transformation only to K, Ca, Ba, and Fe
variables_to_log_transform <- c("K", "Ba", "Fe", "Al")

# Apply log transformation (adding a small constant to avoid log(0) issue)
Glass_transformed[, variables_to_log_transform] <- log(Glass[, variables_to_log_transform] + 1)

# Apply square root transformation only on Mg
Glass_transformed$Mg <- exp(Glass$Mg)

# Check skewness after transformations (Box-Cox and log)
skewness_values_after_transformation <- sapply(Glass_transformed[, 1:9], skewness)  # Check all variables
print(skewness_values_after_transformation)

# Create histograms for each predictor variable after transformation
par(mfrow = c(3, 3))  # Arrange the plots in a 3x3 grid

# List of predictor variables after transformation
predictors_transformed <- names(Glass_transformed)[1:9]

# Loop through each predictor and plot the histogram with density line
for (var in predictors_transformed) {
  hist_data <- hist(Glass_transformed[[var]], 
                    main = paste("Histogram of", var, "(Transformed)"), 
                    xlab = var, 
                    col = "lightblue", 
                    border = "black", 
                    probability = TRUE)  # Set probability = TRUE for density line
  
  mean_val <- mean(Glass_transformed[[var]], na.rm = TRUE)
  sd_val <- sd(Glass_transformed[[var]], na.rm = TRUE)
  
  curve(dnorm(x, mean = mean_val, sd = sd_val), 
        col = "red", 
        lwd = 2, 
        add = TRUE)
}

# c) Remove outliers using IQR

# Function to remove outliers using IQR
remove_outliers_iqr <- function(data) {
  data_no_outliers <- data  # Make a copy of the dataset
  
  # Loop through each numeric variable
  for (var in names(data_no_outliers)) {
    if (is.numeric(data_no_outliers[[var]])) {
      # Calculate Q1 (25th percentile) and Q3 (75th percentile)
      Q1 <- quantile(data_no_outliers[[var]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data_no_outliers[[var]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      # Define outlier bounds
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      
      # Identify and remove outliers
      data_no_outliers <- data_no_outliers[data_no_outliers[[var]] >= lower_bound & data_no_outliers[[var]] <= upper_bound, ]
    }
  }
  
  return(data_no_outliers)
}

# Apply the outlier removal function
Glass_no_outliers_iqr <- remove_outliers_iqr(Glass_transformed)

# Check the number of rows after removing outliers
cat("Number of rows after outlier removal using IQR:", nrow(Glass_no_outliers_iqr), "\n")

# Output the first few rows of the dataset after removing outliers
print(head(Glass_no_outliers_iqr))

# Boxplots to identify outliers after IQR-based outlier removal
boxplots_no_outliers <- lapply(predictors, function(var) {
  ggplot(Glass_no_outliers_iqr, aes_string(y = var)) +
    geom_boxplot(fill = 'lightblue', color = 'black') +
    labs(title = paste("Boxplot of No Outliers (IQR)", var)) +
    theme_minimal()
})

# Arrange all boxplots in a 3x3 grid using patchwork
boxplot_grid_no_outliers <- wrap_plots(boxplots_no_outliers, nrow = 3, ncol = 3)
print(boxplot_grid_no_outliers)
