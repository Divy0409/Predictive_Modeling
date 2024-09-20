# Load required libraries
library(mlbench)  # For dataset
library(ggplot2)  # For plotting
library(corrplot) # For correlation plots
library(e1071)    # For skewness calculation
library(patchwork) # For combining plots
library(caret)    # For data preprocessing

# Load the Glass dataset
data(Glass)

# a) Visualizations and Data Exploration
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


# b) Correlation Analysis to find potential outliers and Skewness calculation
# Correlation plot
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

skewness_values <- apply(Glass[, 1:9], 2, skewness)
print("Skewness values for predictor variables:")
print(skewness_values)


# c) Transformation and Outlier Detection

# Apply Box-Cox transformation (excluding the class variable)
trans_boxcox <- preProcess(Glass[, -10], method = c("center", "scale", "BoxCox"))
transformed <- predict(trans_boxcox, Glass[, -10])

# Calculate skewness after transformation
skewness_values_after_box <- apply(transformed, 2, skewness)
print("Skewness after Box-Cox transformation:")
print(skewness_values_after_box)

# Spatial Sign transformation to remove outliers
transformed_spatial <- spatialSign(transformed)

# Skewness after spatial sign transformation
skewness_values_after_spatial <- apply(transformed_spatial, 2, skewness)
print("Skewness after Spatial Sign transformation:")
print(skewness_values_after_spatial)

# Create histograms for each predictor variable after spatial transformation
pdf("Glass_Dataset_Analysis_2.pdf", width = 12, height = 10)
# plot histograms for all the predictor variables 
par(mfrow = c(3, 3), mar = c(2, 2, 2, 2))

for (i in 1:9) {
  hist(transformed_spatial[, i], 
       main = paste("After Transformation Histogram of", colnames(transformed_spatial)[i]), 
       xlab = colnames(transformed_spatial)[i], 
       col = "lightblue", 
       border = "black", 
       prob = TRUE)
  
  curve(dnorm(x, mean = mean(transformed_spatial[, i], na.rm = TRUE), 
              sd = sd(transformed_spatial[, i], na.rm = TRUE)), 
        col = "darkblue", 
        lwd = 2, 
        add = TRUE)
}


dev.off()


# Boxplots to identify outliers after spatial transformation
# Convert transformed_sp to a data frame
transformed_spatial_df <- as.data.frame(transformed_spatial)

# Boxplots to identify outliers after spatial transformation
pdf("boxplots_after_sp.pdf", width = 12, height = 10)

predictors_transformed <- names(transformed_spatial_df)[1:9]

# Create boxplots
boxplots_after_sp <- lapply(predictors_transformed, function(var) {
  # Check for NA values
  if (any(is.na(transformed_spatial_df[[var]]))) {
    message(paste("Warning: NA values found in", var))
  }
  
  # Create the boxplot
  ggplot(transformed_spatial_df, aes_string(y = var)) +
    geom_boxplot(fill = 'lightblue', color = 'black') +
    labs(title = paste("Boxplot of", var, "(After Spatial Transformation)")) +
    theme_minimal()
})

# Arrange all boxplots in a 3x3 grid using patchwork
boxplot_grid_after_sp <- wrap_plots(boxplots_after_sp, nrow = 3, ncol = 3)
print(boxplot_grid_after_sp)

dev.off()

