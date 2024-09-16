# Load required libraries
library(mlbench)
library(ggplot2)
library(corrplot)
library(e1071)
# library(patchwork)

# Load the Glass dataset
data(Glass)

# Create histograms for each predictor variable
par(mfrow = c(3, 3))  # Arrange the plots in a 3x3 grid

# List of predictor variables
predictors <- names(Glass)[1:9]

# Loop through each predictor and plot the histogram with density line
for (var in predictors) {
  # Create histogram
  hist_data <- hist(Glass[[var]], 
                    main = paste("Histogram of", var), 
                    xlab = var, 
                    col = "lightblue", 
                    border = "black", 
                    probability = TRUE)  # Set probability = TRUE for density line
  
  # Calculate mean and standard deviation
  mean_val <- mean(Glass[[var]], na.rm = TRUE)
  sd_val <- sd(Glass[[var]], na.rm = TRUE)
  
  # Add normal distribution line (bell curve)
  curve(dnorm(x, mean = mean_val, sd = sd_val), 
        col = "red", 
        lwd = 2, 
        add = TRUE)
}

corrplot(cor(dplyr::select_if(Glass, is.numeric), use = "na.or.complete"),
         method = 'number',
         type = 'lower',
         diag = FALSE,
         number.cex = 0.75,
         tl.cex = 0.5)


# Load required library for skewness calculation
library(e1071)

# Create a data frame to store the skewness values
skewness_values <- sapply(Glass[, 1:9], skewness)
skewness_df <- data.frame(Variable = names(skewness_values), Skewness = skewness_values)

# Display the skewness values
print(skewness_df)

df <- Glass
# Boxplots to identify outliers
boxplots <- lapply(predictors, function(var) {
  ggplot(df, aes_string(y = var)) +
    geom_boxplot(fill = 'lightblue', color = 'black') +
    labs(title = paste("Boxplot of", var)) +
    theme_minimal()
})

# Arrange all boxplots in a 3x3 grid using patchwork
boxplot_grid <- wrap_plots(boxplots, nrow = 3, ncol = 3)

# Display the grid of boxplots
print(boxplot_grid)


