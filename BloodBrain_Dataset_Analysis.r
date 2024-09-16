# Load required libraries
library(caret)
library(corrplot)  # For visualizing correlations

# Load the BloodBrain data
data(BloodBrain)
df <- bbbDescr  # Predictor variables
outcome <- logBBB  # Outcome variable

# Step 1: Initial inspection of the data
str(df)  # Display the structure of the predictor data frame
summary(outcome)  # Summary statistics for the outcome variable

# Step 2: Compute correlations among predictors
cor_matrix <- cor(df, use = "pairwise.complete.obs")

# Visualize the correlation matrix
corrplot(cor_matrix, method = "color", tl.cex = 0.7, addCoef.col = "black", 
         order = "hclust", title = "Correlation Matrix of Predictors")

# Identify highly correlated variables (above a threshold of 0.9)
high_cor <- findCorrelation(cor_matrix, cutoff = 0.9)
length(high_cor)  # Number of highly correlated variables
high_cor_vars <- colnames(df)[high_cor]  # Names of highly correlated variables
print(high_cor_vars)  # List of highly correlated variables

# Step 3: Reduce the predictor set by removing highly correlated variables
df_reduced <- df[, -high_cor]  # Remove highly correlated variables

# Step 4: Assess the effect on the number of predictors
original_count <- ncol(df)  # Number of original predictors
reduced_count <- ncol(df_reduced)  # Number of predictors after reduction

cat("Number of original predictors:", original_count, "\n")
cat("Number of predictors after reducing correlations:", reduced_count, "\n")

# Optional: Apply PCA to further reduce dimensionality
pca_result <- prcomp(df_reduced, scale. = TRUE)

# Summary of PCA
summary(pca_result)

