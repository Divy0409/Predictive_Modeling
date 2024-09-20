# Load required libraries
library(caret)  # For data preprocessing and correlation analysis
library(corrplot)  # For visualizing correlation matrices

# (a) Load the BloodBrain dataset
data(BloodBrain)  # Load the dataset from the caret package
?BloodBrain # Display information about the dataset
df <- bbbDescr  # Extract predictor variables
outcome <- logBBB  # Extract the numeric outcome variable

# (b) Step 1: Initial inspection of the data
str(df)  # Display the structure of the predictor data frame to understand its composition
summary(outcome)  # Summary statistics for the outcome variable to understand its distribution

# (b) Step 2: Compute correlations among predictors
cor_matrix <- cor(df)  # Calculate the correlation matrix of predictor variables

# (b) Visualize the correlation matrix
pdf("BBN_CRR.pdf", width = 12, height = 6)  # Open a PDF device to save the plot
corrplot(cor_matrix, 
         order = "hclust",  # Order the correlation matrix by hierarchical clustering
         title = "Correlation Matrix")  # Set the title for the plot
dev.off()  # Close the PDF device

# (b) Identify highly correlated variables (correlation > 0.9)
high_cor <- findCorrelation(cor_matrix, cutoff = 0.75)  # Find indices of variables with high correlation
length(high_cor)  # Number of highly correlated variables
high_cor_vars <- colnames(df)[high_cor]  # Get names of highly correlated variables
print(high_cor_vars)  # Print the names of highly correlated variables

# (b) Step 3: Reduce the predictor set by removing highly correlated variables
df_reduced <- df[, -high_cor]  # Remove columns with high correlation from the predictor set

# (b) Step 4: Assess the effect on the number of predictors
original_count <- ncol(df)  # Number of predictors before reduction
reduced_count <- ncol(df_reduced)  # Number of predictors after reduction

print("Number of original predictors:")
print(original_count)  # Print the original number of predictors
print("Number of predictors after reducing correlations:")  
print(reduced_count)# Print the number after reduction

# (b) Compute correlation matrix for the reduced predictor set
cor_matrix_reduced <- cor(df_reduced)  # Calculate the correlation matrix of the reduced predictor set

# (b) Visualize the reduced correlation matrix
pdf("BBN_CRR_RE.pdf", width = 12, height = 6)  # Open a new PDF device to save the plot
corrplot(cor_matrix_reduced, 
         order = "hclust",  # Order the correlation matrix by hierarchical clustering
         title = "Correlation Matrix of Predictors (Reduced)")  # Set the title for the reduced matrix plot
dev.off()  # Close the PDF device
