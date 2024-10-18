# Load required libraries
library(mlbench)  # For loading the Soybean dataset
library(caret)    # For data preprocessing and near-zero variance identification
library(ggplot2)  # For plotting
library(VIM)      # For visualizing missing data
library(dplyr)    # For data manipulation

# Load the Soybean dataset
data(Soybean)  # Load the dataset from the mlbench package
head(Soybean)  # Display the first few rows of the dataset

# (a) Investigate the frequency distributions for the categorical predictors
# Identify categorical variables
categorical_vars <- sapply(Soybean, is.factor)  # Determine which variables are factors
categorical_data <- Soybean[, categorical_vars]  # Subset data to include only categorical predictors

# Save plots of categorical distributions to a PDF file
pdf("categorical_distributions.pdf", width = 12, height = 10)  # Open a PDF device for plotting

# Adjust layout based on the number of categorical variables
par(mfrow = c(6, 7))  

# Create bar plots for each categorical variable
for (var in names(categorical_data)) {
  counts <- table(categorical_data[[var]])  # Get frequency counts for the variable
  barplot(counts, main = var, col = "lightblue", las = 2, cex.names = 0.6)  # Plot the frequencies
}

# Close the PDF device
dev.off()

# (b) Investigate missing data
# Identify near-zero variance predictors
zero_cols <- nearZeroVar(Soybean)  # Find predictors with near-zero variance
zero_cols  # Print indices of predictors with near-zero variance
colnames(Soybean)[zero_cols]  # Print names of predictors with near-zero variance

# Count missing values per predictor
missing_data <- colSums(is.na(Soybean)) / nrow(Soybean)  # Calculate missing proportion for each predictor
missing_data_summary <- data.frame(Predictor = names(missing_data), MissingProportion = missing_data)  # Create a summary data frame

print(missing_data_summary)  # Print the missing data summary

# Check if missing data is related to classes
missing_data_by_class <- Soybean %>%
  mutate(nul = rowSums(is.na(Soybean))) %>%  # Calculate number of missing values per row
  group_by(Class) %>%  # Group by class
  summarize(miss = sum(nul)) %>%  # Summarize total missing values per class
  filter(miss != 0)  # Filter to show classes with missing values

missing_data_by_class  # Print the missing data by class

# Save missing data proportion plot to a PDF file
pdf("missing_data_proportion.pdf", width = 12, height = 6)  # Open a PDF device for plotting

# Create a bar plot of missing data proportions
ggplot(missing_data_summary, aes(x = Predictor, y = MissingProportion)) +
  geom_bar(stat = "identity", fill = "#04485f") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Missing Data Proportion by Predictor", x = "Predictor", y = "Missing Proportion")

# Close the PDF device
dev.off()
pdf("missing_data_pattern.pdf")
# Visualize the missing data pattern using VIM package
missing_data_plot <- aggr(Soybean, col = c('navyblue', 'red'),
                          numbers = TRUE, sortVars = TRUE, 
                          labels = names(Soybean), 
                          cex.axis = 0.7, gap = 3, 
                          ylab = c("Missing data pattern", "Pattern Count"))

# Save the missing data pattern as PDF

print(missing_data_plot)
dev.off()
