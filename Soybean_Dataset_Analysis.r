library(mlbench)

data(Soybean)
head(Soybean)

# Investigate categorical predictors
categorical_vars <- sapply(Soybean, is.factor)
categorical_data <- Soybean[, categorical_vars]

# Save plots to a PDF file
pdf("categorical_distributions.pdf", width = 12, height = 10)  

# Plot frequency distributions
par(mfrow = c(6, 7))  # Adjust layout based on the number of categorical variables

for (var in names(categorical_data)) {
  counts <- table(categorical_data[[var]])
  barplot(counts, main = var, col = "lightblue", las = 2, cex.names = 0.6)
}

# Close the PDF device
dev.off()

# Proportion of missing data
missing_data <- colSums(is.na(Soybean)) / nrow(Soybean)
missing_data_summary <- data.frame(Predictor = names(missing_data), MissingProportion = missing_data)

print(missing_data_summary)

# Visualize missing data patterns
library(ggplot2)
ggplot(missing_data_summary, aes(x = Predictor, y = MissingProportion)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Missing Data Proportion by Predictor", x = "Predictor", y = "Missing Proportion")

# Check missing data by class
missing_by_class <- aggregate(is.na(Soybean), by = list(Soybean$Class), FUN = mean)
print(missing_by_class)
