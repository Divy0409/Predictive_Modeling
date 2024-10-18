# Load necessary libraries
library(AppliedPredictiveModeling)
library(caret)
library(pls)

# Load the dataset
data(ChemicalManufacturingProcess)

# Extract predictors (X) and outcome (Y)
X <- ChemicalManufacturingProcess[, -ncol(ChemicalManufacturingProcess)]  # All but the last column
Y <- ChemicalManufacturingProcess[, ncol(ChemicalManufacturingProcess)]   # The last column (Yield)

# Set up training control
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

# Train PLS models with 1 to 10 components
set.seed(123)
pls_model <- train(X, Y, 
                   method = "pls", 
                   tuneGrid = expand.grid(ncomp = 1:10), 
                   trControl = train_control,
                   preProcess = c("center", "scale"))

# Print the cross-validated results
print(pls_model$results)

# a) One-Standard Error Rule
best_R2 <- max(pls_model$results$Rsquared)
best_R2_std <- pls_model$results[pls_model$results$Rsquared == best_R2, "RsquaredSD"]

# Apply the one-standard error rule
one_std_threshold <- best_R2 - best_R2_std
parsimonious_model <- min(pls_model$results$ncomp[pls_model$results$Rsquared >= one_std_threshold])
cat("Number of PLS components for the most parsimonious model:", parsimonious_model, "\n")

# b) Tolerance Values (10% loss in R2 is acceptable)
tolerance_threshold <- best_R2 * 0.90
tolerance_model <- min(pls_model$results$ncomp[pls_model$results$Rsquared >= tolerance_threshold])
cat("Optimal number of PLS components with 10% tolerance:", tolerance_model, "\n")
     