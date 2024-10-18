library(AppliedPredictiveModeling)
library(caret)
library(dplyr)


data(permeability)

zero_fingerprints<-nearZeroVar(fingerprints)
f_clean<-fingerprints[,-zero_fingerprints]

as.data.frame(f_clean)
number_of_columns<-ncol(f_clean)
number_of_columns

model_df <- as.data.frame(f_clean) %>% bind_cols(permeability)

set.seed(123)
train_index <- createDataPartition(model_df$permeability , p=.8, list=FALSE)
 
train <-  model_df[ train_index,] 
       
test <- model_df[-train_index,]

pls_model <- train(
  permeability ~ ., data = train, method = "pls",
  center = TRUE,
  trControl = trainControl("cv", number = 5),
  tuneLength = 20)
pls_model
plot(pls_model)

postResample(pred = predict(pls_model,test), obs =test$permeability)
