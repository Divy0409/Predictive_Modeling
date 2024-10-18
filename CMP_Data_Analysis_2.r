library(AppliedPredictiveModeling)
data(ChemicalManufacturingProcess)
library(RANN) 

impute <- preProcess(ChemicalManufacturingProcess, "knnImpute")
chem_data <- predict(impute, ChemicalManufacturingProcess)

set.seed(123)

chem_data <- chem_data %>% select(!nearZeroVar(.))
train_index_chen <- createDataPartition(chem_data$Yield , p=.8, list=FALSE)
train_chem <-  chem_data[ train_index_chen,] 
test_chem <- chem_data[-train_index_chen,]

lm_model_chem <- train(
  Yield ~ ., data = train_chem, method = "lm",
  trControl = trainControl("cv", number = 3)
)
lm_model_chem

ridge_model_chem <- train(
  Yield ~ ., data = train_chem, method = "ridge",
  trControl = trainControl("cv", number = 3),
  preProcess= c('center','scale'),
  tuneLength = 10
)
ridge_model_chem
     
lasso_model_chem <- train(
  Yield ~ ., data = train_chem, method = "lasso",
  trControl = trainControl("cv", number = 3),
  preProcess= c('center','scale'),
  tuneLength = 10
)
lasso_model_chem

elasticnet_model_chem <- train(
  Yield ~ ., data = train_chem, method = "enet",
  trControl = trainControl("cv", number = 3),
  preProcess= c('center','scale'),
  tuneLength = 10
)
elasticnet_model_chem


pred_lm <- predict(lm_model_chem, test_chem)
pred_ridge <- predict(ridge_model_chem, test_chem)
pred_lasso <- predict(lasso_model_chem, test_chem)
pred_enet <- predict(elasticnet_model_chem, test_chem)

rmse_test_lm <- RMSE(pred_lm, test_chem$Yield)
rmse_test_ridge <- RMSE(pred_ridge, test_chem$Yield)
rmse_test_lasso <- RMSE(pred_lasso, test_chem$Yield)
rmse_test_enet <- RMSE(pred_enet, test_chem$Yield)

rmse_test_lm
rmse_test_ridge
rmse_test_lasso
rmse_test_enet
                                                            
elasticnet_importance <- varImp(elasticnet_model_chem, scale = FALSE)
print(elasticnet_importance)
plot(elasticnet_importance)
