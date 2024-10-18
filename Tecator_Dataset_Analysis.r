library(tidyverse)
library(caret)
library(caTools)
library(ggfortify)
library(ggpubr)
library(elasticnet)

data(tecator)

absorp[1:10,1:10]
head(endpoints)

fat<-endpoints[, 2]
df_raw<-as.data.frame(cbind(absorp, fat))

fat.pca<-prcomp(df_raw%>%select(-fat), center = TRUE, scale. = TRUE)
summary(fat.pca)
plot(fat.pca, type = 'l')

pca_var <- cumsum(fat.pca$sdev^2 / sum(fat.pca$sdev^2))
plot(pca_var, xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", type = "b")
effective_dimension <- which(pca_var >= 0.95)[1]
effective_dimension  


endpoints= as.data.frame(endpoints)
absorp = as.data.frame(absorp)

partition <- createDataPartition(endpoints[, 2], p = .8, list= FALSE)

feature_train <- absorp[ partition,]
feature_test  <- absorp[-partition,]
target_train <- endpoints[ partition, 2]
target_test <- endpoints[-partition,2]

ctrl <- trainControl(method = "cv", number = 3)

set.seed(123)

pcr.fat<-train(y = target_train, x=feature_train, method = "pcr",preProcess= c('center','scale'), trControl = ctrl, tuneLength = effective_dimension)
pcr.fat
                                                                                                                                                   
mean_rmse_pcr <- mean(pcr.fat$resample$RMSE)
mean_rmse_pcr

pred.pcr<-predict(pcr.fat, feature_test)
pcr.RMSE <- RMSE(pred.pcr, target_test) 
pcr.RMSE
