library(readr)
library(tidyverse)
library(glmnet)
library(caret)
library(ROCR)
library(class)
library(caTools)
library(rpart) 
library(rpart.plot)
library(randomForest) 
library(gbm)

train <- read.csv("bike_train.csv")
test <- read.csv("bike_test.csv")

train$avg_docks_available <- as.numeric(train$avg_docks_available)
test$avg_docks_available <- as.numeric(test$avg_docks_available)

row.has.na <- apply(train, 1, function(x){any(is.na(x))})
predictors_no_NA <- train[!row.has.na, ]
row.has.na.test <- apply(test, 1, function(x){any(is.na(x))})
test_no_NA <- test[!row.has.na.test, ]

predictors_no_NA$zip_code <- as.factor(predictors_no_NA$zip_code)
test_no_NA$zip_code <- as.factor(test_no_NA$zip_code)


x.train = model.matrix(avg_docks_available ~ zip_code + max_temperature_f + mean_temperature_f + 
                         min_temperature_f + max_dew_point_f + mean_dew_point_f + min_dew_point_f + 
                         max_humidity + mean_humidity + min_humidity + max_sea_level_pressure_inches + 
                         mean_sea_level_pressure_inches + min_sea_level_pressure_inches + 
                         max_visibility_miles + mean_visibility_miles + min_visibility_miles + 
                         max_wind_Speed_mph + mean_wind_speed_mph + max_gust_speed_mph + cloud_cover + 
                         wind_dir_degrees + avg_docks_1D + avg_docks_7D + avg_docks_30D, data=predictors_no_NA) # The "-1" just mean that we are excluding the constant term (that is, the intercept)
y.train = predictors_no_NA$avg_docks_available
x.test = model.matrix(avg_docks_available ~ zip_code + max_temperature_f + mean_temperature_f + 
                        min_temperature_f + max_dew_point_f + mean_dew_point_f + min_dew_point_f + 
                        max_humidity + mean_humidity + min_humidity + max_sea_level_pressure_inches + 
                        mean_sea_level_pressure_inches + min_sea_level_pressure_inches + 
                        max_visibility_miles + mean_visibility_miles + min_visibility_miles + 
                        max_wind_Speed_mph + mean_wind_speed_mph + max_gust_speed_mph + cloud_cover + 
                        wind_dir_degrees + avg_docks_1D + avg_docks_7D + avg_docks_30D, data=test_no_NA) # The "-1" just mean that we are excluding the constant term (that is, the intercept)
y.test = test_no_NA$avg_docks_available

lambdas.lasso <- exp(seq(5, -5, -.01))
lasso = glmnet(x.train,y.train,alpha=1,lambda=lambdas.lasso)
cv.lasso <- cv.glmnet(x.train,y.train,alpha=1,lambda=lambdas.lasso,nfolds=10)
lasso.lambda.cv <- cv.lasso$lambda.min
lasso.final <- glmnet(x.train,y.train,alpha=1,lambda=lasso.lambda.cv)
pred.test.final <- predict(lasso.final, x.test)
SSE <- sum((pred.test.final-y.test)^2)
SST <- sum((mean(predictors_no_NA$avg_docks_available)-y.test)^2)
OSR2 <- 1-SSE/SST
OSR2



cpVals <- data.frame(.cp = c(0,0.0000001,0.000001,.00001,0.0001,0.001,0.01,0.1))
cpCV <- train(avg_docks_available ~ zip_code + max_temperature_f + mean_temperature_f + 
                min_temperature_f + max_dew_point_f + mean_dew_point_f + min_dew_point_f + 
                max_humidity + mean_humidity + min_humidity + max_sea_level_pressure_inches + 
                mean_sea_level_pressure_inches + min_sea_level_pressure_inches + 
                max_visibility_miles + mean_visibility_miles + min_visibility_miles + 
                max_wind_Speed_mph + mean_wind_speed_mph + max_gust_speed_mph + cloud_cover + 
                wind_dir_degrees + avg_docks_1D + avg_docks_7D + avg_docks_30D,
              trControl=trainControl(method="cv",number=10), data=predictors_no_NA, 
              method="rpart", minbucket=70,
              tuneGrid=cpVals, metric="Rsquared", maximize=TRUE)
best.cp <- cpCV$bestTune
mod.tree <- rpart(avg_docks_available ~ zip_code + max_temperature_f + mean_temperature_f + 
                    min_temperature_f + max_dew_point_f + mean_dew_point_f + min_dew_point_f + 
                    max_humidity + mean_humidity + min_humidity + max_sea_level_pressure_inches + 
                    mean_sea_level_pressure_inches + min_sea_level_pressure_inches + 
                    max_visibility_miles + mean_visibility_miles + min_visibility_miles + 
                    max_wind_Speed_mph + mean_wind_speed_mph + max_gust_speed_mph + cloud_cover + 
                    wind_dir_degrees + avg_docks_1D + avg_docks_7D + avg_docks_30D, 
                  data=predictors_no_NA, minbucket = 70, cp=bestTune)
prp(mod.tree, digits = 3, varlen = 0, faclen = 0)
pred.test.tree <- predict(mod.tree, newdata=test)
SSE <- sum((pred.test.tree-test$avg_docks_available)^2)
SST <- sum((mean(train$avg_docks_available)-test$avg_docks_available)^2)
OSR2 <- 1-SSE/SST
OSR2

mod_rf <- randomForest(avg_docks_available ~ zip_code + max_temperature_f + mean_temperature_f + 
                         min_temperature_f + max_dew_point_f + mean_dew_point_f + min_dew_point_f + 
                         max_humidity + mean_humidity + min_humidity + max_sea_level_pressure_inches + 
                         mean_sea_level_pressure_inches + min_sea_level_pressure_inches + 
                         max_visibility_miles + mean_visibility_miles + min_visibility_miles + 
                         max_wind_Speed_mph + mean_wind_speed_mph + max_gust_speed_mph + cloud_cover + 
                         wind_dir_degrees + avg_docks_1D + avg_docks_7D + avg_docks_30D, 
                       data = predictors_no_NA, ntree = 100)
importance.rf <- data.frame(imp=importance(mod_rf))
importance.rf
pred.best.rf <- predict(mod_rf, test_no_NA)
SSE <- sum((pred.best.rf-test_no_NA$avg_docks_available)^2)
SST <- sum((mean(predictors_no_NA$avg_docks_available)-test_no_NA$avg_docks_available)^2)
OSR2 <- 1-SSE/SST
OSR2




train.rf.oob <- train(x = predictors_no_NA %>% select(zip_code, max_temperature_f, mean_temperature_f,
                                                      min_temperature_f, max_dew_point_f, mean_dew_point_f,
                                                      min_dew_point_f, max_humidity, mean_humidity, 
                                                      min_humidity, max_sea_level_pressure_inches, 
                                                      mean_sea_level_pressure_inches, 
                                                      min_sea_level_pressure_inches, 
                                                      max_visibility_miles, mean_visibility_miles, 
                                                      min_visibility_miles, max_wind_Speed_mph, 
                                                      mean_wind_speed_mph, max_gust_speed_mph, 
                                                      cloud_cover, wind_dir_degrees, avg_docks_1D, 
                                                      avg_docks_7D, avg_docks_30D),
                      y = predictors_no_NA$avg_docks_available,
                      method="rf",
                      tuneGrid=data.frame(mtry=10:13), 
                      ntree = 300, trControl=trainControl(method="oob"))
best.mtry <- train.rf.oob$bestTune[[1]]
best.mtry
mod.rfFINAL = randomForest(avg_docks_available ~ zip_code + max_temperature_f + mean_temperature_f + 
                              min_temperature_f + max_dew_point_f + mean_dew_point_f + min_dew_point_f + 
                              max_humidity + mean_humidity + min_humidity + max_sea_level_pressure_inches + 
                              mean_sea_level_pressure_inches + min_sea_level_pressure_inches + 
                              max_visibility_miles + mean_visibility_miles + min_visibility_miles + 
                              max_wind_Speed_mph + mean_wind_speed_mph + max_gust_speed_mph + cloud_cover + 
                              wind_dir_degrees + avg_docks_1D + avg_docks_7D + avg_docks_30D, 
                            data = predictors_no_NA,mtry=train.rf.oob$bestTune[[1]])
importance.rf <- data.frame(imp=importance(mod.rfFINAL))
importance.rf
pred.best.rf <- predict(mod.rfFINAL, test_no_NA)
SSE <- sum((pred.best.rf-test_no_NA$avg_docks_available)^2)
SST <- sum((mean(predictors_no_NA$avg_docks_available)-test_no_NA$avg_docks_available)^2)
OSR2 <- 1-SSE/SST
OSR2
