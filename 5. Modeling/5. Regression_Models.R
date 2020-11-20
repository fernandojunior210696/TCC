# 5. Modeling with caret

# settings
setwd("~/Desktop/TCC")

# load required packages
library(tidyverse)
library(magrittr)
library(caret)
library(zoo)
# parallel training
library(doParallel)
library(lubridate)

# allocate CPU to parallel training
cores=detectCores()
cl <- makeCluster(cores-1)
registerDoParallel(cl)

# read databases
all_songs <- read_csv('2.Datasets/songs_features_v2.csv')
final_df <- read_csv('2.Datasets/modeling_final.csv')

# encode factors features
all_songs$time_signature %<>% as.factor()
all_songs$mode %<>% as.factor()
all_songs$key %<>% as.factor()
all_songs$date_ref <- ymd('2020-11-15')
all_songs$track.album.release_date %<>% as.Date()

# remove incompleted cases
all_songs <- all_songs[complete.cases(all_songs),]
final_df <- final_df[complete.cases(final_df),]

all_songs$days_since_release <- all_songs$date_ref - all_songs$track.album.release_date
all_songs$days_since_release %<>% as.integer()

final_df$time_signature %<>% as.factor()
final_df$mode %<>% as.factor()
final_df$key %<>% as.factor()

final_df %<>% left_join(all_songs[, c('track.id','days_since_release')], by = 'track.id')
# get rid of id column
final_df %<>% 
  select(-c('track.id'))

# split train (70%) and test (30%) sets 
trainIndex <- createDataPartition(final_df$track.popularity, p = .7, 
                                  list = FALSE, 
                                  times = 1)

TrainSet <- final_df[trainIndex,]
TestSet  <- final_df[-trainIndex,]

TrainSet %<>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
TestSet %<>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

# auxiliar funtion
get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

# create dummy vars
dummies <- dummyVars(track.popularity ~ ., 
                     data = TrainSet)

dummies_train <- predict(dummies, 
                    newdata = TrainSet)

dummies <- dummyVars(track.popularity ~ ., 
                     data = TestSet)

dummies_test <- predict(dummies, 
                         newdata = TestSet)

# change to data frame
dummies_train %<>% as_tibble()
dummies_test %<>% as_tibble()

# restore target variable
dummies_train['pop_index'] <- TrainSet$track.popularity
# dummies_test['pop_index'] <- TestSet$track.popularity

# 1. Multiple linear regression model - without preprocessing

# 1.1 Grid to search - lm doesn`t have one

# 1.2 Fit with 10 fold cv and grid search

fitControl <- trainControl(method = "cv",   
                           number = 5, # no training log
                           allowParallel = TRUE )   

LMmodel <- train(pop_index ~ .,
               data = dummies_train,
               method = "lm",
               trControl = fitControl)

LMmodel

# 1.3 Show results
get_best_result(LMmodel)

# 1.4 Predict in test set
LMpred <- predict(LMmodel, dummies_test)

# 1.5 Evaluate on test set
postResample(pred = LMpred, obs = TestSet$track.popularity)

# 2. Multiple linear regression model - with preprocessing

# 2.1 Grid to search - lm doesn`t have one

# 2.2 Fit with 5 fold cv and grid search
fitControl <- trainControl(method = "cv",   
                           number = 5, # no training log
                           allowParallel = TRUE )   

PLMmodel <- train(pop_index ~ .,
               data = dummies_train,
               method = "lm",
               trControl = fitControl,
               preProcess = c("center", "scale"))

PLMmodel

# 2.3 Show results
get_best_result(PLMmodel)

# 2.4 Predict in test set
PLMpred <- predict(PLMmodel, dummies_test)

# 2.5 Evaluate on test set
postResample(pred = PLMpred, obs = TestSet$track.popularity) %>% round(2)


# 3. Xgboost without hyperparameter tunning

# 3.1 Grid to search - default
grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)


# 3.2 Fit with 5 fold cv
fitControl <- trainControl(method = "cv",   
                           number = 5, 
                           verboseIter = FALSE, # no training log
                           allowParallel = TRUE )   

XGBmodel <- train(pop_index ~ .,
               data = dummies_train,
               method = "xgbTree",
               verbose = FALSE,
               trControl = fitControl,
               tuneGrid = grid_default,
               objective = "reg:squarederror")

XGBmodel

# 3.3 Show results
get_best_result(XGBmodel)

# 3.4 Predict in test set
XGBpred <- predict(XGBmodel, dummies_test)

# 3.5 Evaluate on test set
postResample(pred = XGBpred, obs = TestSet$track.popularity)

# 4. Xgboost with hyperparameter tunning

# Step 1: Number of Iterations and the Learning Rate
tune_grid <- expand.grid(
  nrounds = seq(from = 200, to = 1000, by = 50),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)


fitControl <- trainControl(method = "cv",   
                           number = 5, 
                           verboseIter = TRUE, # no training log
                           allowParallel = TRUE )    


finalmodel <- train(pop_index ~ .,
                    data = dummies_train,
                    method = "xgbTree",
                    verbose = FALSE,
                    trControl = fitControl,
                    tuneGrid = tune_grid,
                    objective = "reg:squarederror")


get_best_result(finalmodel)
finalmodel

# Plot grid search
trellis.par.set(caretTheme())
plot(finalmodel)
finalmodel$bestTune

# nrounds max_depth   eta gamma colsample_bytree min_child_weight subsample
# 75     500         6 0.025     0                1                1         1

#  Step 2: Maximum Depth and Minimum Child Weight
tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = 1000, by = 50),
  eta = finalmodel$bestTune$eta,
  max_depth = ifelse(finalmodel$bestTune$max_depth == 2,
                     c(finalmodel$bestTune$max_depth:4),
                     finalmodel$bestTune$max_depth - 1:finalmodel$bestTune$max_depth + 1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)

finalmodel2 <- train(pop_index ~ .,
                    data = dummies_train,
                    method = "xgbTree",
                    verbose = FALSE,
                    trControl = fitControl,
                    tuneGrid = tune_grid2,
                    objective = "reg:squarederror")


get_best_result(finalmodel2)

# Plot grid search
trellis.par.set(caretTheme())
plot(finalmodel2)
finalmodel2$bestTune

# nrounds max_depth   eta gamma colsample_bytree min_child_weight subsample
# 18     900         6 0.025     0                1                1         1

# Step 3: Column and Row Sampling
tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = 1000, by = 50),
  eta = finalmodel$bestTune$eta,
  max_depth = finalmodel2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = finalmodel2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)

finalmodel3 <- train(pop_index ~ .,
                     data = dummies_train,
                     method = "xgbTree",
                     verbose = FALSE,
                     trControl = fitControl,
                     tuneGrid = tune_grid3,
                     objective = "reg:squarederror")


get_best_result(finalmodel3)

# Plot grid search
trellis.par.set(caretTheme())
plot(finalmodel3)
finalmodel3$bestTune

# nrounds max_depth   eta gamma colsample_bytree min_child_weight subsample
# 59     950         6 0.025     0              0.4                1         1

# Step 4: Gamma
tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = 1000, by = 50),
  eta = finalmodel$bestTune$eta,
  max_depth = finalmodel2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = finalmodel3$bestTune$colsample_bytree,
  min_child_weight = finalmodel2$bestTune$min_child_weight,
  subsample = finalmodel3$bestTune$subsample
)

finalmodel4 <- train(pop_index ~ .,
                     data = dummies_train,
                     method = "xgbTree",
                     verbose = FALSE,
                     trControl = fitControl,
                     tuneGrid = tune_grid4,
                     objective = "reg:squarederror")


get_best_result(finalmodel4)

# Plot grid search
trellis.par.set(caretTheme())
plot(finalmodel4)
finalmodel4$bestTune

# nrounds max_depth   eta gamma colsample_bytree min_child_weight subsample
# 36     800         6 0.025  0.05              0.4                1         1

# Step 5: Reducing the Learning Rate
tune_grid5 <- expand.grid(
  nrounds = seq(from = 100, to = 5000, by = 100),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = finalmodel2$bestTune$max_depth,
  gamma = finalmodel4$bestTune$gamma,
  colsample_bytree = finalmodel3$bestTune$colsample_bytree,
  min_child_weight = finalmodel2$bestTune$min_child_weight,
  subsample = finalmodel3$bestTune$subsample
)

finalmodel5 <- train(pop_index ~ .,
                     data = dummies_train,
                     method = "xgbTree",
                     verbose = TRUE,
                     trControl = fitControl,
                     tuneGrid = tune_grid5,
                     objective = "reg:squarederror")


get_best_result(finalmodel5)

# Plot grid search
trellis.par.set(caretTheme())
plot(finalmodel5)
finalmodel5$bestTune

# nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
# 23    2300         6 0.01  0.05              0.4                1         1

# Final model

final_grid <- expand.grid(
  nrounds = finalmodel5$bestTune$nrounds,
  eta = finalmodel5$bestTune$eta,
  max_depth = finalmodel5$bestTune$max_depth,
  gamma = finalmodel5$bestTune$gamma,
  colsample_bytree = finalmodel5$bestTune$colsample_bytree,
  min_child_weight = finalmodel5$bestTune$min_child_weight,
  subsample = finalmodel5$bestTune$subsample
)

finalXGBOOST <- train(pop_index ~ .,
                     data = dummies_train,
                     method = "xgbTree",
                     verbose = FALSE,
                     preProcess = c('knnImpute'),
                     trControl = fitControl,
                     tuneGrid = final_grid,
                     objective = "reg:squarederror")


get_best_result(finalXGBOOST)
finalXGBOOST$bestTune

# nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
# 1    2300         6 0.01  0.05              0.4                1         1

# Predict in test set
finalXGBOOSTpred <- predict(finalXGBOOST, dummies_test)

# Evaluate on test set
postResample(pred = finalXGBOOSTpred, obs = TestSet$track.popularity) %>% round(2)

stopCluster(cl)

# Plot variable importance
plot(varImp(finalXGBOOST))

# ggplot(varImp(finalXGBOOST))+
#   theme_minimal()

# Compare models

results <- resamples(list(`Regressão Linear`= PLMmodel,
                          XGboost = finalXGBOOST))
# summarize the distributions
summary(results)

# boxplot of results
bwplot(results, main = 'Resultados de treinamento dos modelos (MAE)', metric="MAE")
bwplot(results, main = 'Resultados de treinamento dos modelos (RMSE)', metric="RMSE")
bwplot(results, main = 'Resultados de treinamento dos modelos (Rsquared)', metric="Rsquared")
