## My first predictive model
##########
##  Function to flag missing value in variables
##########
flag_missing_val <- function(data, var) {
  data$missing_yn <- ifelse(is.na(data$var), 1, 0)
  idx <- which(data$missing_yn == 1)
  return(idx)
}
##########
##  Function to impute missing value in variables
##########
impute_vars <- function(data, var, idx) {
  ## Mean imputation
  if (is.numeric(data$var)) {
    mean_val <- mean(data$var)
    data$var[idx] <- mean_val
  } else{
    data$var[idx] <- 'NULL'
  }
  return(data)
}
####################
##  Create basic model
####################
create_model <- function(trainData, target) {
  set.seed(120)
  model <- lm(target ~ . , data = trainData)
  return(model)
}
############
## AUC calculation for Kaggle
############
auc4kaggle <- function(outcome, proba){
  N = length(proba)
  N_pos = sum(outcome)
  df = data.frame(out = outcome, prob = proba)
  df = df[order(-df$prob),]
  df$above = (1:N) - cumsum(df$out)
  return( 1- sum( df$above * df$out ) / (N_pos * (N-N_pos) ) )
}
###############################################
##  Action!
##############################################
red_wine_data <-
  read.csv(
    'data/winequality-red.csv',
    check.names = F,
    sep = ';',
    stringsAsFactors = T
  )
colnames(red_wine_data)
str(red_wine_data)
####
white_wine_data <-
  read.csv(
    'data/winequality-white.csv',
    check.names = F,
    sep = ';',
    stringsAsFactors = T
  )
#########################
##  Data treatment
# for(c in colnames(red_wine_data)){
#   idx <- flag_missing_val(red_wine_data,c)
#   if(length(idx) != 0){
#     red_wine_data <- impute_vars(red_wine_data,c,idx)    
#   }
# }
#########################
library(caret)
library(pROC)
#####
train_idx <- createDataPartition(y = red_wine_data$quality,times = 1,p = 0.66,list = F)
model <- create_model(red_wine_data[train_idx,],red_wine_data$quality[train_idx])
score <- predict.lm(model,newdata = red_wine_data[-train_idx,])
##
auc(red_wine_data$quality[-train_idx],score)
###
train_idx <- createDataPartition(y = white_wine_data$quality,times = 1,p = 0.66,list = F)
model <- create_model(white_wine_data[train_idx,],white_wine_data$quality[train_idx])
score <- predict.lm(model,newdata = white_wine_data[-train_idx,])
##
auc(white_wine_data$quality[-train_idx],score)
