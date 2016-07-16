#### Ensemble learning & Boosting techniques
forest_fire_data <- read.csv('data/forestfires.csv')
forest_fire_data$logArea <- log(forest_fire_data$area + 1)
library(dplyr)
##################
##  Feature selection - Pending
# lin_model <- lm(logArea ~ .,forest_fire_data[-13])
# summary(lin_model)
colnames(forest_fire_data)
features <- c('temp', 'RH', 'wind', 'rain')
##################
library(e1071)
library(caret)
for (run in 1:30) {
  print(paste0('------------',run,'------------'))
  train_idx <-
    createDataPartition(forest_fire_data$logArea, p = c(0.67, 0.33))
  cost_var <- seq(0.1, 1, 0.1)
  eps_var <- seq(0.01, 0.99, 0.03)
  accuracy <- 0
  optimal_eps <- 0
  optimal_cost <- 0
  for (c in cost_var) {
    # print(c)
    for (e in eps_var) {
      # print(e)
      svr_model <-
        svm(
          data.matrix(forest_fire_data[train_idx[[1]], features]),
          forest_fire_data$logArea[train_idx[[1]]],
          type = 'eps-regression',
          kernel = 'radial',
          cross = 10,
          cost = c,
          epsilon = e
        )
      predictions <-
        predict(svr_model, newdata = forest_fire_data[-train_idx[[1]], features])
      score <-
        ifelse(predictions - forest_fire_data$logArea[-train_idx[[1]]] < 0.1, 1, 0)
      tmp_accuracy <- sum(score) / length(score)
      if (tmp_accuracy > accuracy) {
        accuracy <- tmp_accuracy
        optimal_eps <- e
        optimal_cost <- c
      }
    }
  }
  print(paste0("Accuracy: ",accuracy, ':: cost = ', c, ' :: epsilon = ', e))
}
