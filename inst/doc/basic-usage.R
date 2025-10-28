## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(comment = "#",
                      collapse = TRUE,
                      eval = TRUE,
                      echo = TRUE,
                      warning = FALSE,
                      message = FALSE)

## ----Load Packages------------------------------------------------------------
library(e1071)
library(modeltuning) # devtools::install_github("dmolitor/modeltuning")
library(yardstick)

## ----Iris Big-----------------------------------------------------------------
iris_new <- do.call(
  what = rbind,
  args = replicate(n = 10, iris, simplify = FALSE)
) |>
  transform(
    Sepal.Length = jitter(Sepal.Length, 0.1),
    Sepal.Width = jitter(Sepal.Width, 0.1),
    Petal.Length = jitter(Petal.Length, 0.1),
    Petal.Width = jitter(Petal.Width, 0.1),
    Species = factor(Species == "virginica")
  )

# Shuffle the data-set
iris_new <- iris_new[sample(1:nrow(iris_new), nrow(iris_new)), ]

# Quick overview of the dataset
summary(iris_new[, 1:4])

## ----CV-----------------------------------------------------------------------
iris_cv <- CV$new(
  learner = svm,
  learner_args = list(type = "C-classification", probability = TRUE),
  splitter = cv_split,
  splitter_args = list(v = 3),
  scorer = list(roc_auc = roc_auc_vec), 
  prediction_args = list(roc_auc = list(probability = TRUE)),
  convert_predictions = list(roc_auc = function(.x) attr(.x, "probabilities")[, "FALSE"])
)

# Fit cross validated model
iris_cv_fitted <- iris_cv$fit(formula = Species ~ ., data = iris_new)

iris_cv_fitted$mean_metrics

## ----GridSearch---------------------------------------------------------------
iris_new_train <- iris_new[1:1000, ]
iris_new_eval <- iris_new[1000:nrow(iris_new), ]

iris_grid <- GridSearch$new(
  learner = svm,
  tune_params = list(
    cost = c(0.01, 0.1, 0.5, 1, 3, 6),
    kernel = c("polynomial", "radial", "sigmoid")
  ),
  learner_args = list(type = "C-classification", probability = TRUE),
  evaluation_data = list(x = iris_new_eval[, -5], y = iris_new_eval[, 5]),
  scorer = list(roc_auc = roc_auc_vec), 
  prediction_args = list(roc_auc = list(probability = TRUE)),
  convert_predictions = list(roc_auc = function(.x) attr(.x, "probabilities")[, "FALSE"]),
  optimize_score = "max"
)

# Fit cross validated model
iris_grid_fitted <- iris_grid$fit(formula = Species ~ ., data = iris_new)

iris_grid_fitted$best_params

## ----Grid Search with CV------------------------------------------------------
iris_grid <- GridSearchCV$new(
  learner = svm,
  tune_params = list(
    cost = c(0.01, 0.1, 0.5, 1, 3, 6),
    kernel = c("polynomial", "radial", "sigmoid")
  ),
  learner_args = list(type = "C-classification", probability = TRUE),
  splitter = cv_split,
  splitter_args = list(v = 3),
  scorer = list(roc_auc = roc_auc_vec), 
  prediction_args = list(roc_auc = list(probability = TRUE)),
  convert_predictions = list(roc_auc = function(.x) attr(.x, "probabilities")[, "FALSE"]),
  optimize_score = "max"
)

# Fit cross validated model
iris_grid_fitted <- iris_grid$fit(formula = Species ~ ., data = iris_new)

iris_grid_fitted$best_params

