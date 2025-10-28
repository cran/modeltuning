## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(comment = "#",
                      collapse = TRUE,
                      eval = TRUE,
                      echo = TRUE,
                      warning = FALSE,
                      message = FALSE)

## -----------------------------------------------------------------------------
library(rsample)
library(yardstick)
library(modeltuning)

mtcars$w <- abs(rnorm(nrow(mtcars)))

splitter <- function(data, ...) lapply(vfold_cv(data, ...)$splits, \(.x) .x$in_id)

## -----------------------------------------------------------------------------
mtcars_cv <- CV$new(
  learner = glm,
  learner_args = list(weights = .data$w, family = gaussian),
  splitter = splitter,
  splitter_args = list(v = 2, strata = .data$cyl),
  scorer = list("rmse" = yardstick::rmse_vec),
  scorer_args = list("rmse" = list(case_weights = .data$w)),
  prediction_args = list("rmse" = list(weights = .data$w))
)
mtcars_cv_fitted <- mtcars_cv$fit(formula = mpg ~ . - w, data = mtcars)

coef(mtcars_cv_fitted$model)

## -----------------------------------------------------------------------------
coef(glm(mpg ~ . - w, data = mtcars, weights = mtcars$w))

## -----------------------------------------------------------------------------
mtcars_cv <- CV$new(
  learner = glm,
  learner_args = list(weights = mtcars$w[.index], family = gaussian),
  splitter = splitter,
  splitter_args = list(v = 2, strata = cyl),
  scorer = list("rmse" = yardstick::rmse_vec),
  scorer_args = list("rmse" = list(case_weights = mtcars$w[.index])),
  prediction_args = list("rmse" = list(weights = mtcars$w[.index]))
)
mtcars_cv_fitted <- mtcars_cv$fit(formula = mpg ~ . - w, data = mtcars)

coef(mtcars_cv_fitted$model)

## -----------------------------------------------------------------------------
coef(glm(mpg ~ . - w, data = mtcars, weights = mtcars$w))

## -----------------------------------------------------------------------------
mtcars_train <- mtcars[1:25, ]
mtcars_eval <- mtcars[26:nrow(mtcars), ]

mtcars_gs <- GridSearch$new(
  learner = glm,
  tune_params = list(na.action = c(na.omit, na.fail)),
  learner_args = list(weights = .data$w, family = gaussian),
  evaluation_data = list(x = mtcars_eval, y = mtcars_eval$mpg),
  scorer = list("rmse" = yardstick::rmse_vec),
  scorer_args = list("rmse" = list(case_weights = .data$w)),
  prediction_args = list("rmse" = list(weights = .data$w))
)
mtcars_gs_fitted <- mtcars_gs$fit(formula = mpg ~ . - w, data = mtcars_train)

mtcars_gs_fitted$best_params

## ----error=TRUE---------------------------------------------------------------
try({
mtcars_gs <- GridSearch$new(
  learner = glm,
  tune_params = list(na.action = c(na.omit, na.fail)),
  learner_args = list(weights = mtcars$w[.index], family = gaussian),
  evaluation_data = list(x = mtcars_eval[, -1], y = mtcars_eval[, 1]),
  scorer = list("rmse" = yardstick::rmse_vec),
  scorer_args = list("rmse" = list(case_weights = mtcars$w[.index])),
  prediction_args = list("rmse" = list(weights = mtcars$w[.index]))
)
mtcars_gs_fitted <- mtcars_gs$fit(formula = mpg ~ . - w, data = mtcars_train)
})

## -----------------------------------------------------------------------------
mtcars_gs <- GridSearch$new(
  learner = glm,
  tune_params = list(na.action = c(na.omit, na.fail)),
  learner_args = list(weights = mtcars_train$w, family = gaussian),
  evaluation_data = list(x = mtcars_eval[, -1], y = mtcars_eval[, 1]),
  scorer = list("rmse" = yardstick::rmse_vec),
  scorer_args = list("rmse" = list(case_weights = mtcars_eval$w)),
  prediction_args = list("rmse" = list(weights = mtcars_eval$w))
)
mtcars_gs_fitted <- mtcars_gs$fit(formula = mpg ~ . - w, data = mtcars_train)

coef(mtcars_gs_fitted$best_model)

## -----------------------------------------------------------------------------
mtcars_gs_cv <- GridSearchCV$new(
  learner = glm,
  tune_params = list(na.action = c(na.omit, na.fail)),
  learner_args = list(weights = .data$w, family = gaussian),
  splitter = splitter,
  splitter_args = list(v = 2, strata = cyl),
  scorer = list("rmse" = yardstick::rmse_vec),
  scorer_args = list("rmse" = list(case_weights = .data$w)),
  prediction_args = list("rmse" = list(weights = .data$w))
)
mtcars_gs_cv_fitted <- mtcars_gs_cv$fit(formula = mpg ~ . - w, data = mtcars)

coef(mtcars_gs_cv_fitted$best_model)

## -----------------------------------------------------------------------------
mtcars_gs_cv <- GridSearchCV$new(
  learner = glm,
  tune_params = list(na.action = c(na.omit, na.fail)),
  learner_args = list(weights = mtcars$w[.index], family = gaussian),
  splitter = splitter,
  splitter_args = list(v = 2, strata = cyl),
  scorer = list("rmse" = yardstick::rmse_vec),
  scorer_args = list("rmse" = list(case_weights = mtcars$w[.index])),
  prediction_args = list("rmse" = list(weights = mtcars$w[.index]))
)
mtcars_gs_cv_fitted <- mtcars_gs_cv$fit(formula = mpg ~ . - w, data = mtcars)

coef(mtcars_gs_cv_fitted$best_model)

