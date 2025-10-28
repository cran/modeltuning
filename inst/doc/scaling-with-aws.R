## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(comment = "#",
                      collapse = TRUE,
                      eval = TRUE,
                      echo = TRUE,
                      warning = FALSE,
                      message = FALSE)

## ----Requisite Packages-------------------------------------------------------
library(e1071)
library(future)
library(modeltuning) # devtools::install_github("dmolitor/modeltuning")
library(parallelly)
library(paws)
library(rsample)
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

## ----Grid Search--------------------------------------------------------------
# Create a splitter function that will return CV folds
splitter_fn <- function(data) lapply(vfold_cv(data, v = 5)$splits, \(y) y$in_id)

iris_grid <- GridSearchCV$new(
  learner = svm,
  tune_params = list(
    cost = c(0.01, 0.1, 0.5, 1, 3, 6),
    kernel = c("polynomial", "radial", "sigmoid")
  ),
  learner_args = list(
    scale = TRUE,
    type = "C-classification",
    probability = TRUE
  ),
  splitter = splitter_fn,
  scorer = list(
    accuracy = accuracy_vec,
    f_measure = f_meas_vec,
    auc = roc_auc_vec
  ),
  prediction_args = list(
    accuracy = NULL,
    f_measure = NULL,
    auc = list(probability = TRUE)
  ),
  convert_predictions = list(
    accuracy = NULL,
    f_measure = NULL,
    auc = function(.x) attr(.x, "probabilities")[, "FALSE"]
  ),
  optimize_score = "max"
)

## ----N-Models-----------------------------------------------------------------
cat("We will estimate", nrow(iris_grid$tune_params), "SVM models\n")

## ----Launch Instances, eval = FALSE-------------------------------------------
# ec2_client <- ec2()
# 
# # Request Instances
# instance_req <- ec2_client$run_instances(
#   ImageId = "ami-06dd49fc9e3a5acee",
#   InstanceType = "t2.large",
#   KeyName = key_name,
#   MaxCount = 6,
#   MinCount = 6,
#   InstanceInitiatedShutdownBehavior = "terminate",
#   SecurityGroupIds = security_group,
#   # This names the instances
#   TagSpecifications = list(
#     list(
#       ResourceType = "instance",
#       Tags = list(
#         list(
#           Key = "Name",
#           Value = "Worker Node"
#         )
#       )
#     )
#   )
# )

## ----Wait for Instances, eval = FALSE-----------------------------------------
# # Chalk up a quick function to return instance IDs from our request
# instance_ids <- function(response) {
#   vapply(response$Instances, function(i) i$InstanceId, character(1))
# }
# 
# # Wait for instances to all respond as 'running'
# while(
#   !all(
#     vapply(
#       ec2_client$
#       describe_instances(InstanceIds = instance_ids(instance_req))$
#       Reservations[[1]]$
#       Instances,
#       function(i) i$State$Name,
#       character(1)
#     ) == "running"
#   )
# ) {
#   Sys.sleep(5)
# }
# 
# # Rough heuristic -- give additional 45 seconds for instances to initialize
# Sys.sleep(45)

## ----IPs, eval = FALSE--------------------------------------------------------
# # Get public IPs
# inst_public_ips <- vapply(
#   ec2_client$
#     describe_instances(InstanceIds = instance_ids(instance_req))$
#     Reservations[[1]]$
#     Instances,
#   function(i) i$PublicIpAddress,
#   character(1)
# )

## ----Compute Cluster, eval = FALSE--------------------------------------------
# cl <- makeClusterPSOCK(
#   worker = inst_public_ips,
#   user = "ubuntu",
#   rshopts = c("-o", "StrictHostKeyChecking=no",
#               "-o", "IdentitiesOnly=yes",
#               "-i", pem_fp), # Local filepath to private SSH key-pair
#   connectTimeout = 25,
#   tries = 3
# )

## ----Parallel plan, eval = FALSE----------------------------------------------
# plan(
#   list(
#     tweak(cluster, workers = cl),
#     multisession
#   )
# )

## ----Estimate Models----------------------------------------------------------
iris_grid_fitted <- iris_grid$fit(
  formula = Species ~ .,
  data = iris_new,
  progress = TRUE
)

## ----Best Model Info----------------------------------------------------------
best_idx <- iris_grid_fitted$best_idx
metrics <- iris_grid_fitted$metrics

# Print model metrics of best model
cat(
  " Accuracy:", round(100 * metrics$accuracy[[best_idx]], 2),
  "%\nF-Measure:", round(100 * metrics$f_measure[[best_idx]], 2),
  "%\n      AUC:", round(metrics$auc[[best_idx]], 4), "\n"
)

params <- iris_grid_fitted$best_params

# Print the best hyper-parameters
cat(
  "  Optimal Cost:", params[["cost"]],
  "\nOptimal Kernel:", params[["kernel"]], "\n"
)

## ----Kill Instances, eval = FALSE---------------------------------------------
# ec2_client$stop_instances(
#   InstanceIds = instance_ids(instance_req)
# )

