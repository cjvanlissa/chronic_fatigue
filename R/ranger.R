do_ranger <- function(dat, dv = "cis4_nextwave"){
  library(mlr)
  library(tuneRanger)
  library(ranger)
  X <- dat$train
  X[["patientid"]] <- NULL
  reg_task = mlr::makeRegrTask(data = X, target = dv, blocking = factor(dat$train$patientid))
  # Tuning
  res_tune_ranger = tuneRanger::tuneRanger(reg_task, measure = list(mse), tune.parameters = c("mtry", "min.node.size"))
  pred <- predict(res_tune_ranger$model$learner.model, data = dat$test)$predictions
  pred_train <- predict(res_tune_ranger$model$learner.model, data = dat$train)$predictions
  # Get cv error
  cv_mses <- sapply(dat$folds, function(f){
    Args <- as.list(res_tune_ranger$recommended.pars)
    Args[c("mse", "exec.time")] <- NULL
    Args <- c(Args,
              list(
                formula = quote(as.formula(paste0(dv, "~ ."))),
                data = dat$train[-f, ]
              )
              )
    forest_model <- do.call(ranger::ranger, args = Args)
    preds <- ranger:::predict.ranger(forest_model, data = dat$train[f, ])$predictions
    mean((dat$train[[dv]][f]-preds)^2)
    })

  Args <- as.list(res_tune_ranger$recommended.pars)
  Args[c("mse", "exec.time")] <- NULL
  Args <- c(Args,
              list(
                formula = quote(as.formula(paste0(dv, "~ ."))),
                data = dat$train,
                importance = "permutation"
              )
    )
  forest_model <- do.call(ranger::ranger, args = Args)

  out <- list(
    res_cv = res_tune_ranger,
    res = forest_model,
    tune_pars = unlist(res_tune_ranger$recommended.pars)[c("mtry", "min.node.size")],
    mse_cv = cv_mses,
    rsq = rsq_numeric(dat$test[[dv]], pred, mean(dat$train[[dv]]))
    , rsq_train = rsq_numeric(dat$train[[dv]], pred_train, mean(dat$train[[dv]]))
  )
  class(out) <- "res_ranger"
  return(out)
}
