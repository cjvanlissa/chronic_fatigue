do_xgboost_cv <- function(dat, dv = "cis4_nextwave"){
  library(xgboost) #for fitting the xgboost model

  train_x <- model.matrix(as.formula(paste0(dv, " ~.")), dat$train)
  train_y <- dat$train[[dv]]

  test_x <- model.matrix(as.formula(paste0(dv, " ~.")), dat$test)
  test_y <- dat$test[[dv]]

  #define final training and testing sets
  train_folds <- lapply(dat$folds, function(f){
    xgb.DMatrix(data = train_x[-f, ], label = train_y[-f])
  })
  test_folds <- lapply(dat$folds, function(f){
    xgb.DMatrix(data = train_x[f, ], label = train_y[f])
  })
  test_y <- lapply(dat$folds, function(f){
    train_y[f]
  })

  tune_grid <- expand.grid(
    # Learning rate ~ "shrinkage" parameter, prevents overfitting
    eta = seq(.01, 0.7, length.out = 10),
    max_depth = c(2:6))

  res_tune <- sapply(1:nrow(tune_grid), function(i){
    params <- do.call(xgb.params, as.list(tune_grid[i, ]))
    sapply(seq_along(dat$folds), function(i){
      res <- xgb.train(
        params = params,
        data = train_folds[[i]],
        nrounds = 100
      )
      preds <- predict(res, newdata = test_folds[[i]])
      mean((test_y[[i]] - preds)^2)
    })
  })
  best <- which.min(colMeans(res_tune))
  final <- xgb.train(
    params = do.call(xgb.params, as.list(tune_grid[best, , drop = F])),
    data = xgb.DMatrix(data = train_x, label = train_y),
    nrounds = 100
  )

  pred_train <- xgboost:::predict.xgboost(final, newdata = train_x)
  pred_test <- xgboost:::predict.xgboost(final, newdata = test_x)
  out <- list(
    res_cv = res_tune,
    res = final,
    tune_pars = unlist(tune_grid[best, , drop = T]),
    mse_cv = c(cvm = mean(res_tune[, best]),
               cvsd = sd(res_tune[, best])),
    rsq = rsq_numeric(dat$test[[dv]], as.numeric(pred_test), mean(train_y))
    , rsq_train = rsq_numeric(dat$train[[dv]], as.numeric(pred_train), mean(dat$train[[dv]]))
  )
  class(out) <- "res_xgboost"
  return(out)
}
