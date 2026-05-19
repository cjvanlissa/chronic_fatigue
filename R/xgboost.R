do_xgboost <- function(dat, dv = "diff_k_cis4"){
  library(xgboost) #for fitting the xgboost model

  train_x <- model.matrix(as.formula(paste0(dv, " ~.")), dat$train)
  train_y <- dat$train[[dv]]

  test_x <- model.matrix(as.formula(paste0(dv, " ~.")), dat$test)
  test_y <- dat$test[[dv]]

  #define final training and testing sets
  xgb_train <- xgb.DMatrix(data = train_x, label = train_y)

  # Set initial parameters

  # Perform cross-validation
  cv_results <- xgb.cv(
    #params = params,
    data = xgb_train,
    nfold = 10,
    train_folds = dat$folds,
    nrounds = 100,
    early_stopping_rounds = 10,
    showsd = TRUE
  )

  tune_grid <- expand.grid(
    minbucket = as.integer(seq(2, nrow(dat$train)/100, length.out = 20))
  )

  res_tune <- sapply(1:nrow(tune_grid), function(i){
    sapply(dat$folds, function(f){
      Args <- list(
        formula = quote(as.formula(paste0(dv, "~ ."))),
        data = dat$train[-f, ],
        method = "anova",
        control = do.call(rpart.control, args = as.list(tune_grid[i, , drop = F]))
      )
      tree_model <- do.call(rpart, args = Args)
      preds <- predict(tree_model, newdata = dat$train[f, ])
      mean((dat$train[[dv]][f]-preds)^2)
    })
  })


  # Step 4: Fit the Model
  # Next, we’ll fit the XGBoost model by using the xgb.dat$train() function, which displays the training and testing RMSE (root mean squared error) for each round of boosting.
  # model = xgb.dat$train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 70)

  best <- which.min(cv_results$evaluation_log$test_rmse_mean)
  #define final model

  final <- xgb.train(
    #params = params,
    data = xgb_train,
    nrounds = best
  )

  pred_train <- xgboost:::predict.xgboost(final, newdata = train_x)
  pred_test <- xgboost:::predict.xgboost(final, newdata = test_x)
  out <- list(
    res_cv = cv_results,
    res = final,
    tune_pars = c("iteration" = best),
    mse_cv = c(cvm = cv_results$evaluation_log$test_rmse_mean[best],
               cvsd = cv_results$evaluation_log$test_rmse_std[best]),
    rsq = rsq_numeric(test_y, as.numeric(pred_test), mean(train_y))
    , rsq_train = rsq_numeric(dat$train[[dv]], as.numeric(pred_train), mean(dat$train[[dv]]))
  )
  class(out) <- "res_xgboost"
  return(out)
}


