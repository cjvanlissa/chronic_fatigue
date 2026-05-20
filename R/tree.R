# Tree
do_tree <- function(dat, dv = "cis4_nextwave"){
  library(rpart)
  X <- model.matrix(as.formula(paste0(dv, "~.")), dat$train)[, -1]
  Y <- as.numeric(dat$train[[dv]])
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

  Args <- list(
    formula = quote(as.formula(paste0(dv, "~ ."))),
    data = dat$train,
    method = "anova",
    control = do.call(rpart.control, args = as.list(tune_grid[which.min(colMeans(res_tune)), , drop = F]))
  )
  tree_model <- do.call(rpart, args = Args)

  pred <- predict(tree_model, newdata = dat$test)
  pred_train <- predict(tree_model, newdata = dat$train)

  out <- list(
    res_cv = res_tune,
    res = tree_model,
    tune_pars = as.vector(tune_grid[which.min(res_tune), , drop = FALSE]),
    mse_cv = res_tune[, which.min(colMeans(res_tune)), drop = TRUE],
    rsq = rsq_numeric(dat$test[[dv]], pred, mean(dat$train[[dv]]))
    , rsq_train = rsq_numeric(dat$train[[dv]], pred_train, mean(dat$train[[dv]]))
  )
  class(out) <- "res_tree"
  return(out)
}
