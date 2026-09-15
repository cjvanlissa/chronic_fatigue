predict.res_ranger <- function(object, newdata){
  predictions(ranger:::predict.ranger(object$res,
                          data = newdata,
                          type = "response"))
}
predict.res_tree <- function(object, newdata){
  rpart:::predict.rpart(object$res, newdata = newdata)
}
predict.tuneRanger <- function(object, newdata){
  mlr:::predict.WrappedModel(object$model, newdata = newdata)$data$response
}

rsq_numeric <- function(obs, preds, mn){
  tss <- sum((obs-mn)^2)
  rss <- sum((preds - obs) ^ 2)
  return(1 - rss/tss)
}

rsq <- function(model, newdata, tss){
  preds <- predict(model, newdata)
  rss <- sum((preds - newdata$moral_concern) ^ 2)
  return(1 - rss/tss)
}

etasq <- function(model){
  tmp <- summary(model)
  sss <- unclass(tmp[[1]])$`Sum Sq`
  etasqs <- (sss/sum(sss))[-length(sss)]
  partial <- (sss[-length(sss)]/(sss[-length(sss)]+sss[length(sss)]))
  if(length(partial) < 1) partial <- rep(NA, length(etasqs))
  data.frame(term = attr(model$terms,"term.labels"),
            etasq = etasqs,
            partial = partial)
}


# eval_results <- function(dat, res_lasso, res_ranger){
# # Evaluate performance ----------------------------------------------------
# mean_y_train <-  mean(dat$train$moral_concern)
#
# # On training data
# tss <- sum((dat$train$moral_concern - mean_y_train) ^ 2)
# rsq_lasso <- rsq(res_lasso, dat$train, tss)
# rsq_ranger <- rsq(res_ranger, dat$train, tss)
#
# # On test data
# tss <- sum((dat$test$moral_concern - mean_y_train) ^ 2)
# rsq_lasso_test <- rsq(res_lasso, dat$test, tss)
# rsq_ranger_test <- rsq(res_ranger, dat$test, tss)
#
# data.frame(
#     R2 = c(rsq_lasso, rsq_ranger, rsq_lasso_test, rsq_ranger_test),
#     model = rep(c("lasso", "ranger"), 2),
#     data = rep(c("train", "test"), each = 2)
#   )
# }



eval_results <- function(dat, models){
  # for(n in names(models)){
  #   names(models[[n]]) <- paste0(n, "_", names(models[[n]]))
  # }
  #models <- do.call(c, models)

  # Evaluate performance ----------------------------------------------------
  mses <- sapply(models, function(x){
    if("cvm" %in% names(x$mse_cv)){
      return(x$mse_cv["cvm"])
    } else {
      return(mean(x$mse_cv))
    }
  })
  mse_sds <- sapply(models, function(x){
    if("cvsd" %in% names(x$mse_cv)){
      return(x$mse_cv["cvsd"])
    } else {
      return(sd(x$mse_cv))
    }
  })
  rsqs_train <- sapply(models, `[[`, "rsq_train")
  # On test data
  rsqs_test <- rsqs_train <- sapply(models, `[[`, "rsq")

  df_rsq <- data.frame(
    mse = mses,
    mse_se = mse_sds,
    rsq_test = rsqs_test,
    rsq_train = rsqs_train,
    do.call(rbind, strsplit(names(rsqs_test), ".", fixed = TRUE)))
  names(df_rsq)[ncol(df_rsq)] <- c("model")
  #mod_rsq <- aov(rsq_test ~ model, data = df_rsq)

  # Choose best model
  #rsqs <- unlist(lapply(do.call(c, models), `[[`, "rsq"))
  best_model <- mses[which.min(mses)]
  if(!grepl("(lasso|tree)", names(best_model))){
    within_se <- mses[grepl("(lasso|tree)", names(mses))]
    within_se <- within_se[within_se <= best_model+mse_sds[which.min(mses)]]
    if(any(grepl("(lasso|tree)", names(within_se)))){
      best_model <- within_se[which.min(within_se)]
    }
  }

  return(
    list(
      rsqs = df_rsq,
      #etasqs = etasq(mod_rsq),
      best = names(best_model)
    )
  )
}

# features <- list(
#   all = setdiff(names(dat$train), "moral_concern")
#   , target = c("target_group", "utility", "similarity_humans", "social_status")
#   , judge = c("conservative_econ", "conservative_social")
#   , demographic = c("gender", "age", "country")
# )
# models <- list(lasso = res_lasso, ranger = res_ranger, tree = res_tree)
