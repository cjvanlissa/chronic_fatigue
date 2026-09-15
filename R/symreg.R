do_sr <- function(dat){
  suppressWarnings(library("gramEvol"))
  train_features <- model.matrix(moral_concern ~ ., dat$train)[, -1][, 1:20]
  colnames(train_features) <- paste0("x", 1:ncol(train_features))
  test_features <- model.matrix(moral_concern ~ ., dat$test)[, -1][, 1:20]
  colnames(test_features) <- paste0("x", 1:ncol(test_features))

  train_labels <- dat$train$moral_concern
  test_labels <- dat$test$moral_concern


  ruleDef <- list(expr = grule(op(var, var)),
                  op   = grule(`+`, `-`, `*`, `/`),
                  var  = do.call(grule, args = as.list((sapply(c("n", colnames(train_features)), as.name)))),
                  n    = gvrule(seq(-10,10,by=0.01)  #constants
                  )
  )

  grammarDef <- CreateGrammar(ruleDef)

  SymRegFitFunc <- function(expr) {
    result <- eval(expr)
    if (any(is.nan(result)))
      return(Inf)
    return (mean((train_labels - result)^2))
  }
  # Construct equation
  attach(as.data.frame(train_features))
  ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc, terminationCost = 0.01, iterations = 1000, max.depth = 10)
  pred_train <- eval(ge$best$expressions)
  # Compute test error
  env_test <- new.env()
  for(i in 1:ncol(test_features)){
    assign(colnames(test_features)[i], test_features[, i], envir = env_test)
  }
  pred <- eval(ge$best$expressions, envir = env_test)

  out <- list(
    #res_cv = res_tune_ranger,
    res = ge
    , tune_pars = vector("numeric")
    , rsq = rsq(test_labels, pred, mean(dat$train$moral_concern))
    , rsq_train = rsq(train_labels, pred_train, mean(dat$train$moral_concern))
  )
  class(out) <- "res_symreg"
  return(out)

}
