do_nn <- function(dat, dv = NULL, epochs = 10){
  library(tensorflow)
  library(keras3)
    # Get rownumbers for all folds
    all.folds <- dat$folds
    fold_id <- unlist(lapply(seq_along(all.folds), function(id){
      out <- rep(id, length(all.folds[[id]]))
      names(out) <- all.folds[[id]]
      out
    }))
    fold_id <- fold_id[order(as.integer(names(fold_id)))]
    # Prep data
    train_dataset <- dat$train
    test_dataset <- dat$test

    train_features <- model.matrix(as.formula(paste0(dv, " ~.")), dat$train)
    train_labels <- dat$train[[dv]]

    test_features <- model.matrix(as.formula(paste0(dv, " ~.")), dat$test)
    test_labels <- dat$test[[dv]]

    # Get CV mean squared errors
    mses <- vector("numeric", length = length(dat$folds))
    for(thisfold in seq_along(dat$folds)){
      train_features_minusk <- train_features[!fold_id == thisfold, -1]
      train_labels_minusk <- train_labels[!fold_id == thisfold]
      train_features_k <- train_features[fold_id == thisfold, -1]
      train_labels_k <- train_labels[fold_id == thisfold]

      # Define model
      keras_model_sequential() |>
        layer_dense(512, kernel_regularizer = regularizer_l2(0.0001),
                    activation = 'elu', input_shape = dim(train_features_minusk)[2]) |>
        layer_dropout(0.5) |>
        layer_dense(512, kernel_regularizer = regularizer_l2(0.0001),
                    activation = 'elu') |>
        layer_dropout(0.5) |>
        layer_dense(512, kernel_regularizer = regularizer_l2(0.0001),
                    activation = 'elu') |>
        layer_dropout(0.5) |>
        layer_dense(512, kernel_regularizer = regularizer_l2(0.0001),
                    activation = 'elu') |>
        layer_dropout(0.5) |>
        layer_dense(1) ->
        model

      model |>
        compile(
          loss = "mse",
          optimizer = optimizer_adam(),
          metrics = list("mean_squared_error")
        )

      model |> fit(x = train_features_minusk,
                   y = train_labels_minusk,
                   validation_split = 0,
                   verbose = 0,
                   epochs = epochs)

      pred <- predict(model, train_features_k, verbose = 0)
      mses[thisfold] <- mean((pred-train_labels_k)^2)
    }

    # Get final model
    # Define model
    keras_model_sequential() |>
      layer_dense(512, kernel_regularizer = regularizer_l2(0.0001),
                  activation = 'elu', input_shape = dim(train_features)[2]) |>
      layer_dropout(0.5) |>
      layer_dense(512, kernel_regularizer = regularizer_l2(0.0001),
                  activation = 'elu') |>
      layer_dropout(0.5) |>
      layer_dense(512, kernel_regularizer = regularizer_l2(0.0001),
                  activation = 'elu') |>
      layer_dropout(0.5) |>
      layer_dense(512, kernel_regularizer = regularizer_l2(0.0001),
                  activation = 'elu') |>
      layer_dropout(0.5) |>
      layer_dense(1) ->
      model

    model |>
      compile(
        loss = "mse",
        optimizer = optimizer_adam(),
        metrics = list("mean_squared_error")
      )

    model |> fit(x = train_features,
                 y = train_labels,
                 validation_split = 0,
                 verbose = 0,
                 epochs = epochs)

    pred <- predict(model, test_features, verbose = 0)
    pred_train <- predict(model, train_features, verbose = 0)

    out <- list(
      res = NA,
      tune_pars = vector("numeric"),
      mse_cv = mses,
      rsq = rsq_numeric(test_labels, pred, mean(train_labels)),
      rsq_train = rsq_numeric(train_labels, pred_train, mean(train_labels))
    )
    class(out) <- "res_nn"
  return(out)
}
