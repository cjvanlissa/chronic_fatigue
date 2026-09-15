preprocessing <- function(df, k = 10){
  # Split the data into train and test data sets ----------------------------
  n <- length(unique(df$patientid))
  train <- sample(unique(df$patientid), size = floor(.7*n))
  df_train <- df[df$patientid %in% train, ]
  train_id <- df_train$patientid
  df_test <- df[!df$patientid %in% train, ]
  test_id <- df_test$patientid

  # Clean Data
  nums <- names(df_train)[sapply(df_train, inherits, what = c("numeric", "integer"))]
  nums <- setdiff(nums, c(dv, "mt", "patientid")) # Exclude DV and time variables
  scld <- scale(df_train[nums], center = TRUE, scale = TRUE)
  means <- attr(scld, "scaled:center")
  sds <- attr(scld, "scaled:scale")
  df_train[nums] <- scld

  scld_test <- df_test[nums]
  scld_test <- sweep(scld_test, 2, means)
  scld_test <- sweep(scld_test, 2, sds, FUN = "/")
  df_test[nums] <- scld_test

  df_train <- VIM::kNN(df_train)
  df_train[grep("_imp$", names(df_train))] <- NULL

  df_test <- VIM::kNN(df_test)
  df_test[grep("_imp$", names(df_test))] <- NULL

  # Remove variables only used for imputation
  table3 <- readxl::read_xlsx("data/Tables3_4_Updated3_Caspar.xlsx", 1)
  table3$Variable <- tolower(table3$Variable)
  removethese <- table3$Variable[which(table3$`Primary use` == "0")]
  df_train <- df_train[, !names(df_train) %in% removethese]
  df_test <- df_test[, !names(df_test) %in% removethese]

  # Preprocess ordinal variables; keep linear and quadratic effect
  ord <- names(df_train)[sapply(df_train, inherits, what = c("ordered"))]
  df_tmp <- model.matrix(~., df_train[, ord, drop = F])
  df_tmp <- df_tmp[, grep(".[LQ]$", colnames(df_tmp))]
  df_train <- data.frame(df_train, df_tmp)
  df_train[ord] <- NULL

  ord <- names(df_test)[sapply(df_test, inherits, what = c("ordered"))]
  df_tmp <- model.matrix(~., df_test[, ord, drop = F])
  df_tmp <- df_tmp[, grep(".[LQ]$", colnames(df_tmp))]
  df_test <- data.frame(df_test, df_tmp)
  df_test[ord] <- NULL

  # Make time lagged dv
  df_train$cis4_nextwave <- NA
  for(id in unique(df_train$patientid)){
    #id = df_train$patientid[1]
    rws <- which(df_train$patientid == id)
    df_train$cis4_nextwave[rws] <- c(df_train$k_cis4[rws], NA)
  }
  df_test$cis4_nextwave <- NA
  for(id in unique(df_test$patientid)){
    #id = df_test$patientid[1]
    rws <- which(df_test$patientid == id)
    df_test$cis4_nextwave[rws] <- c(df_test$k_cis4[rws], NA)
  }

  df_train <- df_train[!is.na(df_train$cis4_nextwave), ]
  df_test <- df_test[!is.na(df_test$cis4_nextwave), ]

  # Create k-folds ----------------------------------------------------------
  fold <- split(sample(unique(df_train$patientid)), cut(seq_along(unique(df_train$patientid)), k, labels=FALSE))
  all.folds <- lapply(1:k, function(i){
    which(df_train$patientid %in% fold[[i]])
  })
  names(all.folds) <- 1:k

  return(
    list(
      train = df_train,
      test = df_test,
      train_id = train_id[train_id %in% df_train$patientid],
      test_id = test_id[test_id %in% df_test$patientid],
      folds = all.folds,
      train_means = means,
      train_sds = sds
    )
  )
}
