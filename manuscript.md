Predicting Chronic Fatigue
================
20 May, 2026

This manuscript uses the Workflow for Open Reproducible Code in Science
(**vanlissaWORCSWorkflowOpen2021?**) to ensure reproducibility and
transparency. All code <!--and data--> are available at
<https://github.com/cjvanlissa/moral_standing.git>.

This is an example of a non-essential citation
(**vanlissaWORCSWorkflowOpen2021?**). If you change the rendering
function to `worcs::cite_essential`, it will be removed.

<!--The function below inserts a notification if the manuscript is knit using synthetic data. Make sure to insert it after load_data().-->

## Results

These are the Rsquared values on training and test data (use test data
to determine unbiased performance estimates):

``` r
temp_env <- new.env()
tar_load_everything(envir = temp_env)
res <- grep("res_", ls(envir = temp_env), value = TRUE)
tab_res <- temp_env$analysis_results$rsqs
rownames(tab_res) <- NULL
knitr::kable(tab_res, digits = 2)
```

|   mse | mse_se | rsq_test | rsq_train | model   |
|------:|-------:|---------:|----------:|:--------|
| 25.40 |   0.84 |     0.68 |      0.68 | lasso   |
| 25.76 |   2.54 |     0.68 |      0.68 | ranger  |
| 31.48 |   2.84 |     0.60 |      0.60 | tree    |
| 78.37 |   9.76 |     0.03 |      0.03 | nn      |
| 24.20 |   3.20 |     0.68 |      0.68 | xgboost |

The best performing model (or interpretable model whose cross-validated
mean squared error was within 1SE of the best model’s cross-validated
mean squared error) is lasso.cvm.
