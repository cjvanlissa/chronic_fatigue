Predicting Chronic Fatigue
================
19 May, 2026

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

|   mse | mse_se | rsq_test | rsq_train | model  |
|------:|-------:|---------:|----------:|:-------|
| 15.61 |   0.51 |     0.82 |      0.82 | lasso  |
| 16.37 |   1.32 |     0.81 |      0.81 | ranger |
| 24.21 |   3.54 |     0.75 |      0.75 | tree   |

The best performing model (or interpretable model whose cross-validated
mean squared error was within 1SE of the best model’s cross-validated
mean squared error) is lasso.cvm.
