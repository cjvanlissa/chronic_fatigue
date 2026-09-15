# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(worcs)
# library(lme4)
# library(tuneRanger)
# library(ranger)
# library(glmnet)
# library(tuneRanger)
# library(mlr)
# library(doParallel)
# library(tensorflow)
#
# tmp <- try(tf$constant("Hello TensorFlow!"))
# if(!inherits(tmp, "tensorflow.tensor")){
#   reticulate::install_python()
#   tensorflow::install_tensorflow(envname = "r-tensorflow")
#   tmp <- try(tf$constant("Hello TensorFlow!"))
#   if(!inherits(tmp, "tensorflow.tensor")){
#     stop("Tensorflow not installed.")
#   }
# }



# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed.
set.seed(812)
dv <- "k_cis4"
# Replace the target list below with your own:
list(
  tar_target(
    name = df,
    command = get_data()
  )
  , tar_target(
    name = dat,
    command = preprocessing(df, k = 10)
  )
  , tar_target(
    name = res_lasso,
    command = do_lasso(dat, dv = "cis4_nextwave")
  )
  , tar_target(
    name = res_ranger,
    command = do_ranger(dat, dv = "cis4_nextwave")
  )
  , tar_target(
    name = res_tree,
    command = do_tree(dat, dv = "cis4_nextwave")
  )
  , tar_target(
    name = res_nn,
    command = do_nn(dat, dv = "cis4_nextwave", epochs = 10) # Change to 500 for real data
  )
  , tar_target(
    name = res_xgboost,
    command = do_xgboost_cv(dat, dv = "cis4_nextwave") # Change to 500 for real data
  )
  , tar_target(
    name = analysis_results,
    command = eval_results(dat, models = list(lasso = res_lasso, ranger = res_ranger, tree = res_tree, nn = res_nn, xgboost = res_xgboost))
                                              #, nn = res_nn))
  )
  , tarchetypes::tar_render(manuscript, "manuscript.Rmd", cue = tar_cue("always"))
  , tar_file (
    name = create_index,
    command = { file.rename("manuscript.html", "index.html"); return("index.html")},
    cue = tar_cue("always")
  )
)
