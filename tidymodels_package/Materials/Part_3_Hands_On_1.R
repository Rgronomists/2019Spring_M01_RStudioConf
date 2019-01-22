# Slides for Applied Machine Learning workshop at 2019 RStudio ---
# Conference -----------------------------------------------------

# Part_3_Feature_Engineering.R Hands-on break #1

# Slide 2 --------------------------------------------------------

library(tidymodels)
theme_set(theme_bw())

# ----------------------------------------------------------------

library(AmesHousing)
ames <- make_ames() %>%
  dplyr::select(-matches("Qu"))

set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")
# This just tells it how many obs to put into training/testing
# Let's explore this a little; initial_split is one function of rsample
# Cross validation, default is 10 folds
data_cvd <- vfold_cv(ames, strata = "Sale_Price")
data_bsd <- bootstraps(ames, strata = "Sale_Price")

# Ok. But the initial split is for training and testing. 
ames_train <- training(data_split)
ames_test  <- testing(data_split)

# Now that we have our training data, we want robust estimates of parms
set.seed(2453)
cv_splits <- vfold_cv(ames_train, v = 10, strata = "Sale_Price")

mod_rec <-
  recipe(
    Sale_Price ~ Longitude + Latitude + Neighborhood,
    data = ames_train
  ) %>%
  step_log(Sale_Price, base = 10) %>%
  step_other(Neighborhood, threshold = 0.05) %>%
  step_dummy(all_nominal()) %>%
  prep(training = ames_train)

# ----------------------------------------------------------------

# Instead of using step_other(), take 10 minutes and research how
# to eliminate any zero-variance predictors using the recipe
# reference site.

mod_rec <-
  recipe(
    Sale_Price ~ Longitude + Latitude + Neighborhood,
    data = ames_train
  ) %>%
  step_log(Sale_Price, base = 10) %>%
  #step_other(Neighborhood, threshold = 0.05) %>%
  #step_zv(Neighborhood) %>%
  step_dummy(all_nominal()) %>%
  prep(training = ames_train) %>%
  step_nzv(starts_with("Neighborhood_")) %>% #nzv means near zero variance
  #step_zv(all_predictors()) %>%
  prep(training = ames_train)


# Re-run the recipe with this step.
#
# What were the results?
#
# Do you prefer either of these approaches to the other?
