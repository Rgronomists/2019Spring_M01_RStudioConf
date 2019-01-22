# Slides for Applied Machine Learning workshop at 2019 RStudio ---
# Conference -----------------------------------------------------
# Jan 16 2019 (Wednesday)

# Part_4_Regression_Modeling.R

# Slide 3 --------------------------------------------------------

library(tidymodels)
library(ggrepel)
theme_set(theme_bw())

# Slide 5 --------------------------------------------------------

url <- "https://github.com/topepo/cars/raw/master/2018_12_02_city/car_data_splits.RData"
temp_save <- tempfile()
download.file(url, destfile = temp_save)
load(temp_save)

car_train %>% bind_rows(car_test) %>% group_by(year) %>% count()

# want to use cars from first 4 years to predict 2019 cars

# Slide 6 --------------------------------------------------------
# get rid of electric cars, there aren't many
# note we are including hybrids
removals <- c("CNG", "Electricity")

car_train <- 
  car_train %>% 
  dplyr::filter(!(fuel_type %in% removals)) %>%
  mutate(fuel_type = relevel(fuel_type, "Gasoline_or_natural_gas"))

car_test <-
  car_test %>% 
  dplyr::filter(!(fuel_type %in% removals)) %>%
  mutate(fuel_type = relevel(fuel_type, "Gasoline_or_natural_gas"))

library(summarytools)

car_train %>% dfSummary %>% view

# maybe gallons per mile is a more interesting way to look at it
# in Canada that is what they do

# Slide 7 -----------------------------------------------------------------

# How to decide what difference would have to be to be meaningful.
# A RMSE of 0.5 mpg. Is that meaningful?
# Max says 1-6

# Slide 9 --------------------------------------------------------

library(splines)
## lm(mpg ~ . -model + ns(eng_displ, 4) + ns(cylinders, 4), data = car_train)
# linear model means linear in the parameters
# splines, quadratic terms, etc. 

# Slide 10 -------------------------------------------------------

# There are a few super fancy cars.
# Let's other those.
car_train %>%
  group_by(make) %>%
  count() %>%
  arrange(n) %>%
  head(6)

# Slide 10 -------------------------------------------------------

# here's how we other them

basic_rec <- recipe(mpg ~ ., data = car_train) %>%
  # keep the car name but don't use as a predictor
  # update_role is a recipes thing
  # eery column has a role. 
  # mpg has a role of outcome
  # everything else has a role of predictor
  # this is trick where car model is not a predictor, but keep it
  update_role(model, new_role = "model") %>%
  
  # collapse some makes into "other"
  step_other(make, car_class, threshold = 0.005) %>% #--there are weird makes
  step_other(fuel_type, threshold = 0.01) %>% #--there are weird fuel types
  step_dummy(all_nominal(), -model) %>% #--make dummy vars w/factored preds
  step_zv(all_predictors()) #--get rid of zero-variance preds


# Slides 13-14 ------------------------------------------------------------

# things are correlated.
# 2 door luggage volume
# 2 door passenger volume
# including one or the other or both really fucks with coefficients
# he gets a variance inflation factor that shows this

# regularizing regression coeffs helps
# glmnet uses it
# he does an elastic net model, where alpha (1 = lasso, 0=RR) is optimized
# it's very important to center and scale your data here
# so the penalty is applying equally to all predictors
# L1 penalty gives you mostly 0s, then a couple non-zeros;
#  the correlated feature it picks is arbitrary
# L2 dampens coefficients

# so we need to explore both lambda and alpha
# with lambda explore a log scale, alpha must be between 0-1

# Slide 15 -------------------------------------------------------

# in general, lambda is in decimal ranges.
# make sure your tuning params figs show a minimum, if they
#  don't you haven't explored enough lambda space

glmn_grid <- expand.grid(alpha = seq(0, 1, by = .25), 
                         lambda = 10^seq(-3, -1, length = 20))
nrow(glmn_grid)


# Slidd 17 ----------------------------------------------------------------

library(caret)

# caret is a lot older than other packages
# it uses camel case apparently
# it has 3 basic interfaces
# caret should take a recipe?
# caret does high level stuff
# you CAN use the previous workflow, it just requires more coding

train(recipe, data = dataset)
#or
train(y~., data = dataset)
#or the vector thing


# Slide 18 -------------------------------------------------------

# this specifies the type of cross-validation you want to do
ctrl <- trainControl(
  method = "cv", #--could use bagged trees, whatever .It's your resample strat
  # there are lots of defaults
  # we haven't specified the number of folds,
  # Max sets it at 10. The defaults are Max's favorites
  # Save the assessment predictions from the best model
  savePredictions = "final",
  # Log the progress of the tuning process
  verboseIter = TRUE
  )

# Slide 19 -------------------------------------------------------

# preprocessing, using recipes
# this is stupid, we centered and scaled after making dummy vars
glmn_rec <- 
  basic_rec %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_ns(eng_displ, cylinders, options = list(df = 4))

set.seed(92598)
glmn_mod <- train(
  glmn_rec, #--this is the recipe
  data = car_train,
  method = "glmnet", 
  trControl = ctrl,
  tuneGrid = glmn_grid
  )

# caret automatically did a submodel trick. ugh. 

# Slide 21 -------------------------------------------------------

glmn_mod$bestTune

# we can call ggplot directly on our glmn_mod object
ggplot(glmn_mod) + scale_x_log10() + theme(legend.position = "top")

# Slide 22 -------------------------------------------------------

glmn_mod$pred %>% head(4)

ggplot(glmn_mod$pred, aes(x = obs, y = pred)) +
  geom_abline(col = "green", alpha = .5) + 
  geom_point(alpha = .3) + 
  geom_smooth(se = FALSE, col = "red", 
              lty = 2, lwd = 1, alpha = .5)


# Slide 23 -------------------------------------------------------

# define helper functions to make a better plot

add_columns <- function(x, dat, ...) {
  # capture any selectors and filter the data
  dots <- quos(...)
  if (!is_empty(dots))
    dat <- dplyr::select(dat, year, model, !!!dots)
  
  dat <-
    x %>%
    pluck("pred") %>%
    arrange(rowIndex) %>%
    dplyr::select(-rowIndex) %>%
    bind_cols(dat)
  
  # create a label column when possible
  if (all(c("model", "year") %in% names(dat)))
    dat <-
    dat %>%
    mutate(plot_label = paste(year, model))
  dat
}

# Slide 24 -------------------------------------------------------

obs_pred_plot <- function(x, dat, cutoff = 25, ...) {
  
  pred_dat <- x %>%
    add_columns(dat, model, year) %>%
    mutate(residuals = obs - pred) 
  
  ggplot(pred_dat, aes(x = pred, y = obs)) +
    
    geom_abline(col = "green", alpha = .5) + 
    
    geom_point(alpha = .3) + 
    
    geom_smooth(
      se = FALSE, col = "red", 
      lty = 2, lwd = .25, alpha = .5
    ) + 
    
    geom_text_repel(
      data = dplyr::filter(pred_dat, abs(residuals) > cutoff),
      aes(label = plot_label),
      segment.color = "grey50"
    )
}

# Slide 24 -------------------------------------------------------

resid_plot <- function(x, dat, cutoff = 25, ...) {
  
  pred_dat <- x %>%
    add_columns(dat, model, year) %>%
    mutate(residuals = obs - pred) 
  
  ggplot(pred_dat, aes(x = pred, y = residuals)) +
    
    geom_hline(col = "green", yintercept = 0) + 
    
    geom_point(alpha = .3) + 
    
    geom_text_repel(
      data = dplyr::filter(
        pred_dat, 
        abs(residuals) > cutoff
      ),
      aes(label = plot_label),
      segment.color = "grey50"
    )
}

# Slide 25 -------------------------------------------------------

# these are cool
obs_pred_plot(glmn_mod, car_train)
resid_plot(glmn_mod, car_train)

# Slide 26 -------------------------------------------------------

# maybe we want to know what is driving the model
# which feature is doing the 'work'
# regression coeffs are like varImp scores
reg_imp <- varImp(glmn_mod, scale = FALSE) #--why wouldn't you scale?
reg_imp <- varImp(glmn_mod, scale = T) #makes most important one 100
# var imp for factors gives importance for a factor level,
#  not the overall factor
ggplot(reg_imp, top = 30) + xlab("")


# Slide 27 ----------------------------------------------------------------
# notes on train:
# setting the seed right before train ensures things are reproducible
# the formula interface automatically makes dummy variables
# you don't want that in tree-based models
# use the recipe interface to avoid that


# Slide 28 -------------------------------------------------------
## library(glmnet)
## plot(glmn_mod$finalModel, xvar = "lambda")

# only use predict on objects produced by train

# Slide 29/30 ----------------------------------------------------
# redo the glmnet plot in ggplot2

# Get the set of coefficients across penalty values
tidy_coefs <- broom::tidy(glmn_mod$finalModel) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::select(-step, -dev.ratio)

# Get the lambda closest to caret's optimal choice
delta <- abs(tidy_coefs$lambda - glmn_mod$bestTune$lambda)
lambda_opt <- tidy_coefs$lambda[which.min(delta)]

# Keep the large values
label_coefs <- tidy_coefs %>%
  mutate(abs_estimate = abs(estimate)) %>%
  dplyr::filter(abs_estimate >= 3) %>%
  distinct(term) %>%
  inner_join(tidy_coefs, by = "term") %>%
  dplyr::filter(lambda == lambda_opt)

# plot the paths and highlight the large values
tidy_coefs %>%
  ggplot(aes(x = lambda, y = estimate, group = term, col = term, label = term)) +
  geom_line(alpha = .4) +
  theme(legend.position = "none") +
  scale_x_log10() +
  geom_text_repel(data = label_coefs, aes(x = .0005))


# Slide 32-42 ----------------------------------------------------------------

# gcb will figure out how many MARS entries. Or you can do it yourself.
# Max likes to see the relationship, he likes more complex models than
#  gcb likes
# MARS does feature selection. If something isn't important, 
#  it eliminates it. 
# Recipes doesn't fit into this framework, I guess. 
# SEcond degree MARS term, conceptual ex
#  increasing the number of bedrooms has a different effect on price
#  depending on the square footage. 
#  Below 900 square feet, more rooms = less money. 2nd degree MARS finds this.
# Earth package is superior to mda

# Center and scaling not required
# Change qualitative data to dummy vars
# no need to remove zv preds

# Slide 43 -------------------------------------------------------

# We built this ctrl earlier...
ctrl$verboseIter <- FALSE

mars_grid <- expand.grid(degree = 1:2, nprune = seq(2, 26, by = 2))

# Using the same seed to obtain the same 
# resamples as the glmnet model.
# this takes FOREVER. 
# set.seed(92598)
# mars_mod <- train(
#   basic_rec,
#   data = car_train,
#   method = "earth",
#   tuneGrid = mars_grid,
#   trControl = ctrl
# )

# How long? Parallel processing reduced it by ~1 minute. Fucking great. 
mars_mod$times$everything[[3]]/60

# Parallel Processing Code ---------------------------------------

# Caret will run in parallel automatically?
parallel::detectCores()

library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 2)
registerDoParallel(cl)


# Slide 46 -------------------------------------------------------

ggplot(mars_mod) + theme(legend.position = "top")

# Slide 47 -------------------------------------------------------

obs_pred_plot(mars_mod, car_train)

resid_plot(mars_mod, car_train)

# Slide 48 -------------------------------------------------------

library(earth)
# the 1 10 15 means:
# 1= intercept
# 2 = single terms
# 3 = interaction terms

mars_mod$finalModel

# Slide 49 -------------------------------------------------------

mars_mod$finalModel %>% format() %>% cat()

# Slide 50 -------------------------------------------------------

mars_imp <- varImp(mars_mod)
ggplot(mars_imp, top = 20) + xlab("")

# An importance of 100 means that variable is 2 fold more important
#  than importance of 50.

# Slide 51 -------------------------------------------------------

set.seed(92598)
mars_gcv_mod <- train(
  basic_rec, 
  data = car_train,
  method = "gcvEarth", #--instead of just "earth"
  tuneGrid = data.frame(degree = 1:2),
  trControl = ctrl
)
mars_gcv_mod$finalModel
mars_gcv_mod$times$everything[[3]]/60

mars_gvc_imp <- varImp(mars_gcv_mod)
ggplot(mars_gvc_imp, top = 20) + xlab("")


# Tangent - c50 and cubist - Max loves them
# It makes a tree that ends in a linear regression
# But it is all c code - do we have access to it?
# Yes, in the cubist package. Hmm. 


# Slides 53-57 ------------------------------------------------------------

# Bagging reduces variation
# Bootstrap aggregating
# Does removing some data sig change model? If so, bagging could be good.
# Simple linear regression is not a good candidate for ensembles
# MARS is a good candidate.
# Tree-based models too. 
# In MAx's experience, you can get a 5-10% improvement in fit

# Slide 52 -------------------------------------------------------

set.seed(92598)
# THis takes seriously like 30 minutes to run.
# But caret can do bagging
# mars_gcv_bag <- train(
#   basic_rec, 
#   data = car_train,
#   method = "bagEarthGCV",
#   tuneGrid = data.frame(degree = 1:2),
#   trControl = ctrl,
#   # Number of bootstraps for `bagEarth` function
#   B = 50
# )

# Slide 57 -------------------------------------------------------
# Is bagging worth it? We reduce the RMSE from 2.7 to 2.4, 
#  but we don't have an equation from the model now. 

mars_gcv_bag

# Slide 58 -------------------------------------------------------

obs_pred_plot(mars_gcv_bag, car_train)

resid_plot(mars_gcv_bag, car_train)

# Slide 60 -------------------------------------------------------

# There is a resample-to-resample effect. We must account for this. 
ls()
load("RData/glmn_mod.RData")
load("RData/mars_gcv_bag.RData")
load("RData/mars_gcv_mod.RData")
load("RData/mars_mod.RData")

# Need to load the RData, not sure how to do that....
##################
# This is important, I need to work through this. 

rs <- resamples(
  list(glmnet = glmn_mod, MARS = mars_mod,  bagged_MARS = mars_gcv_bag)
)

# Slide 62 -------------------------------------------------------

rs$values %>%
  dplyr::select(Resample, matches("RMSE$")) %>%
  gather(Model, RMSE, -Resample) %>%
  mutate(
    Model = gsub("~RMSE", "", Model, fixed = TRUE),
    Model = factor(as.character(Model), levels = c("glmnet", "MARS", "bagged_MARS"))
    ) %>%
  ggplot(aes(x = Model, y = RMSE, group = Resample, col = Resample)) + 
  geom_line(alpha = .35, lwd = 1) + 
  theme(legend.position = "none") + 
  xlab("")

# Slide 64 -------------------------------------------------------

library(tidyposterior)
rmse_mod <- perf_mod(rs, seed = 4344, iter = 5000, metric = "RMSE")

# Slide 65 -------------------------------------------------------

posteriors <- tidy(rmse_mod, seed = 366784)
summary(posteriors)

ggplot(posteriors) + coord_flip()

# Slide 66 -------------------------------------------------------

differences <-
  contrast_models(
    rmse_mod,
    list_1 = "bagged_MARS",
    list_2 = "MARS",
    seed = 2581
  )

ggplot(differences, size = 0.25)

# Slide 67 -------------------------------------------------------

summary(differences, size = 0.25) %>% 
  dplyr::select(contrast, mean, size, contains("pract"))

# Slide 69 -------------------------------------------------------

car_test <- car_test %>%
  mutate(pred = predict(mars_gcv_bag, car_test))

rmse(car_test, truth = mpg, estimate = pred)

ggplot(car_test, aes(x = mpg, y = pred, label = model)) +
  geom_abline(col = "green", alpha = .5) +
  geom_point(alpha = .3) +
  geom_smooth(se = FALSE, col = "red",
              lty = 2, lwd = .5, alpha = .5) +
  geom_text_repel(
    data = car_test  %>% dplyr::filter(abs(mpg - pred) > 10),
    segment.color = "grey50"
    )
