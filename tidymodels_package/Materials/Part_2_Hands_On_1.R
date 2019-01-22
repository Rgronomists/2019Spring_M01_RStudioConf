# Slides for Applied Machine Learning workshop at 2019 RStudio ---
# Conference -----------------------------------------------------

# Part_2_Basic_Principles - Hands-On: Some Basic Diagnostics #1

# ----------------------------------------------------------------

library(tidymodels)

# ----------------------------------------------------------------

library(AmesHousing)
ames <-
  make_ames() %>%
  # Remove quality-related predictors
  dplyr::select(-matches("Qu"))
nrow(ames)

# Make sure that you get the same random numbers
set.seed(4595)
data_split <- initial_split(ames, strata = "Sale_Price")
# NTS: initial_split is an rsample function.
#  It assumes you want 75% in the training set,
#  but you can set that with prop.
ames_train <- training(data_split)
# NTS: training is also an rsample function.

# ----------------------------------------------------------------

simple_lm <- lm(log10(Sale_Price) ~ Longitude + Latitude, data = ames_train)
simple_lm_values <- augment(simple_lm)

# ----------------------------------------------------------------

# From these results, let's take 10 minutes and do some visualizations:
#
#  - Plot the observed versus fitted values
head(simple_lm_values)

simple_lm_values %>%
  ggplot(aes(.fitted, log10.Sale_Price.)) + 
  geom_point() 

#  - Plot the residuals

# Histogram - looks normal to me
simple_lm_values %>%
  ggplot(aes(.resid)) + 
  geom_histogram()

# QQ-plot = approximately normal, slightly extreme values
simple_lm_values %>%
  ggplot(aes(sample = .resid)) + 
  geom_qq() + 
  geom_qq_line()
#
#  - Plot the predicted versus residuals
# NTS: I think in this case, predicted is the same as fitted.
#   It's a sausage
simple_lm_values %>%
  ggplot(aes(.fitted, .resid)) + 
  geom_point() 


# Are there any downsides to this approach?
