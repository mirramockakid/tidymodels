install.packages("modeldata")
install.packages("tidymodels")
library(tidymodels)


# get dataset
data(ames, package = "modeldata")

# === A. TRANSFORM === #
ames$Sale_Price <- log10(ames$Sale_Price)
# === END === #

# === B. SAMPLE === #
# split data - 80% training / 20% testing
# preserve distribution of "Sales Price" after split
set.seed(501)
ames_split <- ames |>
  rsample::initial_split(prop = 0.8, strata = "Sale_Price")

ames_train   <- rsample::training(ames_split)
ames_testing <- rsample::testing(ames_split)

# === END === #

# === C. FIT === #
# prepare model
lm_model <- parsnip::linear_reg() |>
  parsnip::set_engine("lm")

# fit model
lm_fit <- lm_model |>
  parsnip::fit(Sale_Price ~ Longitude + Latitude, data = ames_train)

# extract info
lm_fit |>
  parsnip::extract_fit_engine() |>
  summary()
#OR 
yardstick::tidy(lm_fit)
# === END === #


# === D. PREDICT === #
# prediction in test set with 95% CI
ames_testing |>
  select(Sale_Price) |>
  bind_cols(predict(lm_fit, new_data = ames_testing)) |>
  bind_cols(predict(lm_fit, new_data = ames_testing, type = "pred_int"))
# === END === #


# === E. WORKFLOW === #
# as above
lm_model <- parsnip::linear_reg() |>
  parsnip::set_engine("lm")

lm_workflow <- 
  workflows::workflow() |>
  workflows::add_model(lm_model) |>
  workflows::add_variables(outcomes = Sale_Price, predictors = c(Longitude, Latitude))

# FIT
lm_fit <- parsnip::fit(lm_workflow, ames_train)

# PREDICT
predict(lm_fit, ames_testing)
# === END === #


# === F. RECIPE === #
ames_recipe <-
  recipes::recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + 
                    Bldg_Type + Latitude + Longitude , data = ames_train) |>
  recipes::step_log(Gr_Liv_Area, base = 10) |>
  recipes::step_other(Neighborhood, threshold = 0.01) |>
  recipes::step_dummy(recipes::all_nominal_predictors()) |>
  recipes::step_interact(~ Gr_Liv_Area:starts_with("Bldg_Type_")) |>
  recipes::step_ns(Latitude, Longitude, deg_free = 20)

# look at recipe (tidy format)
# id can be set in step function and helps describe the step0
tidy(ames_recipe)

lm_model <- parsnip::linear_reg() |>
  parsnip::set_engine("lm")

lm_workflow <- 
  workflows::workflow() |>
  workflows::add_model(lm_model) |>
  workflows::add_recipe(ames_recipe)

# FIT
lm_fit <- parsnip::fit(lm_workflow, ames_train)

# PREDICT
predict(lm_fit, ames_testing)

# === END === #
