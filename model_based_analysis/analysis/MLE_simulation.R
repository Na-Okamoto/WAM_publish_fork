library(tidyverse)
library(rio)
library(here)

# load model definitions and utility functions
source(here("model_based_analysis","model","model_definition.R"))
source(here("model_based_analysis","model","model_utility.R"))
source(here("model_based_analysis","model","simulation.R"))
# data preprocessing provides df_rule_hit
source(here("model_based_analysis","preprocess","MLE_preprocess.R"))

# step 1: load parameter estimates of the full model
full_param_file <- here("model_based_analysis","R_result","binary_sign_model_obj_Distance_random_estimation.rds")
full_params <- import(full_param_file) %>%
  as_tibble() %>%
  mutate(PlayerID = as.factor(PlayerID))

# keep only parameters required for prediction
param_cols <- c("PlayerID","a_G","a_B","b_G","b_B","gamma","threshold")
full_params <- full_params %>% select(any_of(param_cols))

# step 2: simulate behaviour for each participant
sim_df <- simulate_model(df_rule_hit, full_params, binary_sign_model_obj, "Distance")

# step 3: format the simulated data so that MLE can be applied
sim_for_mle <- sim_df %>%
  mutate(
    DisplayScore = if_else(DisplayScore == "positive", 1, 0),
    EstRule = if_else(output, "random", "skill"),
    threshold = true_threshold
  ) %>%
  select(PlayerID, Distance, DisplayScore, EstRule, threshold)

# step 4: list up models to fit
model_obj_list <- c(
  binary_sign_model_obj = binary_sign_model_obj,
  binary_sign_no_gamma_model_obj = binary_sign_no_gamma_model_obj,
  binary_sign_true_theta_model_obj = binary_sign_true_theta_model_obj,
  binary_sign_common_alpha_model_obj = binary_sign_common_alpha_model_obj,
  binary_sign_common_beta_model_obj = binary_sign_common_beta_model_obj,
  binary_sign_common_alpha_common_beta_model_obj = binary_sign_common_alpha_common_beta_model_obj
)

# step 5: fit each model to the simulated data
df_sim_for_mle <- sim_for_mle
for (model_name in names(model_obj_list)) {
  cat(sprintf("fitting %s\n", model_name))
  model_obj <- model_obj_list[[model_name]]
  estimate_model(df_sim_for_mle, model_obj, model_name, "Distance", "random", suffix = "simulated_data")
}
