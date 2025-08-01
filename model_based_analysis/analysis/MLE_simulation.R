library(tidyverse)
library(rio)
library(here)

# load model definitions and utility functions
source(here("model_based_analysis", "model", "model_definition.R"))
source(here("model_based_analysis", "model", "model_utility.R"))
source(here("model_based_analysis", "model", "model_prediction.R"))
# source(here("model_based_analysis", "model", "simulation.R"))
# data preprocessing provides df_rule_hit
source(here("model_based_analysis", "preprocess", "MLE_preprocess.R"))

run_simulation_and_mle <- function(simulation_model_name, model_obj_list, df_rule_hit) {
  param_file <- fs::path("R_result", paste0(simulation_model_name, "_Distance_random_estimation.rds"))
  sim_df <- df_rule_hit %>%
    predict_from_file(param_file, c("Distance")) %>%
    mutate(behaviour = as.numeric(EstRule == "random"), input = "Distance") %>%
    select(
      PlayerID, TrialID,
      TrialsAfterSwitchToSkill, TrialsAfterSwitchToRandom,
      behaviour, pred, input,
      zConfidence, EstRuleConfidence,
      DisplayScore,
      TrueRule, EstRule,
      any_of(c("state", "error")),
      Distance,
      true_threshold,
      all_of(c("Distance")),
      data
    ) %>%
    select(
      "PlayerID", "TrialID",
      "TrialsAfterSwitchToSkill", "TrialsAfterSwitchToRandom", "pred", "DisplayScore",
      "TrueRule", "Distance", "true_threshold"
    ) %>%
    mutate(
      pred_adj = if_else(abs(pred) < 1, pred, pred - 10^(-5)),
      entropy = (pred_adj * log(pred_adj) + (1 - pred_adj) * log(1 - pred_adj)),
      EstRule = if_else(pred_adj > 0.5, "random", "skill"),
      threshold = true_threshold
    ) %>%
    group_by(PlayerID) %>%
    mutate(
      zConfidence = scale(entropy, center = TRUE, scale = TRUE)
    ) %>%
    ungroup() %>%
    select(
      PlayerID, TrialID,
      TrialsAfterSwitchToSkill, TrialsAfterSwitchToRandom,
      DisplayScore,
      TrueRule, EstRule, pred,
      Distance, threshold,
      zConfidence
    )
  print(paste0("MLE simulation started for ", simulation_model_name))
  for (model_name in names(model_obj_list)) {
    cat(sprintf("fitting %s\n", model_name))
    model_obj <- model_obj_list[[model_name]]
    estimate_model(
      sim_df,
      model_obj,
      model_name,
      "Distance",
      "random",
      suffix = simulation_model_name
    )
  }
  print(paste0("MLE simulation ended for ", simulation_model_name))
}

model_obj_list <- c(
  binary_sign_model_obj = binary_sign_model_obj,
  binary_sign_no_gamma_model_obj = binary_sign_no_gamma_model_obj,
  binary_sign_true_theta_model_obj = binary_sign_true_theta_model_obj,
  binary_sign_common_alpha_model_obj = binary_sign_common_alpha_model_obj,
  binary_sign_common_beta_model_obj = binary_sign_common_beta_model_obj,
  binary_sign_common_alpha_common_beta_model_obj = binary_sign_common_alpha_common_beta_model_obj
)

simulation_model_names <- names(model_obj_list)

for (sim_model in simulation_model_names) {
  run_simulation_and_mle(sim_model, model_obj_list, df_rule_hit)
}

est_result_files <- list.files(
  here("model_based_analysis", "R_result"),
  pattern = "binary_sign.*Distance_random_.*_estimation\\.rds",
  full.names = TRUE
)

est_results <-
  map(est_result_files, ~ {
    df <- rio::import(.x)
    file <- basename(.x)
    df$name <- stringr::str_extract(file, ".*(?=_Distance_random_)")
    df$simulation <- stringr::str_extract(file, "(?<=_Distance_random_).*(?=_estimation)")
    df
  }) %>%
  bind_rows() %>%
  as_tibble() %>%
  mutate(PlayerID = as.factor(PlayerID)) %>%
  mutate(
    name = case_when(
      name == "binary_sign_model" ~ "full",
      name == "binary_sign_no_gamma_model" ~ "no accumulation",
      name == "binary_sign_common_beta_model" ~ "common bias",
      name == "binary_sign_common_alpha_model" ~ "common weight",
      name == "binary_sign_common_alpha_common_beta_model" ~ "symmetric",
      name == "binary_sign_true_theta_model" ~ "fixed threshold",
      TRUE ~ name
    ),
    simulation = case_when(
      simulation == "binary_sign_model_obj" ~ "full",
      simulation == "binary_sign_no_gamma_model_obj" ~ "no accumulation",
      simulation == "binary_sign_common_beta_model_obj" ~ "common bias",
      simulation == "binary_sign_common_alpha_model_obj" ~ "common weight",
      simulation == "binary_sign_common_alpha_common_beta_model_obj" ~ "symmetric",
      simulation == "binary_sign_true_theta_model_obj" ~ "fixed threshold",
      TRUE ~ simulation
    )
  )

p_AIC_boxplot_across_models_for_simulated <- est_results %>%
  ggplot(aes(x = name, y = AIC, fill = name)) +
  geom_boxplot() +
  theme_fig_boxplot +
  xlab("model") +
  ylab("AIC") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~simulation)

p_AIC_boxplot_across_models_for_simulated %>%
  save_svg_figure("AIC boxplot across models for each simulated data",
    width = fig_timeseries_width,
    height = fig_timeseries_height,
    scaling = fig_anova_scale, unit = "mm",
    analysis_group = "simulated_data"
  )
