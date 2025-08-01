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


# step 1: load parameter estimates of the full model
full_param_file <- here("model_based_analysis", "R_result", "binary_sign_model_obj_Distance_random_estimation.rds")

sim_df <- df_rule_hit %>%
  predict_from_file(
    fs::path("R_result", "binary_sign_model_obj_Distance_random_estimation.rds"),
    c("Distance")
  ) %>%
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
    # DisplayScore = if_else(DisplayScore > 0, "positive", "negative"),
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


# step 4: list up models to fit
model_obj_list <- c(
  # binary_sign_model_obj = binary_sign_model_obj,
  binary_sign_no_gamma_model_obj = binary_sign_no_gamma_model_obj,
  binary_sign_true_theta_model_obj = binary_sign_true_theta_model_obj,
  binary_sign_common_alpha_model_obj = binary_sign_common_alpha_model_obj,
  binary_sign_common_beta_model_obj = binary_sign_common_beta_model_obj,
  binary_sign_common_alpha_common_beta_model_obj = binary_sign_common_alpha_common_beta_model_obj
)

# step 5: fit each model to the simulated data
print("MLE simulation started")
df_sim_for_mle <- sim_df

# cluster <- makeCluster(
#   min(10, getOption("mc.cores", detectCores())),
#   outfile = ""
# )
# registerDoParallel(cluster)
# on.exit(stopCluster(cluster), add = TRUE)
for (model_name in names(model_obj_list)) {
  cat(sprintf("fitting %s\n", model_name))
  model_obj <- model_obj_list[[model_name]]
  estimate_model(
    df_sim_for_mle,
    model_obj,
    model_name,
    "Distance",
    "random",
    suffix = "simulated_data"
  )
}
print("MLE simulation ended")

# compare the results across models
# list up results to be compared
est_result_files <- list.files(
  here("model_based_analysis", "R_result"),
  pattern = "binary_sign.*Distance_random_simulated_data_estimation\\.rds",
  full.names = TRUE
)

# import and combine results
est_results <-
  map2(est_result_files, est_result_files, ~ {
    df <- rio::import(.x)
    # ファイル名からモデル名部分を抽出（例: "binary_sign_no_gamma_model_obj"）
    model_name <- stringr::str_extract(basename(.y), ".*(?=_obj_Distance_random_simulated_data_estimation)")
    df$name <- model_name
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
    )
  )
# mutate(name = gsub("(_estimation|model_|obj_)", "", name)) %>%
# mutate(name = gsub(" $", "", name))
est_results %>%
  pull(name) %>%
  unique()

p_AIC_boxplot_across_models_for_simulated <- est_results %>%
  ggplot(aes(x = name, y = AIC, fill = name)) +
  geom_boxplot() +
  theme_fig_boxplot +
  xlab("model") +
  ylab("AIC") +
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position = "none") +
  coord_flip()


p_AIC_boxplot_across_models_for_simulated %>%
  save_svg_figure("AIC boxplot across models for simulated data",
    width = fig_timeseries_width,
    height = fig_timeseries_height,
    scaling = fig_anova_scale, unit = "mm",
    analysis_group = "simulated_data"
  )
