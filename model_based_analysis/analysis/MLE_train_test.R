library(tidyverse)
library(here)
library(rio)

source(here::here("model_based_analysis","model","model_definition.R"))
source(here::here("model_based_analysis","model","model_utility.R"))
source(here::here("model_based_analysis","preprocess","MLE_preprocess.R"))

# Split each participant's data into training and test sets at the midpoint of trials
split_half <- function(df) {
  df %>%
    group_by(PlayerID) %>%
    arrange(TrialID) %>%
    mutate(trial_index = row_number(),
           split_point = floor(max(trial_index) / 2),
           dataset = if_else(trial_index <= split_point, "train", "test")) %>%
    ungroup()
}

# Fit model on training data and evaluate on test data
fit_and_evaluate <- function(train_df, test_df, model_obj, input_col, output_rule) {
  est_result <- fit_all_participants(train_df, model_obj, input_col, output_rule)

  eval_results <- unique(test_df$PlayerID) %>%
    map_dfr(function(p_) {
      each_df <- test_df %>% filter(PlayerID == p_)
      theta <- each_df %>% pull(threshold) %>% unique()
      stopifnot(length(theta) == 1)

      input <- each_df %>% pull({{input_col}}) %>% as.vector() %>% unlist()
      is_good <- each_df$DisplayScore
      behaviour <- each_df$EstRule == output_rule

      params <- est_result %>%
        filter(PlayerID == p_) %>%
        select(all_of(model_obj$param_name_list)) %>%
        as.numeric()

      if (!("threshold" %in% model_obj$param_name_list)) {
        params <- c(params, theta)
      }

      pred <- function(input, params) model_obj$predict_choice(input, params, is_good)
      log_lik <- log_lik_bernoulli(params, input, behaviour, pred)
      accuracy <- mean((pred(input, params) > 0.5) == behaviour)

      tibble(PlayerID = p_,
             test_log_likelihood = log_lik,
             test_accuracy = accuracy)
    })

  list(estimation = est_result, evaluation = eval_results)
}

# ensure output directory exists
result_dir <- here::here("model_based_analysis", "R_result")
if (!dir.exists(result_dir)) {
  dir.create(result_dir, recursive = TRUE)
}

# prepare train and test data
df_rule_hit_modify <- df_rule_hit %>% mutate(threshold = 1 * true_threshold)
splitted <- split_half(df_rule_hit_modify)
train_df <- splitted %>% filter(dataset == "train")
test_df <- splitted %>% filter(dataset == "test")

model_obj_list <- c(
  binary_sign_model_obj = binary_sign_model_obj,
  binary_sign_no_gamma_model_obj = binary_sign_no_gamma_model_obj,
  binary_sign_true_theta_model_obj = binary_sign_true_theta_model_obj,
  binary_sign_common_alpha_model_obj = binary_sign_common_alpha_model_obj,
  binary_sign_common_beta_model_obj = binary_sign_common_beta_model_obj,
  binary_sign_common_alpha_common_beta_model_obj = binary_sign_common_alpha_common_beta_model_obj
)

for (col in c("Distance")) {
  message(sprintf("processing input %s", col))
  for (model_name in names(model_obj_list)) {
    message(sprintf("fitting %s", model_name))
    model_obj <- model_obj_list[[model_name]]
    res <- fit_and_evaluate(train_df, test_df, model_obj, col, "random")
    est_result <- res$estimation
    eval_result <- res$evaluation
    export(est_result, here::here("model_based_analysis", sprintf("R_result/%s_%s_random_train_estimation.rds", model_name, col)))
    export(eval_result, here::here("model_based_analysis", sprintf("R_result/%s_%s_random_test_evaluation.rds", model_name, col)))
    message(sprintf("completed %s", model_name))
  }
}

message("MLE train/test split finished")

# 全モデル分のtrain estimation結果を一括ロード
train_estimation_list <- list()
for (model_name in names(model_obj_list)) {
  file_path <- here::here("model_based_analysis", "R_result", sprintf("%s_Distance_random_train_estimation.rds", model_name))
  if (file.exists(file_path)) {
    train_estimation_list[[model_name]] <- rio::import(file_path) %>% as_tibble()
    message(sprintf("Loaded: %s", file_path))
  } else {
    message(sprintf("File not found: %s", file_path))
  }
}

# concatenate all train estimation results into a single tibble
train_estimation_df <- bind_rows(train_estimation_list, .id = "model_name")
train_estimation_df %>%
  group_by(model_name) %>%
  summarise(
    num_participants = n_distinct(PlayerID),
    mean_log_likelihood = mean(log_likelihood, na.rm = TRUE),
    mean_AIC = mean(AIC, na.rm = TRUE)
  ) %>%
  # rename  model name for clarity
  mutate(model_name = case_when(
    model_name == "binary_sign_model_obj" ~ "full",
    model_name == "binary_sign_no_gamma_model_obj" ~ "no accumulation",
    model_name == "binary_sign_common_beta_model_obj" ~ "common constant term",
    model_name == "binary_sign_common_alpha_model_obj" ~ "common error sensitivity",
    model_name == "binary_sign_common_alpha_common_beta_model_obj" ~ "common error sensitivity, constant term",
    model_name == "binary_sign_true_theta_model_obj" ~ "true threshold",
    TRUE ~ model_name
  )) %>%
  arrange(desc(mean_log_likelihood))
