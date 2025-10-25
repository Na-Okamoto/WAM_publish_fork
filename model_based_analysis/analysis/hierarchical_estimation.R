#' Hierarchical Bayesian estimation for binary sign model
#'
#' This script loads the rule prediction task data and fits a hierarchical
#' Bayesian version of the binary sign model using cmdstanr.

if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  message("cmdstanr not found; attempting installation via scripts/install_cmdstanr.R")
  source(here::here("scripts", "install_cmdstanr.R"))
}
library(cmdstanr)
library(tidyverse)

if (!dir.exists(here::here("model_based_analysis", "R_result"))) {
  dir.create(here::here("model_based_analysis", "R_result"), recursive = TRUE)
}

# load data
rule_data <- read_csv(here::here("data", "df_rule_hit_switch.csv")) %>%
  mutate(
    PlayerID = as.factor(PlayerID),
    DisplayScore = if_else(DisplayScore < 0, 0, DisplayScore),
    behaviour = as.integer(EstRule == "random"),
    is_good = DisplayScore
  ) %>%
  arrange(PlayerID, TrialID) %>%
  filter(PlayerID %in% levels(PlayerID)[1:10]) %>%
  droplevels()

# indices of each participant's first trial in the filtered data
start_idx <- which(!duplicated(rule_data$PlayerID))
start_idx <- c(start_idx, nrow(rule_data) + 1)

stan_data <- list(
  J = nlevels(rule_data$PlayerID),
  N = nrow(rule_data),
  subj = as.integer(rule_data$PlayerID),
  distance = rule_data$Distance,
  is_good = rule_data$is_good,
  behaviour = rule_data$behaviour,
  start_idx = start_idx,
  step_slope = 10
)

model_file <- here::here("model_based_analysis", "stan", "hierarchical_binary_model.stan")
mod <- cmdstan_model(model_file)

fit <- mod$sample(
  data = stan_data,
  iter_warmup = 20,
  iter_sampling = 20,
  chains = 2,
  parallel_chains = 2
)
summary_fit <- fit$summary()
print(summary_fit)
converged <- all(summary_fit$rhat < 1.1, na.rm = TRUE)
cat("Converged:", converged, "\n")
saveRDS(fit, file = here::here("model_based_analysis", "R_result", "hierarchical_fit_demo.rds"))
