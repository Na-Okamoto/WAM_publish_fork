source(here::here("model_based_analysis", "model", "param_recovery_utility.R"))

run_parameter_recovery <- function(model_obj_list, input_cols = c("Distance"),
                                   output_rule = "random", iteration = 100) {
  for (col in input_cols) {
    print(col)
    for (model_name in names(model_obj_list)) {
      model_obj <- model_obj_list[[model_name]]
      print(model_name)

      file <-
        here::here(
          "model_based_analysis",
          "R_result",
          paste0(model_name, "_", col, "_", output_rule, "_estimation.rds")
        )
      print(file)
      df_MLE_result_tmp <- import(file) %>% mutate(PlayerID = as.factor(PlayerID))
      print("file import done")

      df_param_stat_tmp <- df_MLE_result_tmp %>%
        pivot_longer(
          cols = c(-PlayerID, -number_of_params, -AIC, -log_likelihood),
          names_to = "param", values_to = "value"
        ) %>%
        group_by(param) %>%
        summarise(mean = mean(value), sd = sd(value)) %>%
        mutate(order_key = match(param, model_obj$param_name_list)) %>%
        arrange(order_key) %>%
        select(-order_key)

      param_mean <- df_param_stat_tmp %>% pull(mean)
      param_sd <- df_param_stat_tmp %>% pull(sd)

      recovery_model(
        df_rule_hit,
        model_obj,
        model_name,
        col,
        output_rule,
        iteration,
        param_mean,
        param_sd
      )
    }
  }

  file_lists <- list.files(
    here::here("model_based_analysis", "R_result"),
    pattern = sprintf(".*parameter_recovery_iter_%d.rds", iteration),
    full.names = TRUE
  )

  for (filename in file_lists) {
    print(filename)
    create_param_recovery_figs(filename)
  }
}

model_obj_list <- c(
  binary_sign_model_obj = binary_sign_model_obj,
  binary_sign_no_gamma_model_obj = binary_sign_no_gamma_model_obj,
  binary_sign_true_theta_model_obj = binary_sign_true_theta_model_obj,
  binary_sign_common_alpha_model_obj = binary_sign_common_alpha_model_obj,
  binary_sign_common_beta_model_obj = binary_sign_common_beta_model_obj,
  binary_sign_common_alpha_common_beta_model_obj = binary_sign_common_alpha_common_beta_model_obj
)

run_parameter_recovery(model_obj_list)
