library(rio)
library(dplyr)
library(here)

# Load the dataset containing all trials
df_all <- rio::import(here::here("data", "df_all.csv"))

# filter the df_all data for practice trials
df_practice <- df_all %>%
    filter(GameMode == "ruleInferenceTraining", IsHit == TRUE) %>%
    mutate(
        EstRule = factor(EstRule, levels = c("skill", "random")),
        TrueRule = factor(TrueRule, levels = c("skill", "random")),
        PlayerID = as.factor(PlayerID)
    )

# Save the filtered practice data
rio::export(df_practice, here::here("data", "df_practice.csv"))
