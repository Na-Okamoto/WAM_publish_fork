# PlayerIDごとに主観的thresholdとzスコア化confidenceの平均を紐付けてプロット
df_subjective_threshold <- df_MLE_result_binary %>%
    filter(params == "threshold") %>%
    select(PlayerID, subjective_threshold = value)

df_confidence <- df_rule_hit %>%
    group_by(PlayerID) %>%
    summarise(zConfidence = mean(scale(EstRuleConfidence), na.rm = TRUE))

df_thr_conf <- inner_join(df_subjective_threshold, df_confidence, by = "PlayerID")

p_thr_conf <- ggplot(df_thr_conf, aes(x = subjective_threshold, y = zConfidence)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", color = "black") +
    theme_fig_base +
    xlab("Subjective threshold") +
    ylab("Mean z-scored confidence") +
    ggtitle("Relationship between subjective threshold and z-scored confidence")

p_thr_conf %>% save_svg_figure("subjective_threshold_vs_zscored_confidence",
    analysis_group = "parameter_analysis",
    width = fig_1box_width_param, height = fig_1box_height, scaling = fig_anova_scale, unit = "mm"
)
