##KAPLAN_LIVER##
############################################################
### 1) CHECK FACTORS
############################################################

df_liver$bmi_binary <- factor(
  df_liver$bmi_binary,
  levels = c("Non-obesity", "Obesity")
)

df_liver$lancet_binary <- factor(
  df_liver$lancet_binary,
  levels = c("Non-obesity", "Obesity")
)

df_liver$lancet_stage <- factor(
  df_liver$lancet_stage,
  levels = c("No obesity", "Preclinical obesity", "Clinical obesity")
)

############################################################
### 2) COMMON SETTINGS
############################################################

x_max <- 15
x_breaks <- seq(0, 15, 3)

km_theme <- theme_classic(base_size = 15) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    plot.title = element_blank(),
    panel.grid = element_blank()
  )

risk_theme <- theme_classic(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    text = element_text(color = "black"),
    axis.text.x = element_text(color = "black"),
    axis.title.x = element_blank()
  )

############################################################
### 3) KM 1 — BMI-BASED OBESITY
############################################################

fit_bmi <- survfit(
  Surv(followup_liver, event) ~ bmi_binary,
  data = df_liver
)

p_bmi <- ggsurvplot(
  fit_bmi,
  data = df_liver,
  fun = "event",
  conf.int = TRUE,
  conf.int.alpha = 0.15,
  censor = FALSE,
  risk.table = TRUE,
  risk.table.title = "Number at risk",
  risk.table.y.text = TRUE,
  risk.table.y.text.col = TRUE,
  pval = TRUE,
  pval.method = FALSE,
  legend.title = "",
  legend.labs = c("Non-obesity", "Obesity"),
  palette = c("grey50", "#1F77B4"),
  xlab = "Follow-up (years)",
  ylab = "Cumulative incidence of liver-related events (%)",
  break.time.by = 3,
  xlim = c(0, 15),
  risk.table.height = 0.22,
  ggtheme = km_theme
)

p_bmi$plot <- p_bmi$plot +
  scale_x_continuous(breaks = x_breaks, limits = c(0, 15)) +
  scale_y_continuous(
    labels = label_percent(accuracy = 0.1),
    limits = c(0, 0.03)
  ) +
  theme(legend.position = "top")

p_bmi$table <- p_bmi$table +
  scale_x_continuous(breaks = x_breaks, limits = c(0, 15)) +
  risk_theme

############################################################
### 4) KM 2 — LANCET CRITERIA-BASED OBESITY
############################################################

fit_lancet_bin <- survfit(
  Surv(followup_liver, event) ~ lancet_binary,
  data = df_liver
)

p_lancet_bin <- ggsurvplot(
  fit_lancet_bin,
  data = df_liver,
  fun = "event",
  conf.int = TRUE,
  conf.int.alpha = 0.15,
  censor = FALSE,
  risk.table = TRUE,
  risk.table.title = "Number at risk",
  risk.table.y.text = TRUE,
  risk.table.y.text.col = TRUE,
  pval = TRUE,
  pval.method = FALSE,
  legend.title = "",
  legend.labs = c("Non-obesity", "Obesity"),
  palette = c("grey50", "#1F77B4"),
  xlab = "Follow-up (years)",
  ylab = "Cumulative incidence of liver-related events (%)",
  break.time.by = 3,
  xlim = c(0, 15),
  risk.table.height = 0.22,
  ggtheme = km_theme
)

p_lancet_bin$plot <- p_lancet_bin$plot +
  scale_x_continuous(breaks = x_breaks, limits = c(0, 15)) +
  scale_y_continuous(
    labels = label_percent(accuracy = 0.1),
    limits = c(0, 0.03)
  ) +
  theme(legend.position = "top")

p_lancet_bin$table <- p_lancet_bin$table +
  scale_x_continuous(breaks = x_breaks, limits = c(0, 15)) +
  risk_theme

############################################################
### 5) KM 3 — LANCET STAGING
############################################################

fit_lancet_stage <- survfit(
  Surv(followup_liver, event) ~ lancet_stage,
  data = df_liver
)

p_lancet_stage <- ggsurvplot(
  fit_lancet_stage,
  data = df_liver,
  fun = "event",
  conf.int = TRUE,
  conf.int.alpha = 0.15,
  censor = FALSE,
  risk.table = TRUE,
  risk.table.title = "Number at risk",
  risk.table.y.text = TRUE,
  risk.table.y.text.col = TRUE,
  pval = TRUE,
  pval.method = FALSE,
  legend.title = "",
  legend.labs = c("No obesity", "Preclinical obesity", "Clinical obesity"),
  palette = c("grey50", "#FFC300", "#A93226"),
  xlab = "Follow-up (years)",
  ylab = "Cumulative incidence of liver-related events (%)",
  break.time.by = 3,
  xlim = c(0, 15),
  risk.table.height = 0.22,
  ggtheme = km_theme
)

p_lancet_stage$plot <- p_lancet_stage$plot +
  scale_x_continuous(breaks = x_breaks, limits = c(0, 15)) +
  scale_y_continuous(
    labels = label_percent(accuracy = 0.1),
    limits = c(0, 0.03)
  ) +
  theme(legend.position = "top")

p_lancet_stage$table <- p_lancet_stage$table +
  scale_x_continuous(breaks = x_breaks, limits = c(0, 15)) +
  risk_theme

############################################################
### 6) PRINT
############################################################

print(p_bmi)
print(p_lancet_bin)
print(p_lancet_stage)

############################################################
### 7) SAVE COMPLETE FIGURES (PLOT + RISK TABLE)
############################################################

bmi_full <- arrange_ggsurvplots(list(p_bmi), print = FALSE)
lancet_bin_full <- arrange_ggsurvplots(list(p_lancet_bin), print = FALSE)
lancet_stage_full <- arrange_ggsurvplots(list(p_lancet_stage), print = FALSE)

ggsave(
  filename = "C:/Users/nicol/Desktop/KM_liver_BMI_binary_full.png",
  plot = bmi_full,
  width = 10,
  height = 8,
  dpi = 400
)

ggsave(
  filename = "C:/Users/nicol/Desktop/KM_liver_Lancet_binary_full.png",
  plot = lancet_bin_full,
  width = 10,
  height = 8,
  dpi = 400
)

ggsave(
  filename = "C:/Users/nicol/Desktop/KM_liver_Lancet_stage_full.png",
  plot = lancet_stage_full,
  width = 10,
  height = 8,
  dpi = 400
)

############################################################
### 8) OPTIONAL: SAVE MAIN PLOTS ONLY
############################################################

ggsave(
  filename = "C:/Users/nicol/Desktop/KM_liver_BMI_binary_plot_only.png",
  plot = p_bmi$plot,
  width = 8,
  height = 6,
  dpi = 400
)

ggsave(
  filename = "C:/Users/nicol/Desktop/KM_liver_Lancet_binary_plot_only.png",
  plot = p_lancet_bin$plot,
  width = 8,
  height = 6,
  dpi = 400
)

ggsave(
  filename = "C:/Users/nicol/Desktop/KM_liver_Lancet_stage_plot_only.png",
  plot = p_lancet_stage$plot,
  width = 8,
  height = 6,
  dpi = 400
)


