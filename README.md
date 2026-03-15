#################################
##### FOREST_PLOT_CANCER ########
#################################

df_cancer_adj_forest <- df_cancer %>%
  filter(
    !is.na(age),
    !is.na(sex),
    !is.na(smoking),
    !is.na(alcohol_g_day),
    !is.na(followup_cancer),
    !is.na(cancer_event),
    !is.na(bmi_binary),
    !is.na(lancet_binary),
    !is.na(lancet_stage)
  )

cox_bmi_unadj_fp <- coxph(
  Surv(followup_cancer, cancer_event) ~ bmi_binary,
  data = df_cancer
)

cox_bmi_adj_fp <- coxph(
  Surv(followup_cancer, cancer_event) ~
    bmi_binary + age + sex + smoking + alcohol_g_day,
  data = df_cancer_adj_forest
)

cox_lancet_bin_unadj_fp <- coxph(
  Surv(followup_cancer, cancer_event) ~ lancet_binary,
  data = df_cancer
)

cox_lancet_bin_adj_fp <- coxph(
  Surv(followup_cancer, cancer_event) ~
    lancet_binary + age + sex + smoking + alcohol_g_day,
  data = df_cancer_adj_forest
)

cox_lancet_stage_unadj_fp <- coxph(
  Surv(followup_cancer, cancer_event) ~ lancet_stage,
  data = df_cancer
)

cox_lancet_stage_adj_fp <- coxph(
  Surv(followup_cancer, cancer_event) ~
    lancet_stage + age + sex + smoking + alcohol_g_day,
  data = df_cancer_adj_forest
)

fmt_hr <- function(est, low, high){
  sprintf("%.2f (%.2f–%.2f)", est, low, high)
}

bmi_unadj <- tidy(cox_bmi_unadj_fp, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("^bmi_binary", term)) %>%
  mutate(
    definition = "BMI-based obesity",
    category = "Obesity"
  ) %>%
  transmute(definition, category,
            est_unadj = estimate,
            low_unadj = conf.low,
            high_unadj = conf.high)

bmi_adj <- tidy(cox_bmi_adj_fp, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("^bmi_binary", term)) %>%
  mutate(
    definition = "BMI-based obesity",
    category = "Obesity"
  ) %>%
  transmute(definition, category,
            est_adj = estimate,
            low_adj = conf.low,
            high_adj = conf.high)

lancet_bin_unadj <- tidy(cox_lancet_bin_unadj_fp, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("^lancet_binary", term)) %>%
  mutate(
    definition = "Lancet criteria-based obesity",
    category = "Obesity"
  ) %>%
  transmute(definition, category,
            est_unadj = estimate,
            low_unadj = conf.low,
            high_unadj = conf.high)

lancet_bin_adj <- tidy(cox_lancet_bin_adj_fp, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("^lancet_binary", term)) %>%
  mutate(
    definition = "Lancet criteria-based obesity",
    category = "Obesity"
  ) %>%
  transmute(definition, category,
            est_adj = estimate,
            low_adj = conf.low,
            high_adj = conf.high)

lancet_stage_unadj <- tidy(cox_lancet_stage_unadj_fp, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("^lancet_stage", term)) %>%
  mutate(
    definition = "Lancet obesity staging",
    category = c("Preclinical obesity", "Clinical obesity")
  ) %>%
  transmute(definition, category,
            est_unadj = estimate,
            low_unadj = conf.low,
            high_unadj = conf.high)

lancet_stage_adj <- tidy(cox_lancet_stage_adj_fp, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("^lancet_stage", term)) %>%
  mutate(
    definition = "Lancet obesity staging",
    category = c("Preclinical obesity", "Clinical obesity")
  ) %>%
  transmute(definition, category,
            est_adj = estimate,
            low_adj = conf.low,
            high_adj = conf.high)

forest_df <- tibble(
  definition = c(
    "BMI-based obesity", "BMI-based obesity",
    "Lancet criteria-based obesity", "Lancet criteria-based obesity",
    "Lancet obesity staging", "Lancet obesity staging", "Lancet obesity staging"
  ),
  category = c(
    "No obesity", "Obesity",
    "No obesity", "Obesity",
    "No obesity", "Preclinical obesity", "Clinical obesity"
  ),
  y = c(5.6, 4.9, 4.0, 3.3, 2.4, 1.7, 1.0)
) %>%
  left_join(bmi_unadj, by = c("definition", "category")) %>%
  left_join(bmi_adj, by = c("definition", "category")) %>%
  rows_update(lancet_bin_unadj, by = c("definition", "category")) %>%
  rows_update(lancet_bin_adj, by = c("definition", "category")) %>%
  rows_update(lancet_stage_unadj, by = c("definition", "category")) %>%
  rows_update(lancet_stage_adj, by = c("definition", "category"))

left_df <- forest_df %>%
  mutate(
    def_display = ifelse(category == "No obesity", definition, ""),
    cat_display = category,
    hr_unadj_display = ifelse(category == "No obesity", "Reference",
                              fmt_hr(est_unadj, low_unadj, high_unadj)),
    hr_adj_display = ifelse(category == "No obesity", "Reference",
                            fmt_hr(est_adj, low_adj, high_adj))
  )

p_left <- ggplot(left_df, aes(y = y)) +
  xlim(0, 1) +
  ylim(0.7, 6.2) +
  theme_void() +
  annotate("text", x = 0.00, y = 6.05, label = "Definition",
           hjust = 0, fontface = "bold", size = 4.2) +
  annotate("text", x = 0.33, y = 6.05, label = "Category",
           hjust = 0, fontface = "bold", size = 4.2) +
  annotate("text", x = 0.58, y = 6.05, label = "Unadjusted HR (95% CI)",
           hjust = 0, fontface = "bold", size = 4.2) +
  annotate("text", x = 0.83, y = 6.05, label = "Adjusted HR (95% CI)",
           hjust = 0, fontface = "bold", size = 4.2) +
  geom_text(aes(x = 0.00, label = def_display),
            hjust = 0, size = 4, fontface = "bold") +
  geom_text(aes(x = 0.33, label = cat_display),
            hjust = 0, size = 4) +
  geom_text(aes(x = 0.58, label = hr_unadj_display),
            hjust = 0, size = 4) +
  geom_text(aes(x = 0.83, label = hr_adj_display),
            hjust = 0, size = 4)

p_right <- ggplot(forest_df, aes(y = y)) +
  geom_vline(xintercept = 1, linetype = "dashed",
             colour = "grey40", linewidth = 0.7) +
  geom_errorbarh(
    data = forest_df %>% filter(!is.na(est_unadj)),
    aes(xmin = low_unadj, xmax = high_unadj, color = "Unadjusted"),
    height = 0.08, linewidth = 1.1,
    position = position_nudge(y = 0.10)
  ) +
  geom_point(
    data = forest_df %>% filter(!is.na(est_unadj)),
    aes(x = est_unadj, color = "Unadjusted"),
    size = 3.8,
    position = position_nudge(y = 0.10)
  ) +
  geom_errorbarh(
    data = forest_df %>% filter(!is.na(est_adj)),
    aes(xmin = low_adj, xmax = high_adj, color = "Adjusted"),
    height = 0.08, linewidth = 1.1,
    position = position_nudge(y = -0.10)
  ) +
  geom_point(
    data = forest_df %>% filter(!is.na(est_adj)),
    aes(x = est_adj, color = "Adjusted"),
    size = 3.8,
    position = position_nudge(y = -0.10)
  ) +
  scale_color_manual(
    values = c("Unadjusted" = "grey45", "Adjusted" = "#D18F00")
  ) +
  scale_x_continuous(
    trans = "log10",
    breaks = c(0.5, 1, 2, 3),
    limits = c(0.5, 3),
    labels = c("0.5", "1", "2", "3")
  ) +
  scale_y_continuous(
    breaks = forest_df$y,
    labels = rep("", nrow(forest_df)),
    limits = c(0.7, 6.2)
  ) +
  labs(x = "Hazard ratio (95% CI), log scale",
       y = NULL, color = NULL) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = c(0.82, 0.90),
    legend.background = element_rect(color = "grey70", fill = "white"),
    legend.text = element_text(size = 12),
    legend.key.size = unit(1.3, "lines"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

p_final <- p_left + p_right +
  plot_layout(widths = c(2.8, 1.35))

ggsave(
  "forest_plot_cancer_models_final.png",
  p_final,
  width = 14,
  height = 4.5,
  dpi = 400
)

p_final

############################################################
### KM_Cancer
############################################################

df_cancer$bmi_binary <- factor(
  df_cancer$bmi_binary,
  levels = c("No obesity", "Obesity")
)

df_cancer$lancet_binary <- factor(
  df_cancer$lancet_binary,
  levels = c("No obesity", "Obesity")
)

df_cancer$lancet_stage <- factor(
  df_cancer$lancet_stage,
  levels = c("No obesity", "Preclinical obesity", "Clinical obesity")
)

############################################################
### 2) COMMON SETTINGS
############################################################

x_max <- 13
x_breaks <- seq(0, 13, 3)

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
  Surv(followup_cancer, cancer_event) ~ bmi_binary,
  data = df_cancer
)

p_bmi <- ggsurvplot(
  fit_bmi,
  data = df_cancer,
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
  legend.labs = c("No obesity", "Obesity"),
  palette = c("grey50", "#1F77B4"),
  xlab = "Follow-up (years)",
  ylab = "Cumulative incidence of solid cancer (%)",
  break.time.by = 2,
  xlim = c(0, 13),
  risk.table.height = 0.22,
  ggtheme = km_theme
)

p_bmi$plot <- p_bmi$plot +
  scale_x_continuous(breaks = x_breaks, limits = c(0, 13)) +
  scale_y_continuous(
    labels = label_percent(accuracy = 0.1),
    limits = c(0, 0.17),
    breaks = seq(0, 0.17, 0.03)
  )
  theme(legend.position = "top")

p_bmi$table <- p_bmi$table +
  scale_x_continuous(breaks = x_breaks, limits = c(0, 13)) +
  risk_theme

############################################################
### 4) KM 2 — LANCET CRITERIA-BASED OBESITY
############################################################

fit_lancet_bin <- survfit(
  Surv(followup_cancer, cancer_event) ~ lancet_binary,
  data = df_cancer
)

p_lancet_bin <- ggsurvplot(
  fit_lancet_bin,
  data = df_cancer,
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
  legend.labs = c("No obesity", "Obesity"),
  palette = c("grey50", "#1F77B4"),
  xlab = "Follow-up (years)",
  ylab = "Cumulative incidence of solid cancer (%)",
  break.time.by = 2,
  xlim = c(0, 13),
  risk.table.height = 0.22,
  ggtheme = km_theme
)

p_lancet_bin$plot <- p_lancet_bin$plot +
  scale_x_continuous(breaks = x_breaks, limits = c(0, 13)) +
  scale_y_continuous(
    labels = label_percent(accuracy = 0.1),
    limits = c(0, 0.17),
    breaks = seq(0, 0.17, 0.03)
  )
  theme(legend.position = "top")

p_lancet_bin$table <- p_lancet_bin$table +
  scale_x_continuous(breaks = x_breaks, limits = c(0, 13)) +
  risk_theme

############################################################
### 5) KM 3 — LANCET STAGING
############################################################

fit_lancet_stage <- survfit(
  Surv(followup_cancer, cancer_event) ~ lancet_stage,
  data = df_cancer
)

p_lancet_stage <- ggsurvplot(
  fit_lancet_stage,
  data = df_cancer,
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
  ylab = "Cumulative incidence of solid cancer (%)",
  break.time.by = 2,
  xlim = c(0, 13),
  risk.table.height = 0.22,
  ggtheme = km_theme
)

p_lancet_stage$plot <- p_lancet_stage$plot +
  scale_x_continuous(breaks = x_breaks, limits = c(0, 13)) +
  scale_y_continuous(
    labels = label_percent(accuracy = 0.1),
    limits = c(0, 0.17),
    breaks = seq(0, 0.17, 0.03)
  )
  theme(legend.position = "top")

p_lancet_stage$table <- p_lancet_stage$table +
  scale_x_continuous(breaks = x_breaks, limits = c(0, 13)) +
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
  filename = "C:/Users/nicol/Desktop/KM_cancer_BMI_binary_full.png",
  plot = bmi_full,
  width = 10,
  height = 8,
  dpi = 400
)

ggsave(
  filename = "C:/Users/nicol/Desktop/KM_cancer_Lancet_binary_full.png",
  plot = lancet_bin_full,
  width = 10,
  height = 8,
  dpi = 400
)

ggsave(
  filename = "C:/Users/nicol/Desktop/KM_cancer_Lancet_stage_full.png",
  plot = lancet_stage_full,
  width = 10,
  height = 8,
  dpi = 400
)

############################################################
### 8) OPTIONAL: SAVE MAIN PLOTS ONLY
############################################################

ggsave(
  filename = "C:/Users/nicol/Desktop/KM_cancer_BMI_binary_plot_only.png",
  plot = p_bmi$plot,
  width = 8,
  height = 6,
  dpi = 400
)

ggsave(
  filename = "C:/Users/nicol/Desktop/KM_cancer_Lancet_binary_plot_only.png",
  plot = p_lancet_bin$plot,
  width = 8,
  height = 6,
  dpi = 400
)

ggsave(
  filename = "C:/Users/nicol/Desktop/KM_cancer_Lancet_stage_plot_only.png",
  plot = p_lancet_stage$plot,
  width = 8,
  height = 6,
  dpi = 400
)

############################################################
### 1) CANCER obesity-related
############################################################

df_40006 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_40006.csv",
  stringsAsFactors = FALSE,
  colClasses = "character",
  check.names = FALSE
)

df_40005 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_40005.csv",
  stringsAsFactors = FALSE,
  colClasses = "character",
  check.names = FALSE
)

df_40006 <- df_40006 %>%
  mutate(eid = as.integer(eid))

df_40005 <- df_40005 %>%
  mutate(eid = as.integer(eid))

cancer_icd_cols  <- grep("^p40006", names(df_40006), value = TRUE)
cancer_date_cols <- grep("^p40005", names(df_40005), value = TRUE)

cancer_icd_long <- df_40006 %>%
  pivot_longer(
    cols = all_of(cancer_icd_cols),
    names_to = "slot",
    values_to = "icd_code"
  ) %>%
  mutate(
    slot_id = str_extract(slot, "[0-9]+$"),
    icd_code = toupper(trimws(icd_code)),
    icd_code = gsub("\\.", "", icd_code),
    icd_code = na_if(icd_code, "")
  ) %>%
  filter(!is.na(icd_code)) %>%
  select(eid, slot_id, icd_code)

cancer_date_long <- df_40005 %>%
  pivot_longer(
    cols = all_of(cancer_date_cols),
    names_to = "slot",
    values_to = "dx_date_raw"
  ) %>%
  mutate(
    slot_id = str_extract(slot, "[0-9]+$"),
    dx_date_raw = trimws(dx_date_raw),
    dx_date_raw = na_if(dx_date_raw, ""),
    dx_date = as.Date(parse_date_time(dx_date_raw, orders = c("ymd", "dmy", "mdy")))
  ) %>%
  filter(!is.na(dx_date)) %>%
  select(eid, slot_id, dx_date)

cancer_long_matched <- cancer_icd_long %>%
  inner_join(cancer_date_long, by = c("eid", "slot_id"))

# Breast: C50
# Ovary: C56
# Endometrium/uterus: C54, C55
# Stomach: C16
# Esophagus: C15
# Colon: C18

obesity_related_cancer_long <- cancer_long_matched %>%
  mutate(
    icd_code = gsub("\\.", "", toupper(trimws(icd_code)))
  ) %>%
  filter(
    grepl("^(C15|C16|C18|C50|C54|C55|C56)", icd_code)
  )

table(substr(obesity_related_cancer_long$icd_code, 1, 3))

df_obcancer <- cohort %>%
  mutate(
    baseline_date = as.Date(baseline_date),
    death_date = as.Date(death_date),
    censor_date = as.Date("2022-06-01"),
    
    bmi_binary = factor(
      ifelse(bmi >= 30, "Obesity", "No obesity"),
      levels = c("No obesity", "Obesity")
    ),
    
    lancet_binary = factor(
      ifelse(obesity_lancet == 1, "Obesity", "No obesity"),
      levels = c("No obesity", "Obesity")
    ),
    
    lancet_stage = factor(
      lancet_obesity_stage_full,
      levels = c("No obesity", "Preclinical obesity", "Clinical obesity")
    ),
    
    smoking = as.factor(smoking),
    sex = as.factor(sex)
  ) %>%
  left_join(
    obesity_related_cancer_long %>%
      inner_join(
        cohort %>% select(eid, baseline_date),
        by = "eid"
      ) %>%
      filter(dx_date > baseline_date) %>%
      group_by(eid) %>%
      summarise(
        obesity_related_cancer_date = min(dx_date, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "eid"
  ) %>%
  mutate(
    end_date = pmin(
      obesity_related_cancer_date,
      death_date,
      censor_date,
      na.rm = TRUE
    ),
    obesity_related_cancer_event = as.integer(
      !is.na(obesity_related_cancer_date) &
        obesity_related_cancer_date <= end_date
    ),
    followup_obesity_related_cancer =
      as.numeric(end_date - baseline_date) / 365.25
  )

summary(df_obcancer$followup_obesity_related_cancer)
table(df_obcancer$obesity_related_cancer_event, useNA = "ifany")
table(df_obcancer$bmi_binary, df_obcancer$obesity_related_cancer_event)
table(df_obcancer$lancet_binary, df_obcancer$obesity_related_cancer_event)
table(df_obcancer$lancet_stage, df_obcancer$obesity_related_cancer_event)

df_obcancer_adj <- df_obcancer %>%
  filter(
    !is.na(age),
    !is.na(sex),
    !is.na(smoking),
    !is.na(alcohol_g_day),
    !is.na(followup_obesity_related_cancer),
    !is.na(obesity_related_cancer_event),
    !is.na(bmi_binary),
    !is.na(lancet_binary),
    !is.na(lancet_stage)
  )

fmt_p <- function(p){
  if(is.na(p)) return("")
  if(p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

fmt_hr <- function(est, low, high){
  sprintf("%.2f (%.2f–%.2f)", est, low, high)
}

make_section_title <- function(txt, ncol = 7){
  x <- as.list(rep("", ncol))
  names(x) <- c(
    "Obesity Definition",
    "Events, n (%)",
    "Follow-up, years (median [IQR])",
    "Unadjusted HR (95% CI)",
    "p",
    "Adjusted HR (95% CI)*",
    "p "
  )
  x$`Obesity Definition` <- txt
  as.data.frame(x, check.names = FALSE)
}

build_binary_table <- function(data_desc, data_adj, group_var, group_label, time_var, event_var){
  tab <- data_desc %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      N = n(),
      Events = sum(.data[[event_var]], na.rm = TRUE),
      pct = round(100 * Events / N, 1),
      med_fu = median(.data[[time_var]], na.rm = TRUE),
      q1_fu = quantile(.data[[time_var]], 0.25, na.rm = TRUE),
      q3_fu = quantile(.data[[time_var]], 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  f_unadj <- as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~ ", group_var))
  f_adj   <- as.formula(paste0(
    "Surv(", time_var, ", ", event_var, ") ~ ", group_var,
    " + age + sex + smoking + alcohol_g_day"
  ))
  
  cox_unadj <- coxph(f_unadj, data = data_desc)
  cox_adj   <- coxph(f_adj, data = data_adj)
  
  hr_unadj <- tidy(cox_unadj, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)")
  hr_adj <- tidy(cox_adj, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)")
  
  out <- tab %>%
    mutate(
      `Events, n (%)` = paste0(format(Events, big.mark = ","), " (", pct, "%)"),
      `Follow-up, years (median [IQR])` =
        sprintf("%.2f [%.2f–%.2f]", med_fu, q1_fu, q3_fu)
    ) %>%
    transmute(
      `Obesity Definition` = as.character(.data[[group_var]]),
      `Events, n (%)`,
      `Follow-up, years (median [IQR])`
    )
  
  out$`Unadjusted HR (95% CI)` <- c(
    "Ref",
    fmt_hr(hr_unadj$estimate[1], hr_unadj$conf.low[1], hr_unadj$conf.high[1])
  )
  out$`p` <- c("", fmt_p(hr_unadj$p.value[1]))
  
  out$`Adjusted HR (95% CI)*` <- c(
    "Ref",
    fmt_hr(hr_adj$estimate[1], hr_adj$conf.low[1], hr_adj$conf.high[1])
  )
  out$`p ` <- c("", fmt_p(hr_adj$p.value[1]))
  
  bind_rows(make_section_title(group_label), out)
}

build_staging_table <- function(data_desc, data_adj, group_var, group_label, time_var, event_var){
  tab <- data_desc %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      N = n(),
      Events = sum(.data[[event_var]], na.rm = TRUE),
      pct = round(100 * Events / N, 1),
      med_fu = median(.data[[time_var]], na.rm = TRUE),
      q1_fu = quantile(.data[[time_var]], 0.25, na.rm = TRUE),
      q3_fu = quantile(.data[[time_var]], 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  f_unadj <- as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~ ", group_var))
  f_adj   <- as.formula(paste0(
    "Surv(", time_var, ", ", event_var, ") ~ ", group_var,
    " + age + sex + smoking + alcohol_g_day"
  ))
  
  cox_unadj <- coxph(f_unadj, data = data_desc)
  cox_adj   <- coxph(f_adj, data = data_adj)
  
  hr_unadj <- tidy(cox_unadj, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(grepl(group_var, term))
  hr_adj <- tidy(cox_adj, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(grepl(group_var, term))
  
  out <- tab %>%
    mutate(
      `Events, n (%)` = paste0(format(Events, big.mark = ","), " (", pct, "%)"),
      `Follow-up, years (median [IQR])` =
        sprintf("%.2f [%.2f–%.2f]", med_fu, q1_fu, q3_fu)
    ) %>%
    transmute(
      `Obesity Definition` = as.character(.data[[group_var]]),
      `Events, n (%)`,
      `Follow-up, years (median [IQR])`
    )
  
  out$`Unadjusted HR (95% CI)` <- c(
    "Ref",
    fmt_hr(hr_unadj$estimate[1], hr_unadj$conf.low[1], hr_unadj$conf.high[1]),
    fmt_hr(hr_unadj$estimate[2], hr_unadj$conf.low[2], hr_unadj$conf.high[2])
  )
  out$`p` <- c(
    "",
    fmt_p(hr_unadj$p.value[1]),
    fmt_p(hr_unadj$p.value[2])
  )
  
  out$`Adjusted HR (95% CI)*` <- c(
    "Ref",
    fmt_hr(hr_adj$estimate[1], hr_adj$conf.low[1], hr_adj$conf.high[1]),
    fmt_hr(hr_adj$estimate[2], hr_adj$conf.low[2], hr_adj$conf.high[2])
  )
  out$`p ` <- c(
    "",
    fmt_p(hr_adj$p.value[1]),
    fmt_p(hr_adj$p.value[2])
  )
  
  bind_rows(make_section_title(group_label), out)
}

table_obesity_related_cancer <- bind_rows(
  build_binary_table(
    data_desc = df_obcancer,
    data_adj  = df_obcancer_adj,
    group_var = "bmi_binary",
    group_label = "Model 1. BMI-based obesity",
    time_var = "followup_obesity_related_cancer",
    event_var = "obesity_related_cancer_event"
  ),
  build_binary_table(
    data_desc = df_obcancer,
    data_adj  = df_obcancer_adj,
    group_var = "lancet_binary",
    group_label = "Model 2. Lancet criteria-based obesity",
    time_var = "followup_obesity_related_cancer",
    event_var = "obesity_related_cancer_event"
  ),
  build_staging_table(
    data_desc = df_obcancer,
    data_adj  = df_obcancer_adj,
    group_var = "lancet_stage",
    group_label = "Model 3. Lancet obesity staging",
    time_var = "followup_obesity_related_cancer",
    event_var = "obesity_related_cancer_event"
  )
)

table_obesity_related_cancer

ft <- flextable(table_obesity_related_cancer) %>%
  autofit() %>%
  align(align = "left", j = 1, part = "all") %>%
  align(align = "center", j = 2:7, part = "all") %>%
  bold(
    i = c(
      which(table_obesity_related_cancer$`Obesity Definition` == "Model 1. BMI-based obesity"),
      which(table_obesity_related_cancer$`Obesity Definition` == "Model 2. Lancet criteria-based obesity"),
      which(table_obesity_related_cancer$`Obesity Definition` == "Model 3. Lancet obesity staging")
    ),
    bold = TRUE,
    part = "body"
  )

doc <- read_docx() %>%
  body_add_par("Incident obesity-related cancers according to BMI-based and Lancet-defined obesity", style = "heading 1") %>%
  body_add_flextable(ft) %>%
  body_add_par("", style = "Normal") %>%
  body_add_par(
    "Obesity-related cancers included breast (C50), ovary (C56), endometrium/uterus (C54-C55), stomach (C16), esophagus (C15), and colon (C18).",
    style = "Normal"
  ) %>%
  body_add_par(
    "*Adjusted for age, sex, smoking status, and alcohol intake.",
    style = "Normal"
  )

print(
  doc,
  target = "C:/Users/nicol/Desktop/Incident_obesity_related_cancers_all_models.docx"
)
