############################################################
### 1) BUILD COMPOSITE LIVER OUTCOME DATASET
############################################################

df_liver <- cohort %>%
  mutate(
    baseline_date = as.Date(baseline_date),
    liver_event_date = as.Date(liver_related_event_date_postbaseline),
    death_date = as.Date(death_date),
    censor_date = as.Date("2023-03-31"),
    
    liver_event_post = as.integer(liver_related_event_postbaseline),
    liver_death_post = as.integer(death_liver_postbaseline),
    
    event = as.integer(
      liver_event_post == 1 | liver_death_post == 1
    ),
    
    date_liver_event = if_else(
      liver_event_post == 1 & !is.na(liver_event_date),
      liver_event_date,
      as.Date(NA)
    ),
    
    date_liver_death = if_else(
      liver_death_post == 1 & !is.na(death_date),
      death_date,
      as.Date(NA)
    ),
    
    end_date_raw = case_when(
      !is.na(date_liver_event) & !is.na(date_liver_death) ~ pmin(date_liver_event, date_liver_death),
      !is.na(date_liver_event) ~ date_liver_event,
      !is.na(date_liver_death) ~ date_liver_death,
      TRUE ~ censor_date
    ),
    
    end_date = pmin(end_date_raw, censor_date),
    followup_liver = as.numeric(end_date - baseline_date) / 365.25,
    
    # Model 1: BMI >30 vs <30
    bmi_binary = factor(
      ifelse(bmi >= 30, "Obesity", "Non-obesity"),
      levels = c("Non-obesity", "Obesity")
    ),
    
    # Model 2: Lancet obesity yes/no
    lancet_binary = factor(
      ifelse(obesity_lancet == 1, "Obesity", "Non-obesity"),
      levels = c("Non-obesity", "Obesity")
    ),
    
    # Model 3: Lancet staging
    lancet_stage = factor(
      lancet_obesity_stage_full,
      levels = c("No obesity", "Preclinical obesity", "Clinical obesity")
    ),
    
    smoking = as.factor(smoking),
    sex = as.factor(sex)
  )

############################################################
### 2) DATASET FOR ADJUSTED MODELS
############################################################

df_liver_adj <- df_liver %>%
  filter(
    !is.na(age),
    !is.na(sex),
    !is.na(smoking),
    !is.na(alcohol_g_day),
    !is.na(LDL_mmol_L),
    !is.na(followup_liver),
    !is.na(event),
    !is.na(bmi_binary),
    !is.na(lancet_binary),
    !is.na(lancet_stage)
  )

############################################################
### 3) HELPER FUNCTIONS
############################################################

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
    "Definition",
    "Events, n (%)",
    "Follow-up, years (median [IQR])",
    "Unadjusted HR (95% CI)",
    "p",
    "Adjusted HR (95% CI)*",
    "p "
  )
  x$Definition <- txt
  as.data.frame(x, check.names = FALSE)
}

build_binary_table <- function(data_desc, data_adj, group_var, group_label){
  tab <- data_desc %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      N = n(),
      Events = sum(event, na.rm = TRUE),
      pct = round(100 * Events / N, 1),
      med_fu = median(followup_liver, na.rm = TRUE),
      q1_fu = quantile(followup_liver, 0.25, na.rm = TRUE),
      q3_fu = quantile(followup_liver, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  f_unadj <- as.formula(paste0("Surv(followup_liver, event) ~ ", group_var))
  f_adj   <- as.formula(paste0(
    "Surv(followup_liver, event) ~ ", group_var,
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
      Definition = as.character(.data[[group_var]]),
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

build_staging_table <- function(data_desc, data_adj, group_var, group_label){
  tab <- data_desc %>%
    group_by(.data[[group_var]]) %>%
    summarise(
      N = n(),
      Events = sum(event, na.rm = TRUE),
      pct = round(100 * Events / N, 1),
      med_fu = median(followup_liver, na.rm = TRUE),
      q1_fu = quantile(followup_liver, 0.25, na.rm = TRUE),
      q3_fu = quantile(followup_liver, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  f_unadj <- as.formula(paste0("Surv(followup_liver, event) ~ ", group_var))
  f_adj   <- as.formula(paste0(
    "Surv(followup_liver, event) ~ ", group_var,
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
      Definition = as.character(.data[[group_var]]),
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

############################################################
### 4) BUILD ONE COMBINED TABLE
############################################################

table_all <- bind_rows(
  build_binary_table(
    data_desc = df_liver,
    data_adj  = df_liver_adj,
    group_var = "bmi_binary",
    group_label = "Model 1. BMI-based obesity"
  ),
  build_binary_table(
    data_desc = df_liver,
    data_adj  = df_liver_adj,
    group_var = "lancet_binary",
    group_label = "Model 2. Lancet criteria-based obesity"
  ),
  build_staging_table(
    data_desc = df_liver,
    data_adj  = df_liver_adj,
    group_var = "lancet_stage",
    group_label = "Model 3. Lancet obesity staging"
  )
)

table_all

############################################################
### 5) EXPORT TO WORD
############################################################

ft <- flextable(table_all) %>%
  autofit() %>%
  align(align = "left", j = 1, part = "all") %>%
  align(align = "center", j = 2:7, part = "all") %>%
  bold(i = c(
    which(table_all$Definition == "Model 1. BMI-based obesity"),
    which(table_all$Definition == "Model 2. Lancet criteria-based obesity"),
    which(table_all$Definition == "Model 3. Lancet obesity staging")
  ), bold = TRUE, part = "body")

doc <- read_docx() %>%
  body_add_par("Composite liver outcome according to BMI-based and Lancet-defined obesity", style = "heading 1") %>%
  body_add_flextable(ft) %>%
  body_add_par("", style = "Normal") %>%
  body_add_par(
    "Composite liver outcome included incident liver-related events or liver-related death.",
    style = "Normal"
  ) %>%
  body_add_par(
    "*Adjusted for age, sex, smoking status, alcohol intake.",
    style = "Normal"
  )

print(
  doc,
  target = "C:/Users/nicol/Desktop/Composite_liver_outcome_all_models.docx"
)



