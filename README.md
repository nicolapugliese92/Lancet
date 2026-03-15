library(dplyr)
library(broom)
library(ggplot2)
library(patchwork)

############################################################
### 1) REFIT MODELS
############################################################

df_liver_adj_forest <- df_liver %>%
  filter(
    !is.na(age),
    !is.na(sex),
    !is.na(smoking),
    !is.na(alcohol_g_day),
    !is.na(followup_liver),
    !is.na(event),
    !is.na(bmi_binary),
    !is.na(lancet_binary),
    !is.na(lancet_stage)
  )

cox_bmi_unadj_fp <- coxph(
  Surv(followup_liver, event) ~ bmi_binary,
  data = df_liver
)

cox_bmi_adj_fp <- coxph(
  Surv(followup_liver, event) ~
    bmi_binary + age + sex + smoking + alcohol_g_day,
  data = df_liver_adj_forest
)

cox_lancet_bin_unadj_fp <- coxph(
  Surv(followup_liver, event) ~ lancet_binary,
  data = df_liver
)

cox_lancet_bin_adj_fp <- coxph(
  Surv(followup_liver, event) ~
    lancet_binary + age + sex + smoking + alcohol_g_day,
  data = df_liver_adj_forest
)

cox_lancet_stage_unadj_fp <- coxph(
  Surv(followup_liver, event) ~ lancet_stage,
  data = df_liver
)

cox_lancet_stage_adj_fp <- coxph(
  Surv(followup_liver, event) ~
    lancet_stage + age + sex + smoking + alcohol_g_day,
  data = df_liver_adj_forest
)

############################################################
### 2) HELPER
############################################################

fmt_hr <- function(est, low, high){
  sprintf("%.2f (%.2f–%.2f)", est, low, high)
}

############################################################
### 3) EXTRACT HR
############################################################

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

############################################################
### 4) FOREST DATAFRAME
############################################################

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
  left_join(bmi_unadj, by=c("definition","category")) %>%
  left_join(bmi_adj, by=c("definition","category")) %>%
  rows_update(lancet_bin_unadj, by=c("definition","category")) %>%
  rows_update(lancet_bin_adj, by=c("definition","category")) %>%
  rows_update(lancet_stage_unadj, by=c("definition","category")) %>%
  rows_update(lancet_stage_adj, by=c("definition","category"))

############################################################
### 5) LEFT PANEL (BIGGER TEXT)
############################################################

left_df <- forest_df %>%
  mutate(
    def_display = ifelse(category=="No obesity",definition,""),
    cat_display = category,
    hr_unadj_display = ifelse(category=="No obesity","Reference",
                             fmt_hr(est_unadj,low_unadj,high_unadj)),
    hr_adj_display = ifelse(category=="No obesity","Reference",
                           fmt_hr(est_adj,low_adj,high_adj))
  )

p_left <- ggplot(left_df, aes(y=y)) +
  xlim(0,1) +
  ylim(0.7,6.2) +
  theme_void() +

  annotate("text",x=0.00,y=6.05,label="Definition",
           hjust=0,fontface="bold",size=4.2) +
  annotate("text",x=0.33,y=6.05,label="Category",
           hjust=0,fontface="bold",size=4.2) +
  annotate("text",x=0.58,y=6.05,label="Unadjusted HR (95% CI)",
           hjust=0,fontface="bold",size=4.2) +
  annotate("text",x=0.83,y=6.05,label="Adjusted HR (95% CI)",
           hjust=0,fontface="bold",size=4.2) +

  geom_text(aes(x=0.00,label=def_display),
            hjust=0,size=4,fontface="bold") +
  geom_text(aes(x=0.33,label=cat_display),
            hjust=0,size=4) +
  geom_text(aes(x=0.58,label=hr_unadj_display),
            hjust=0,size=4) +
  geom_text(aes(x=0.83,label=hr_adj_display),
            hjust=0,size=4)

############################################################
### 6) RIGHT PANEL
############################################################

p_right <- ggplot(forest_df,aes(y=y)) +

  geom_vline(xintercept=1,linetype="dashed",
             colour="grey40",linewidth=0.7) +

  geom_errorbarh(
    data=forest_df %>% filter(!is.na(est_unadj)),
    aes(xmin=low_unadj,xmax=high_unadj,color="Unadjusted"),
    height=0.08,linewidth=1.1,
    position=position_nudge(y=0.10)
  ) +
  geom_point(
    data=forest_df %>% filter(!is.na(est_unadj)),
    aes(x=est_unadj,color="Unadjusted"),
    size=3.8,
    position=position_nudge(y=0.10)
  ) +

  geom_errorbarh(
    data=forest_df %>% filter(!is.na(est_adj)),
    aes(xmin=low_adj,xmax=high_adj,color="Adjusted"),
    height=0.08,linewidth=1.1,
    position=position_nudge(y=-0.10)
  ) +
  geom_point(
    data=forest_df %>% filter(!is.na(est_adj)),
    aes(x=est_adj,color="Adjusted"),
    size=3.8,
    position=position_nudge(y=-0.10)
  ) +

  scale_color_manual(
    values=c("Unadjusted"="grey45","Adjusted"="#D18F00")
  ) +

  scale_x_continuous(
    trans="log10",
    breaks=c(0.5,1,2,3),
    limits=c(0.5,3),
    labels=c("0.5","1","2","3")
  ) +

  scale_y_continuous(
    breaks=forest_df$y,
    labels=rep("",nrow(forest_df)),
    limits=c(0.7,6.2)
  ) +

  labs(x="Hazard ratio (95% CI), log scale",
       y=NULL,color=NULL) +

  theme_classic(base_size=12) +

  theme(
    legend.position=c(0.82,0.90),
    legend.background=element_rect(color="grey70",fill="white"),
    legend.text=element_text(size=12),
    legend.key.size=unit(1.3,"lines"),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )

############################################################
### 7) FINAL FIGURE
############################################################

p_final <- p_left + p_right +
  plot_layout(widths=c(2.8,1.35))

ggsave(
  "forest_plot_liver_models_final.png",
  p_final,
  width=14,
  height=4.5,
  dpi=400
)

p_final
